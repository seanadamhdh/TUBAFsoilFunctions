





#' @title get best model for variable based on metric
#' @description From a folder containing model object created with scripts used in the BDF-SSL framework, select the best model candidate based on a evaluation metric for a given variable.
#' @note Not yet generalised
#' @param model_eval model evaluation object created with evaluate_model_batch()
#' @param prefix Model names prefix, usually defines model type
#' @param variable Variable for which the best model is to be searched
#' @param metric evaluation statistic which is used to select best model
#' @param maximise Should the model with the highest metric be selected? E.g., for R2 set to True. Default is False.
#' @import tidyverse
#' @export
get_best=function(model_eval,
                  prefix="cubist_",
                  variable_="CORG",
                  metric="test.rmse",
                  maximise=F){
  if(!maximise){
    out=model_eval%>%filter(variable==variable_)%>%slice(which.min(.[[metric]]))%>%
      transmute(out=paste0(prefix,set,"-",trans,"-",variable))%>%pull(out)
  }else{
    out=model_eval%>%filter(variable==variable_)%>%slice(which.max(.[[metric]]))%>%
      transmute(out=paste0(prefix,set,"-",trans,"-",variable))%>%pull(out)
  }
  return(out)
}

#' @title predict variable (BDF-SSL framework)
#' @description
#' Selection of best model for agiven variable based on evaluation metric from a set of models and prediction for target spectra
#' @param taget_data Nested tibble with spc data at different pre-processing steps
#' @param prefix Model names prefix, usually defines model type
#' @param variable Variable for which the best model is to be searched
#' @param metric evaluation statistic which is used to select best model
#' @param maximise Should the model with the highest metric be selected? E.g., for R2 set to True. Default is False.
#' @param restrict_spc If True only regard models for which spc preprocessing sets are available. If False, search in all models for best candidate and
#' return ERROR when spc set for best candidate is not available
#' @import tidyverse
#' @import Cubist
#' @import resemble
#' @import caret
#' @import pls
#' @export
predict_variable=function(
    target_data,
    model_folder,
    eval_model_folder=model_folder, # mbl needs different dir
    prefix="cubist_",
    variable_="CORG",
    metric="test.rmse",
    maximise=F,
    restrict=T,
    diss_limit=2.5,
    manual=NA
){
  if(is.na(manual)){

    if(!str_ends(model_folder,"/")){
      model_folder=paste0(model_folder,"/")
    }

    # check spc set availability
    spc_set_target=(str_split_fixed(
      str_split_fixed(
        list.files(model_folder,pattern = prefix),
        pattern ="_",n=2)[,2],
      pattern="-",n=2)[,1])%>%unique


    # mahD check
    #... either here, or after model candidate selection



    #read model evaluation
    print(eval_model_folder) # debug
    evaluation=read_rds(paste0(eval_model_folder,"evaluation"))

    ## get model candidate ####
    if(restrict==T){ #choose only from avail spc sets
      avail_spc=unique(evaluation$eval$set)[which(unique(evaluation$eval$set)%in%spc_set_target)]
      evaluation_data=filter(evaluation$eval,set%in%avail_spc)

      # get best candidate
      best_model=get_best(
        model_eval=evaluation_data,
        prefix = prefix,
        variable_=variable_,
        metric = metric,
        maximise = maximise)
    }else{
      evaluation_data=evaluation$eval
      # get best candidate
      best_model=get_best(
        model_eval=evaluation_data,
        prefix = prefix,
        variable_=variable_,
        metric = metric,
        maximise = maximise)
      # in case not avail, return empty
      if(!best_model$documentation$spc_set%in%spc_set_target){
        print("Spc preprocessing of the best model candidate is not available in the target dataset.
              Check your target dataset or consider using restrict=T to limit model selection to avaialable spc-pretratments.")
        return(NULL)
      }

    }
  }else{
    best_model=manual
  }
  print(best_model)

  ### load model candidate
  model=read_rds(paste0(model_folder,best_model))




  ### compare reference and target spc (similarity metrics)

  target_spc=target_data[[model$documentation$spc_set]]




  # calculate pls scores and mahD
  diss_data=dissimilarity(
    Xr = model$documentation$train_data[[2]],
    Yr = model$documentation$train_data[[1]],
    Xu = target_spc,
    diss_method = "pca.nipals",
    return_projection = TRUE
  )


  spc_diss=bind_cols(sample_id=target_data$sample_id,
                     mean_diss=colMeans(diss_data$dissimilarity))
  spc_diss=mutate(spc_diss,
                  diss_flag=if_else(mean_diss>diss_limit,"outlier_spc","ok")
  )

  # for debug
  cat("Target spectra above dissimilarity limit of ",
      diss_limit,"\n",length(na.omit(spc_diss$diss_flag)),"\n\n")

  ### plot ####

  # reference projection
  (diss_data$projection$scores %>% as_tibble())[c((1):(nrow(model$documentation$train_data[2]))),] %>%
    ggplot(aes(x = pc_1, y = pc_2)) +
    geom_point(color="black") +
    # target projection
    geom_point(
      data = (diss_data$projection$scores %>%

                as_tibble())[c((nrow(model$documentation$train_data[2]) + 1):(nrow(model$documentation$train_data[2]) + nrow(target_spc))), ]%>%
        as_tibble%>%
        bind_cols(mean_mahD=colMeans(diss_data$dissimilarity),outlier=if_else(colMeans(diss_data$dissimilarity) > diss_limit, "outlier", "ok")),
      aes(
        col = mean_mahD,
        shape = outlier,
        size = outlier
      )
    ) + theme_pubr() +
    scale_color_steps(low = "blue", high = "red") +
    scale_shape_manual(breaks = c("outlier", "ok"), values = c(4, 16)) +
    scale_size_manual(breaks = c("outlier", "ok"), values = c(3, 1))->plt

  # de-transformation for log1p models
  print("predicting...")

  if(any(class(model)=="train")){ #e.g. for pls, cubist, svm... everything caret should work
    if(str_detect(best_model,"log1p")){
      print("De-logging predictions...")
      predictions=bind_cols(sample_id=target_data$sample_id,pred=exp(predict(model,target_spc))-1)
    }else{
      predictions=bind_cols(sample_id=target_data$sample_id,pred=predict(model,target_spc))
    }
  }else if(any(class(model)=="mbl")){ # mbl
    # get best k
    best_k_pos<-which.min(model$validation_results$local_cross_validation$rmse)

    sample_id=left_join(tibble(x=as.matrix(model$documentation$test_data)),
                        target_data,
                        by=c("x"=model$documentation$spc_set))%>%pull(sample_id)


    # pull predictions
    if(str_detect(best_model,"log1p")){
      print("De-logging predictions...")
      predictions=bind_cols(sample_id=sample_id,pred=exp(model$results[[best_k_pos]]$pred%>%c)-1)
    }else{
      predictions=bind_cols(sample_id=sample_id,pred=model$results[[best_k_pos]]$pred%>%c)
    }

  }else{
    print("ERROR Model type not recognised")
    return()
  }



  predictions=list(
    plt=plt,
    model=model,
    model_name=best_model,
    predictions=predictions,
    spc_diss=spc_diss,
    diss_data=diss_data)
  return(predictions)
}









# model evaluation aggregation ####

#' @title model evaluation (BDF-SSL)
#' @description Batch model runs evaluation aggregation
#' Currently only supports testset data (no gt300)
#' @note evaluate_model_adjusted source path is legacy. Change in future
#' @param root_dir Root directory to Project
#' @param model_folder Folder containing model objects
#' @param model_type_pattern prefix of model object names
#' @param new_eval recalculate evaluation stats with evaluate_model_adjusted.R (orginal statistics will still be saved seperately)
#' @param verbose_iter Print progress updates
#' @param path_evaluate_model_adjusted Internal argument for sourcing required functions
#' @import tidyverse
#' @import progress
#' @export
evaluate_model_batch=function(root_dir="C:/Users/adam/Documents",
                              model_folder="/GitHub/R_main/models/2024 models/PLS_75_25_var_dependent_preProcess",
                              model_type_pattern="pls_",
                              new_eval=F,
                              verbose_iter=T,
                              path_evaluate_model_adjusted="/GitHub/BDF/BDF-SSL/3_r_scripts/evaluate_model_adjusted.R"){
  plotlist<-list()
  model_eval_table<-c()
  if(new_eval){
    new_eval_table<-c()
  }
  Obs_Pred_data<-c()



  # progressbar
  progress::progress_bar$new(
    total = list.files(paste0(root_dir,model_folder),full.names = T,
                       pattern = model_type_pattern)%>%length,
    incomplete = "-",
    complete = "#",
    current = ">",
    format = ":bar | :percent completed ETA: :eta"
  )->pb
  # loop for all models in dir
  for (i in list.files(paste0(root_dir,model_folder),full.names = T,
                       pattern = model_type_pattern) # small "c" cubist is used as pattern for model class objects in /temp -> results etc should be named Cubist with capital "C"
  ){

    model<-read_rds(i)
    model_name<-basename(i)

    # calculating obs / pred based on transformation and spc-set. Un-transforms log(1+p) data


    train_Obs<-model$documentation$train_data[[1]]  #maybe hardcode with var name
    test_ObsPred<-model$documentation$ObsPred_test

    set<-model$documentation$spc_set



    # currently inactive: selection of spc-set based on namestring
    #    if (model_name%>%str_detect("spc-")){
    #      if(model$documentation$trans=="none"){
    #        test_ObsPred<-data.frame(pred=predict(model,model$testingData$spc),obs=model$testingData[[1]])
    #      }else if (model$documentation$trans=="log1p"){
    #        test_ObsPred<-data.frame(pred=exp(predict(model,model$testingData$spc))-1,obs=exp(model$testingData[[1]])-1)
    #      }
    #      set<-"spc"
    #    }else if (model_name%>%str_detect("spc_sg-")){
    #      if(model$documentation$trans=="none"){
    #        test_ObsPred<-data.frame(pred=predict(model,model$testingData$spc_sg11),obs=model$testingData[[1]])
    #      }else if (model$documentation$tr=="log1p"){
    #        test_ObsPred<-data.frame(pred=exp(predict(model,model$testingData$spc_sg11))-1,obs=exp(model$testingData[[1]])-1)
    #      }
    #      set<-"spc_sg"
    #    }else if (model_name%>%str_detect("spc_sg11_snv_rs4-")){
    #      if(model$documentation$trans=="none"){
    #        test_ObsPred<-data.frame(pred=predict(model,model$testingData$spc_sg11_snv),obs=model$testingData[[1]])
    #      }else if (model$documentation$trans=="log1p"){
    #        test_ObsPred<-data.frame(pred=exp(predict(model,model$testingData$spc_sg11_snv))-1,obs=exp(model$testingData[[1]])-1)
    #      }
    #      set<-"spc_sg_snv_rs4"
    #    }


    #plotting
    ggplot(test_ObsPred,aes(x=obs,y=pred))+
      geom_point()+

      geom_abline(intercept=0,slope=1,linetype="dotted")+
      ggtitle(model_name)+
      theme_pubr()->plotlist[[model_name]]


    #calculating stats
    model_eval_table<-bind_rows(model_eval_table,
                                data.frame(set=set,
                                           trans=model$documentation$transformation,
                                           variable=model$documentation$variable,
                                           test=model$documentation$evaluation_test
                                ))

    Obs_Pred_data[[model_name]][["train"]]=train_Obs
    Obs_Pred_data[[model_name]][["test"]]=test_ObsPred

    if(new_eval){
      source(paste0(root_dir,path_evaluate_model_adjusted))
      new_eval_table=rbind(new_eval_table,data.frame(set=set,
                                                     trans=model$documentation$transformation,
                                                     variable=model$documentation$variable,
                                                     evaluate_model_adjusted(test_ObsPred,obs="obs",pred="pred")
      )
      )
    }


    if(verbose_iter){pb$tick()}
  }

  # aggregating
  list(eval=model_eval_table,plots=plotlist,Obs_Pred_data=Obs_Pred_data,new_eval_table=new_eval_table)->test_evaluation

  # save results
  saveRDS(test_evaluation,paste0(root_dir,model_folder,"/evaluation"))
}

















