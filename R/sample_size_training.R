

######################################################################################################


#' @title val_train_size
#' @description
#' validation of model list output created by train_sample_size()
#'
#' @note install additional packages for specific model types manually
#' @param models Models list object created by train_sample_size.
#' @param trans_rev Optional function to reverse transformation. 'none' will do nothing.
#' If not specified, function will catch trans_rev from models$...$documentation$trans_rev
#'
#' @import progress
#' @import tidyverse
#'
#' @export
val_train_size=function(models,trans_rev=NULL){



  message("Validating with test set...")
  validation_stats=c()
  pb=progress::progress_bar$new(total=length(models))

  print(trans_rev)

  if(is.null(trans_rev)){
    trans_rev=models[[1]]$documentation$input$trans_rev
  }

  print(trans_rev)
  for (i in models){

    validation_stats=bind_rows(validation_stats,
                               tibble(
                                 size=i$documentation$input$size,
                                 # eval stats
                                 evaluate_model_adjusted(
                                   #apply reverse transformation
                                   match.fun(trans_rev)(
                                     data.frame(
                                       obs=i$documentation$test_set$Yr,
                                       pred=predict(
                                         i,
                                         i$documentation$test_set$Xr
                                       )
                                     )
                                   ),
                                   obs="obs",
                                   pred="pred"
                                 )
                               )
    )
    pb$tick()

  }
  return(validation_stats)
}


#' @title Sample size effect on model accuracy estimator
#'
#' @description Splits dataset into train/test set and runs multiple calibrations using increasing number of observartions from training data.
#' Model accuracy is calculated for each created model using the hold-out test set.
#' Can be used to assess effect of sample size on model performance. Should support most (numeric) regression models implemented in caret (no guarantees though - not tested)
#'
#' @note In future, extract kmeans sampling function as standalone
#'
#' @param data Dataset for model calibration. Can be tibble or dataframe. Must contain column Yr and column or nested tibble/data.frame/matrix Xr.
#' @param Xr="spc_sg_snv_rs4" Predictor variable(s). Vector column or nested tibble/data.frame/matrix.Will be converted into nested matrix.
#' @param Yr="TOC" Target variable. Currently only single column supported.
#' @param trans=none, Transformation applied to Yr by match.fun(trans)(Yr). Set to none for no operation.
#' @param trans_rev=none, Reverse operation to trans. Appplied by match.fun(trans_rev)(Yr). Set to none for no operation.
#' @param train_ratio 0.7, train / test split. 0...1 - fraction of data used for training
#' @param kmeans_pc=.99, cumvar of kmeans pc decomposition. Ignored when Xr<3. (kmeans applied directly on Xr)
#' @param min_samples lowest no of samples for training.
#' @param sample_step steps used to increment training sample size from min_samples to nrow(data)*train_ratio
#' @param method="glm" model type (handed to caret::train). Make sure to install/load required packages. (not yet automated)
#' @param trControl model fitting parameters handed to caret::train
#' @param tuneGrid=NULL tuning parameter grid handed to caret::train
#' @param save=F should individual models be saved?
#' @param output_folder folder path in which models will be saved if save = TRUE. If dir.exists(output_folder) = FALSE, folder will be created.
#' @param return_all=T return to console
#' @param seed=123 seed for initial train / test split for reproduceability
#' @param ... additional arguments passed to caret::train
#'
#' @returns If return_all = TRUE, returns object containing a list 'models' with all calibrated models (caret::train object with additional details appended to object$documentation)
#' and val_stats containing evaluation metrics. !!! CURRENTLY USES LOCAL FUNCTION from evaluate_model_adjusted.R
#'
#' @import tidyverse
#' @import caret
#' @import prospectr
#' @import progress
#'
#' @export
train_sample_size <- function(data,
                              Xr="spc_sg_snv_rs4",
                              Yr="TOC",
                              trans=none,
                              trans_rev=none,
                              train_ratio = 0.7,
                              kmeans_pc=.99,
                              min_samples= 30,
                              sample_step = 10,
                              method="glm",
                              trControl = trainControl(method = "cv", number = 10),
                              tuneGrid=NULL,
                              save_all=F,
                              save_models=F,
                              output_folder = "models_folder",
                              return_all=T,
                              seed=123,
                              ...) {
  message("Prepping data...")
  # Load necessary libraries
  library(tidyverse) # essentials
  library(caret) # training framework
  library(prospectr)# contains kmeans (naes) selection algorithm
  # Split dataset into train/test set
  set.seed(seed)  # For reproducibility

  # make sure var match name, only Xr,Yr, complete cases only
  data=(rename(data,Xr={{Xr}},Yr={{Yr}})%>%select(Xr,Yr)%>%na.omit)
  data=tibble(Yr=data$Yr,Xr=as.matrix(data$Xr))%>%na.omit

  # apply trans function if defined
  data$Yr=match.fun(trans)(data$Yr)



  #print("DEBUG 1")


  # Define the K-means sampling function !!! INTERNAL !!! -> if this causes issues make external function that is called here
  kmeans_sampling <- function(dataset, num_samples) {

    if (ncol(dataset$Xr)<3){
      kmeans_result <- naes(dataset$Xr, k=num_samples) # no pc decomposition for clustering
    }else{
      kmeans_result <- naes(dataset$Xr, k=num_samples, pc=kmeans_pc)
    }


    inSample <- kmeans_result$model
    return(inSample)
  }

  # initial split - holding test set back
  trainIndex <- kmeans_sampling(data,train_ratio*nrow(data))
  train_set <- data[trainIndex, ]
  test_set <- data[-trainIndex, ]

  #print("DEBUG 1.5")

  # Select sample sizes from n0 to n (with a step size defined by `sample_step`)
  sample_sizes <- seq(min(c(min_samples,nrow(train_set))), nrow(train_set), by = sample_step)



  #print("DEBUG 2")


  # Create directory to store models
  if ((save_models|save_all)&!dir.exists(output_folder)) {
    dir.create(output_folder)
  }

  # Train models for each sample size ####
  # ... for each value in sample_sizes loop
  pb=progress::progress_bar$new(total=length(sample_sizes))
  models=c()



  message("Building models...")
  for (size in sample_sizes){



    sampled_data <- data[kmeans_sampling(train_set, size),]



    #print(sampled_data$Yr)
    #print(sampled_data$Xr)
    #print(method)
    #print(tuneGrid)
    #print(trControl)

    # Train the model
    # note: built in preprocessing in train for Xr...
    model <- train(y=sampled_data$Yr,
                   x=as.matrix(sampled_data$Xr),
                   method=method,
                   tuneGrid = tuneGrid,
                   trControl = trControl,
                   ...)


    #print("DEBUG 3")

    # appending input data ... some might be redundant, but does not use too much space and "besser haben als brauchen"
    model$documentation[["input"]]=list("model_type"=method,
                                        "Xr" = Xr,
                                        "Yr" = Yr,
                                        "trans" = trans,
                                        "trans_rev" = trans_rev,
                                        "size" = size,
                                        "seed" = seed,
                                        "tune_grid" = tuneGrid,
                                        "train_ratio" = train_ratio,
                                        "kmeans_pc" = kmeans_pc,
                                        "min_samples" = min_samples,
                                        "sample_step" = sample_step,
                                        "train_control" = trControl,
                                        "split"=sampled_data)
    #including test set in model output for streamlined evaluation
    model$documentation[["test_set"]]=test_set



    #print("DEBUG 4")

    if(save_models){
      # Save the model as RDS
      model_filename <- paste0(output_folder, "/",method,"_",Xr,"_",Yr,"_", size)
      saveRDS(model, model_filename)
    }


    models[[paste0(method,"_",Xr,"_",Yr,"_", size)]]=model

    pb$tick()
  }

  # calculate validation metrics
  val_stats=val_train_size(models)

  # Return the list of trained models
  if(save_all){
    saveRDS(list(models=models,
                 val_stats=validation_stats),paste0(output_folder, "/",method,"_", all)
    )
  }

  if(return_all){
    return(list(models=models,
                val_stats=validation_stats))
  }

}
