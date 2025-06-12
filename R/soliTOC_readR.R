

#' @title Fix soliTOC csv colnames
#' @description
#' Automatically fix shifted colnames in soliTOC excel exports.
#'
#' @param soliTOC_raw_excel An Excel file as exported from soliTOC software
#' @param save Should the fixed df be saved as csv? Default FALSE
#' @param save_location Optional: a filepath (C:/dir/other_dir/myFile.csv), where the fixed df should be stored as csv.
#' Default is "keep". This uses soliTOC_raw_excel to get a filepath and saves output as basename_fixed.csv
#' @param return Should the output be returned? Default TRUE
#' @param Date_Time_colname_postion For comparability: Which column contains the separated Date and Time entry?
#' Default is the 6.
#' @param time.col_name Used as failsafe. Checks if the time.col_name string can still be found in the date column name.
#' If TRUE, proceed, else stop (already fixed column name), because function would shift colnames wrongfully.
#'
#' @import tidyverse
#' @export
fix_soliTOC_colnames=function(soliTOC_raw_excel,
                              save=F,
                              save_location="keep",
                              return=T,
                              Date_Time_colname_position=6,
                              time.col_name="Zeit"){
  #load raw excel
  if(is.character(soliTOC_raw_excel)){
    soliTOC_file=read_excel(soliTOC_raw_excel)
  }else if (is.data.frame(soliTOC_raw_excel)){
    soliTOC_file=soliTOC_raw_excel
  }else{
    print("ERROR: Neither path to soliTOC Excel file or data.frame with soliTOC data provided.")
    return(NULL)
  }
  colnames=names(soliTOC_file)

  if(str_detect(colnames[Date_Time_colname_position],time.col_name)){

    colnames_new=c(colnames[1:(Date_Time_colname_position-1)],
                   str_split_fixed(colnames[6],pattern = " ",n = 2)[1,1]%>%str_remove_all(pattern = " "),
                   str_split_fixed(colnames[6],pattern = " ",n = 2)[1,2]%>%str_remove_all(pattern = " "),
                   colnames[(Date_Time_colname_position+1):(length(colnames)-1)])
    # debug
    print(colnames_new)
    names(soliTOC_file)<-colnames_new
    # debug
    print(names(soliTOC_file))
  }else{
    print("Warning: No changes made. Column names seem to be in order. Check manually.")
  }

  if(save==T){
    if (save_location=="keep"&is.character(soliTOC_raw_excel)){
      write.csv(soliTOC_file,
                paste0(dirname(soliTOC_raw_excel),
                       "/",
                       str_remove(basename(soliTOC_raw_excel),
                                  ".xlsx")[1],
                       "_fixed.xlsx"
                )
      )
    }else if(!save_location=="keep"){
      write.csv(soliTOC_file,save_location)
    }else{
      print("Warning: No save location provided. No file saved.")
    }
  }
  if(return==T){
    return(soliTOC_file)
  }

}





#' @title get dayfactors
#' @description
#' function for pulling dayfactors from soliTOC file
#' Creates column called factor with respective dayfactors. Does not yet apply them.
#'
#' Measurements are grouped with preceding repeated measurements of standard sample(s).
#' If multiple repeated standard measurements, the standard is averaged for each set. A dayfactor is
#' calculated for each set based on the average standard measurement and the theoretical value.
#' The dayfactor is applied to all following samples, until a new set of standards is detected.
#' Lets say, this is a small measurement file. Standards were named "S".
#' This function will aggregate the measurements as follows:
#' S-S-S-A-B-C-D-E-F-G-S-S-S-H-I-J-K-L
#' 1-1-1-1-1-1-1-1-1-1-2-2-2-2-2-2-2-2
#' Then, factor_1 would be calculated as S_acutal/S1 and applied to all samples.
#' A (sub)string of the sample names that the user wants to get returned needs to be provided.
#'
#' @param dataset Exported soliTOC csv file
#' @param IC_col Sample names column. Should also include standard identifiers (Default is Name, as in soliTOC software)
#' @param std_id Identifier of the standards for dayfactor calculation (default is caco3)
#' @param value_col Column that contains measurement of standard (default is 'TC  [%]',
#' as caco3 should not contain OC, so this captures all released C)
#' @param actual Theoretical standard concentration in wt-% (default for caco3 is 12)
#' @param keep_batch Logical: Function creates helper variable "batch" to assign dayfactors. Should this be kept?
#' Default is FALSE. Used mostly for debug.
#' @import tidyverse
#' @export
get_dayfactors<-function(dataset,    #data.frame or tibble
                         ID_col="Name",   #id column
                         std_id="caco3",  #reference id
                         value_col="TC  [%]", # value col
                         actual=12, #theroretical concentration of reference
                         keep_batch=F
){

  dataset<-mutate(dataset,
                  factor=if_else(.data[[ID_col]]==std_id,
                                 actual/.data[[value_col]],
                                 NA))->dummy_df #calc factors for each reference sample


  if(is.na(dataset$factor[1])){dataset$factor[1]<-1} #first overall measurement,
  #if no reference sample, init with 1. usually runin anyway

  # calculates "batches" from start of each chunck of reference samples, to the next chunk
  # (sample||ref,ref,sample,...,sample || ref,ref,ref,sample,...,sample||ref...)
  {
    b<-0
    batch<-c(b)
    for (i in c(2:nrow(dataset))){
      if(dataset[[ID_col]][i]==std_id&dataset[[ID_col]][i-1]!=std_id){b<-b+1} #check if new set started
      batch<-c(batch,b)
    }
  }
  # attaching batch vector to df and averaging factors from reference samples for each batch
  dataset<-tibble(dataset,batch)
  dataset%>%group_by(batch)%>%summarise(mean(factor,na.rm=T))->factor_list

  # extending full vector and replacing averages with original factors for reference samples
  left_join(data.frame(batch),factor_list,by="batch")->factor_
  names(factor_)<-c("batch","factor")
  factor_<-mutate(factor_,final_factor=if_else(dummy_df$factor%>%is.na,factor,dummy_df$factor))

  # replacing factors in dataset with the finalised factro vector
  dataset$factor<-factor_$final_factor
  if(!keep_batch){
    dataset<-select(dataset,-c("batch"))
  }

  return(dataset)

}























#' @title pull_set_soliTOC
#' @description
#' Function for the correction and aggregation of SoliTOC measurements.
#' Loads a raw excel file as exported from soliTOC software.
#' Calculates and applies dayfactor correction based on standard measurements.
#' Select and aggregate samples based on identifer string(s).
#'
#' @param soliTOC_file Excel file containing raw export from soliTOC Elementar software.
#' Check for proper column naming, as Date Time is typically read as 2 columns, bt only one column name.
#' This shifts all subsequent col names to the left. Fix manually.
#' @param ID_col_set Which column contains the identifier for the sample selection? Default is "Name"
#' @param set_id String or vector of strings which represent the unique identifier(s) that are searched for in
#' `ID_col_set` by str_detect
#' @param ID_col_std Which columns contain the identifier for the standard selection? Default is "Name"
#' @param std_id String or vector of strings which represent the unique identifier(s) that are searched for in
#' `ID_col_std` by str_detect. Default is "caco3".
#' @param Memo_col_set Which column contains the additional Memo identifier?
#' @param set_memo Optional string or vector to search for in Memo_col_set. Optional - to omit this subset layer set to NA.
#' @param std_value_col Which column containes the measurements
#' @param actual True value (concentration). Default is 12 (wt-%) TC for caco3.
#' @param keep_batch Should the column containing aggregation information be kept? Default=F
#' @param omit_check_cols Override checking of columns integrity. Default=F.
#'
#' @returns A tibble or data.frame containing the dayfactor corrected C-fractions for the samples selected by detection of  ID_col
#' @import tidyverse
#' @export

pull_set_soliTOC=function(soliTOC_file,
                          fix_cols=T,
                          ID_col_set="Name",
                          set_id=c("LA","LQ","LR","LP","LG"),
                          ID_col_std="Name",
                          std_id="caco3",
                          Memo_col_set="Memo",
                          set_memo=c("GT300"),
                          std_value_col="TC  [%]",
                          actual=12,
                          keep_batch=F,
                          omit_check_cols=F){

  #load raw excel ####
  if(is.character(soliTOC_file)){
    #debug
    print("read excel")
    soliTOC_raw=read_excel(soliTOC_file)
  }else if (is.data.frame(soliTOC_file)){
    #dataframe provided
    soliTOC_raw=soliTOC_file
  }else{
    print("ERROR: Neither path to soliTOC Excel file or data.frame with soliTOC data provided.")
    return(NULL)
  }


  ## fix colnames #### if user specifies it
  if(fix_cols){
    soliTOC_raw=fix_soliTOC_colnames(soliTOC_raw)
  }

  print(soliTOC_raw)
  ## check Excel colnames ####
  if((!omit_check_cols)& # manually turn check off, in case of issues
     (names(soliTOC_raw)%>%last%>%str_remove("...")%>%str_detect("[:digit:]")) #are there unnamed cols? Fix structure in Excel before loading
  ){
    print("ERROR: Colnames do not match data. Check raw Excel format. Probably Date/Time separation issue.")
    return(NULL)
  }else{
    # calculating dayfactors
    soliTOC_all=get_dayfactors(soliTOC_raw,
                               ID_col = ID_col_std,
                               std_id = std_id,
                               value_col = std_value_col,
                               actual = actual,
                               keep_batch = keep_batch
    )

    # applying dayfactors
    soliTOC_all=mutate(soliTOC_all,
                       TOC400=`TOC400  [%]`*factor,
                       ROC=`ROC  [%]`*factor,
                       TIC900=`TIC900  [%]`*factor,
                       TOC=`TOC  [%]`*factor,
                       TC=`TC  [%]`*factor)

    # only return measurments that contain id string as part of their value in ID_col_set
    if(!any(is.na(set_memo))){
       soliTOC_set=soliTOC_all%>%
          filter(str_detect(.data[[ID_col_set]],paste(set_id,collapse = "|"))&
                    str_detect(.data[[Memo_col_set]],paste(set_memo,collapse = "|")))
        }else{
          soliTOC_set=soliTOC_all%>%
            filter(str_detect(.data[[ID_col_set]],paste(set_id,collapse = "|")))
          }

    return(soliTOC_set)

  }
}



#' @title duplicate filtering
#' @description
#' soliTOC_remove_duplicates
#' Function that removes repeated samples from soliTOC file based on
#' a) lowest MAE to reference data for SOC (named CORG)
#' b) averageing duplicates
#' c) selecting the latest measured replicate
#'
#' @param dataset soliTOC data.frame or tibble that has already undergone get_dayfactors.
#' Using raw soliTOC data could lead to loss of e.g., standard data, as they would be flagged as duplicates.
#' @param reference Logical. Is reference data provided? If TRUE, a column labeled CORG containing SOC data needs to be attached
#' to the data.frame. This will be compared with the measured TOC to determine best match based on MAE.
#' @param measurement_method soliTOC program name used (e.g. Default="DIN19539").
#' @param method duplicate resolving method.
#' "best_MAE" for a), only available if reference=T.
#' "mean" for b)
#' "latest" for c)
#' @param .col_name several arguments to set column names. Default are German names.
#' Adjust when using Elementar soliTOC software in different language.
#' @import tidyverse
#' @export
soliTOC_remove_duplicates<-function(dataset,
                                    measurement_method="DIN19539",
                                    reference=T,
                                    method="best_MAE",
                                    # defaulting german col names
                                    measurement_method.col_name="Methode",
                                    date.col_name="Datum",
                                    time.col_name="Zeit",
                                    name.col_name="Name",
                                    reference.col_name="CORG",
                                    measured.col_name="TOC"
){
  # get duplicates
  filter(dataset,.data[[measurement_method.col_name]]==measurement_method)%>%
    filter(.data[[name.col_name]]%in%
             {dataset%>%filter(.data[[measurement_method.col_name]]==measurement_method)%>%
                 filter(duplicated({dataset%>%filter(.data[[measurement_method.col_name]]==measurement_method)%>%
                     pull(name.col_name)}))%>%pull(name.col_name)}
    )->duplicates

  print("duplicates found")
  print(nrow(duplicates))


  if(nrow(duplicates>0)){
    if(reference==T&method=="best_MAE"){
      #print(names(duplicates))
      duplicates%>%mutate(MAE=abs(.data[[measured.col_name]]-.data[[reference.col_name]]))%>%
        group_by(.data[[name.col_name]])%>%filter(MAE==min(MAE))->best_measurements # get the better measurements
      #best_measurements<-select(best_measurements,-c("MAE"))
      dataset%>%filter(!(paste(.data[[date.col_name]],.data[[time.col_name]])%in%paste(duplicates[[date.col_name]],duplicates[[time.col_name]]))| # remove all duplicates
                         paste(.data[[date.col_name]],.data[[time.col_name]])%in%paste(best_measurements[[date.col_name]],best_measurements[[time.col_name]]) # re-introduce / allow those who are "best"
      )->dataset_new
    }else if(method=="mean"){
      # average of replicates, first value for non-numeric (or integer,double) values
      #print(names(duplicates))
      duplicates%>%
        group_by(.data[[name.col_name]])%>%
        summarise(across(where(is.numeric),~mean(.x,na.rm = T)),
                  across(where(is.integer),~mean(.x,na.rm = T)),
                  across(where(is.double),~mean(.x,na.rm = T)),
                  across(where(is.character),~first(.x,na_rm = T)),
                  across(where(is.logical),~first(.x,na_rm = T)))->best_measurements # get the better measurements

      dataset%>%filter(!(paste(.data[[date.col_name]],.data[[time.col_name]])%in%paste(duplicates[[date.col_name]],duplicates[[time.col_name]]))| # remove all duplicates
                         paste(.data[[date.col_name]],.data[[time.col_name]])%in%paste(best_measurements[[date.col_name]],best_measurements[[time.col_name]]) # re-introduce / allow those who are "best"
      )->dataset_new
    }else if (method=="latest"){
      duplicates%>%
        group_by(.data[[name.col_name]])%>%
        summarise(across(everything(),~last(.x,na_rm = T))
                  # across(where(is.integer),~last(.x,na_rm = T)),
                  #  across(where(is.double),~last(.x,na_rm = T)),
                  #  across(where(is.character),~last(.x,na_rm = T)),
                  #  across(where(is.logical),~last(.x,na_rm = T))
        )->best_measurements # get the better measurements

      dataset%>%filter(!(paste(.data[[date.col_name]],.data[[time.col_name]])%in%paste(duplicates[[date.col_name]],duplicates[[time.col_name]]))| # remove all duplicates
                         paste(.data[[date.col_name]],.data[[time.col_name]])%in%paste(best_measurements[[date.col_name]],best_measurements[[time.col_name]]) # re-introduce / allow those who are "best"
      )->dataset_new
    }

    print("duplicates removed")
    print(nrow(dataset)-nrow(dataset_new))
    #return(list(new=dataset_new,original=dataset,duplicates=duplicates,re_intro=best_measurements)) # debug output
    return(dataset_new)
  }else{
    return(dataset)
  }
}


