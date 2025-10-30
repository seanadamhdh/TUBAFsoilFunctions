
#' @title label_creator
#' @description Soil lab labels generator. Multi-digit letter codes.
#' `r lifecycle::badge("stable")`
#' @param dataset Either vector or data.frame / tibble structure. length(dataset) or nrwo(dataset) ID labels will be created respectively.
#' @param origin Set an initial string which will be identical for all ID.
#' @param digits Total number of digits of the ID, including origin.
#' @returns Vector of ID strings corresponding to each entry (vector) or row (tibble) of dataset.
#' @import tidyverse
#' @export
label_creator<-function(dataset,origin="X",digits=4){
  fillable<-digits-str_length(origin)

  if(fillable==2){
    if(!is.null(nrow(dataset))){
      sample_count<-nrow(dataset)
    }else if(!is.null(length(dataset))){
      sample_count<-length(dataset)
    }else{
      print("ERROR")
      return(0)
    }


    j<-1
    k<-1
    x<-c()
    while(length(x)<sample_count){
      if(k>26){
        j<-j+1
        k<-1
      }
      if(j>26){

        j<-1
      }
      x<-c(x,
           paste0(
             origin,

             LETTERS[j],
             LETTERS[k]
           )
      )
      k<-k+1
    }
    return(x)
  }
  else if(fillable==3){
    if(!is.null(nrow(dataset))){
      sample_count<-nrow(dataset)
    }else if(!is.null(length(dataset))){
      sample_count<-length(dataset)
    }else{
      print("ERROR")
      return(0)
    }

    i<-1
    j<-1
    k<-1
    x<-c()
    while(length(x)<sample_count){
      if(k>26){
        j<-j+1
        k<-1
      }
      if(j>26){
        i<-i+1
        j<-1
      }
      x<-c(x,
           paste0(
             origin,
             LETTERS[i],
             LETTERS[j],
             LETTERS[k]
           )
      )
      k<-k+1
    }


    return(x)
  }else if(fillable>3){
    warning("Currently max 3 and min 2 free digits are supported.")
  }
}


