

## 95 precent prediction interval ####

#' @title confidence band calculator
#' @description calculates (linear) 95 % confidence band around values or for obs vs. pred
#' @param pred vector with (predicted) values
#' @param obs vector with reference values. Default is mean(pred)
#' @export
conf_95<-function(pred,obs=mean(pred)){
  2*sqrt(
    sum(
      (pred-obs)^2
    )
    /
      (length(pred)-1)
  )
}



#' @title ggplot default color emulator
#' @description Emulate ggplot color scale
#' @param n number of colors to generate
#' @export
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}




#' @title  basic statistics summary
#' @description calculates set of basic metrics (min,max, quartiles, mean...)
#' @param dataset tibble, data.frame with at least one numeric value
#' @param grouping_variable Optional. String or vector of strings describing the columns that should be used for grouping
#' @param variables String or vector of strings of variables that should be summarised.
#' @returns summary_df: Tibble with column varaible, if defined grouping_variable and corresponding summary statistics.
#' @import tidyverse
#' @export
summarise_metrics<-function(dataset,grouping_variables=NULL,variables=NULL){
  summary_df<-tibble()

  for (i in variables){
    summary_df<-bind_rows(
      summary_df,
      tibble(
        variable=i,
        summarise(
          group_by(dataset,across(all_of(grouping_variables))),
          n=length(na.omit(.data[[i]])),
          min=min(na.omit(.data[[i]])),
          q25=quantile(na.omit(.data[[i]]),.25),
          median=median(na.omit(.data[[i]])),
          mean=mean(na.omit(.data[[i]])),
          q75=quantile(na.omit(.data[[i]]),.75),
          max=max(na.omit(.data[[i]])),
          sd=sd(na.omit(.data[[i]])),
          var=var(na.omit(.data[[i]]))
        )
      )
    )
  }
  return(summary_df)
}


#' @title wavenumber-wavelength converter
#' @description Wavenumber to wavelength conversion. Works both ways (wn-wl, wl-wn).
#' @param x Numeric vector
#' @returns converted to respective other unit.
#' @export
wavenumber_wavelength<-function(x){return(10^9/(x*10^2))}



#' @title duplication check
#' @description Checks if a vector contains any duplicated values. Unlike duplicated(), it returns TRUE for all values that appear more than once.
#' duplicated() returns TRUE only for the 2nd element.
#' @param x Input vector
#' @returns boolean vector of length length(x).
#' @export
all_duplicates<-function(x){return(duplicated(x)|duplicated(x,fromLast=T))}


#' @title mean error
#' @param obs Observations, reference values
#' @param pred Predicted values
#' @export
ME <- function(obs, pred){
  mean(pred - obs, na.rm = TRUE)
}



#' @title root mean squared error
#' @param obs Observations, reference values
#' @param pred Predicted values
#' @export
RMSE <- function(obs, pred){
  sqrt(mean((pred - obs)^2, na.rm = TRUE))
}


#' @title degree of determination
#' @param obs Observations, reference values
#' @param pred Predicted values
#' @export
R2 <- function(obs, pred){
  # sum of the squared error
  SSE <- sum((pred - obs) ^ 2, na.rm = T)
  # total sum of squares
  SST <- sum((obs - mean(obs, na.rm = T)) ^ 2, na.rm = T)
  R2 <- 1 - SSE/SST
  return(R2)
}





#' @title get ggthemes discrete colorblind-safe colors
#' @description https://jrnold.github.io/ggthemes/reference/colorblind.html
#' @returns 8 discrete colors as hex-string vector
#' @export
colorblind_safe_colors=function(){
  return(c( "#000000",
            "#E69F00",
            "#56B4E9",
            "#009E73",
            "#F0E442",
            "#0072B2",
            "#D55E00",
            "#CC79A7" )
         )
}




#' @title Date to years after start
#' @description Calculates time difference from dates as fraction of years. Small error possible because using average of 365.25 days/year for calculation.
#' @param dates vector or single date as character ("%Y-%m-%d"-format) or POSIXct or Date
#' @param start Reference date as character ("%Y-%m-%d"-format) or POSIXct or Date. Can be later than dates - returns negative values.
#' @export
yr_frac=function(dates,start="1995-01-01"){return(as.numeric(difftime(dates,start,units = "days"))/365.25)}

#' @title none
#' @description Function that does absolutely nothing and returns data as is.
#' @param x Input
#' @returns x
#' @export
none=function(x){return(x)}

