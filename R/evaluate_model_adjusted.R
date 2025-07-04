#' @title Calculate a range of evaluation metrics
#' @description This function is based on simplerspec::evaluate_model by
#' Baumann P (2020). _simplerspec: Soil and plant spectroscopic model building and prediction_. R package version
#' 0.1.0.9001, <https://github.com/philipp-baumann/simplerspec>.
#' Several metrics have been added to the function (e.g. R2 and linsccc)
#' `r lifecycle::badge("stable")`
#' @param data Tibble, data.frame containing at least 2 columns with numeric data (observations, predicitions)
#' @param obs String for selectingobservation column
#' @param pred String for selecting prediction column
#' @import tidyverse
#' @import DescTools
#' @import e1071
#'
#' @export
evaluate_model_adjusted<-function (data, obs="obs", pred="pred")
{

  if(!require(tidyverse)){
    install.packages("tidyverse")
    require(tidyverse)
  }



  if(!is.null(data)){
  obs <- rlang::enquo(obs)
  pred <- rlang::enquo(pred)
  obs <- dplyr::pull(data, !!obs)
  pred <- dplyr::pull(data, !!pred)
  }

  tibble::tibble(
    n = length(na.omit(obs)),
    min = min(obs, na.rm = TRUE),
    max = max(obs, na.rm = TRUE),
    mean = mean(obs, na.rm = TRUE),
    median = median(obs, na.rm = TRUE),
    iqr=IQR(obs,na.rm = TRUE),
    sdev = sd(obs, na.rm = TRUE),
    cv = sd(obs, na.rm = TRUE) / mean(obs, na.rm = TRUE),
    skewness_b1 = e1071::skewness(obs, na.rm = TRUE, type = 3),
    kurtosis = e1071::kurtosis(obs, na.rm = TRUE),
    rmse = mean((obs -
                   pred) ^
                  2, na.rm = TRUE) ^ 0.5,
    rmsre=  mean(((pred/obs)-1) ^
                   2, na.rm = TRUE) ^ 0.5,
    nrmseavg=(mean((obs -
                     pred) ^
                    2, na.rm = TRUE) ^ 0.5)/mean(obs,na.rm=T),
    nrmseiqr=(mean((obs -
                      pred) ^
                     2, na.rm = TRUE) ^ 0.5)/IQR(obs,na.rm=T),
        mse = mean((obs - pred) ^ 2,
               na.rm = TRUE),
    me = mean(obs - pred, na.rm = TRUE),
    bias = mean(obs - pred, na.rm = TRUE),
    msv = mean(((
      mean(obs,
           na.rm = TRUE) - obs
    ) - (
      mean(pred, na.rm = TRUE) -
        pred
    )) ^ 2),
    sde = mean(((
      mean(obs, na.rm = TRUE) -
        obs
    ) - (
      mean(pred, na.rm = TRUE) - pred
    )) ^ 2) ^ 0.5,
    mae = mean(abs(obs - pred), na.rm = TRUE),
    r2 = cor(obs,
             pred, use = "pairwise.complete.obs") ^
      2,
    R2 = 1 - sum((pred - obs) ^ 2, na.rm = T) / sum((obs - mean(obs, na.rm = T)) ^ 2, na.rm = T),
    b = lm(obs ~
             pred)$coefficients[2],
    rpd = sd(obs, na.rm = TRUE) / mean((obs -
                                          pred) ^
                                         2, na.rm = TRUE) ^ 0.5,
    rpiq = (
      quantile(obs,
               0.75, na.rm = TRUE) - quantile(obs, 0.25, na.rm = TRUE)
    ) / mean((obs -
                pred) ^
               2, na.rm = TRUE) ^ 0.5,
    linsCCC = DescTools::CCC(obs,pred,na.rm = T)$rho.c$est,
   SB = (mean(obs - pred,
               na.rm = TRUE)) ^
      2,
    NU = mean((pred - mean(pred)) ^ 2) *
      (1 - lm(obs ~ pred)$coefficients[2]) ^ 2,
    LC = mean((obs -
                 mean(obs)) ^
                2) * (1 - cor(obs, pred, use = "pairwise.complete.obs") ^ 2),
    SB_prop = round((mean(obs - pred, na.rm = TRUE)) ^ 2 /
                      mean((pred -
                              obs) ^
                             2) * 100, 0),
    NU_prop = round(mean((pred -
                            mean(
                              pred
                            )) ^ 2) * (1 - lm(obs ~ pred)$coefficients[2]) ^ 2 / mean((pred -
                                                                                         obs) ^
                                                                                        2) * 100, 0),
    LC_prop = round(mean((obs - mean(
      obs
    )) ^ 2) *
      (
        1 - cor(obs, pred, use = "pairwise.complete.obs") ^ 2
      ) / mean((pred -
                  obs) ^
                 2) * 100, 0)
  )
}
