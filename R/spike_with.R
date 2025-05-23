


#' @title isotopic tracer calculator
#'
#' @description
#' This function can be used to approximately calculate the amount of pure stable isotope tracer that needs
#' to be added to a reference solution to reach a desired change in delta permil isotope abundance.
#' Uses the assumption that isotope tracer amount is negligible compared to light fraction and
#' that the weight difference between heavy and light isotopes is also negligable.
#'
#' @param delta_x The delta value in permil of the current tracer abundance in the solution
#' @param delta_y The delta value in permil of the desired tracer abundance in the solution
#' @param R_std The isotopic ratio if the reference standard. Default is VSMOV (1/6420) for deuterium.
#' @param m_x The mass of the solution in grams. E.g., for 10L of water, m_x is approximately 10e4 g. Default is 10e3 (1L)
#'
#' @returns Amount of (pure) tracer that needs to be added in grams.
#' @export
#'
spike_with=function(
    delta_x,
    delta_y,
    R_std=1/6420,
    m_x=1000){

  mtracer=(delta_y-delta_x)/10^3*R_std*m_x

  return(mtracer)
}


## perpektivisch Verdünnung


