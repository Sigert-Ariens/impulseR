#' This function can be used to construct empirical trajectory plots, as in X, figure X.
#' The function first fits an ADL(p,q) model to the data using the estimate function. $p$ is the number of AR parameters, 
#' and $q$ the number of lagged covariate effects. The estimate function fits the model using OLS, with the lm() function in R base. 
#' It also processes the output and fixes initial innovations (see appendix X). The, cumulative resposnes over the study period are returned, which can be used
#' in the construction of empirical trajectory plots, see manuscript, figure X.   
#' #' 
#' @param data Dataframe containing the variables of interest
#' @param cols Character vector denoting the columns containing the variables of
#' interest. First character should denote the dependent variable, the second 
#' one the covariate of interest. Defaults to \code{c("y", "x")}
#' @param y_lags Integer denoting the number of AR effects, $p$, to estimate freely. 
#' Starts at the value \code{1}, allowing for an AR(1) structure. Defaults to \code{NA}, 
#' communicating that you don't want to allow for any AR effects.
#' @param x_lags Integer denoting the number of lagged covariate effects, $q$, to estimate freely. 
#' Starts at the value \code{0}, implying that only the contemporaneous effect is estimated freely. 
#' Defaults to \code{NA}, communicating that you don't want to estimate any covariate parameters 
#' @param burnin Logical (True/False) specifying if the part of the system response due to the intercept should be burned in analytically. 
#' Defaults to \code{FALSE} in order to generate empirical trajectory plots (see X). If set to \code{True}, there will usually be discrepancies between the
#' model implied total response $y_{t}$ and the observed values of the outcome variable $y_{t}^{obs}$ at the start of the time series. 
# 
#' 
#' @return List containing the fit of the model (under \code{"fit"}), the 
#' parameters that were used for the generation of the system responses (under 
#' \code{"intercept"}, \code{"x_params"}, and \code{"ar_params"}, and a dataframe 
#' containing the model implied total responses over the observation period (under \code{"y"}). 
#' Within the dataframe, column \code{"time"} contains the time index starting at 0. The columns 
#' \code{"irf_intercept"}, \code{"irf_x"}, \code{"irf_v"} contain the cumulative responses towards the unit vector, covariate, and innovations respectively. 
#' These partial responses sum up to the total response $y_{t}$, which should equal the observed values $y_{t}^{obs}$. The total response is provided in the
#' \code{"y"} column. Finally, the columns \code{"x"} and \code{"innovations"} contain 
#' the values of the covariate, \code{"x"}, and the estimated innovations (together with the first p innovations) \code{"innovations"}.
#' 
#' @examples 
#' # Generate data
#' set.seed(1)
#' data <- irf_generator(
#'   intercept = 1, 
#'   x_params = c(1, 2, -0.5),
#'   ar_params = c(0.9, -0.1, 0.25),
#'   x = rnorm(100),
#'   innovations = rnorm(100)
#' )
#' 
#' colnames(data) <- c("dependent", "independent")
#' 
#' # Fit a lag-2 ADL model to the data and calculate the cumulative responses over the study period:
#' 
#' out_empirical <- irf_empirical(
#'   data = data, 
#'   cols = c("dependent", "independent"),
#'   y_lags = 2,
#'   x_lags = 2
#' )
#' 
#' Pass the data to the irf_plot function to visualize the estimated system responses over the study period:
#' 
#' irf_plot(out_empirical)
#' 
#' 
#' @rdname irf_empirical
#' 
#' @export
irf_empirical <- function(data = NULL, 
                          cols = c("y", "x"),
                          y_lags = NA, 
                          x_lags = NA,
                          burnin = FALSE){
  
  # Call the estimate function. The parameters are estimated and initial innovations are fixed appropriately, see X. 
  
  params <- estimate(
    data, 
    cols = cols,
    y_lags = y_lags, 
    x_lags = x_lags
  )
  
  # Call the irf_generator function  on the observed covariate values and estimated innovations:
  output <- irf_generator(
    intercept = params$intercept,
    x_params = params$x_params,
    ar_params = params$ar_params,
    x = params$x, 
    innovations = params$innovations,
    burnin = FALSE #Burnin should be false for constructing empirical trajectory plots. Can be overridden. 
  )
  
  return(
    list(
      "fit" = params$fit, 
      "intercept" = params$intercept, 
      "x_params" = params$x_params, 
      "ar_params" = params$ar_params,
      "irf" = output
    )
  )  
  
  
  
  return(output)
}
