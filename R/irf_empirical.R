#' Estimate and compute system responses for an 
#' \ifelse{html}{\out{<span class="mathjax-latex">\(ADL(p, q)\)</span>}}{\eqn{ADL(p, q)}}
#' 
#' This function can be used to construct empirical trajectory plots. First, an
#' \ifelse{html}{\out{<span class="mathjax-latex">\(ADL(p, q)\)</span>}}{\eqn{ADL(p, q)}} 
#' model is fitted to the data using the \code{\link[impulseR]{estimate}} function, 
#' where \ifelse{html}{\out{<span class="mathjax-latex">\(p\)</span>}}{\eqn{p}} 
#' is the number of AR parameters and 
#' \ifelse{html}{\out{<span class="mathjax-latex">\(q\)</span>}}{\eqn{q}} the 
#' number of lagged covariate effects. Then, the function creates cumulative 
#' responses through the \code{\link[impulseR]{irf_generator}} function.   
#' 
#' @details 
#' Estimation is currently performed through OLS, using the base \code{lm}
#' function. 
#' 
#' @param data Dataframe containing the variables of interest
#' @param cols Character vector denoting the columns containing the variables of
#' interest. First character should denote the dependent variable, the second 
#' one the covariate of interest. Defaults to \code{c("y", "x")}
#' @param y_lags Integer denoting the number of AR effects, 
#' \ifelse{html}{\out{<span class="mathjax-latex">\(p\)</span>}}{\eqn{p}}, to 
#' estimate. Should start at the value \code{1}, implying a lag of 1 in the 
#' dependent variable. Defaults to \code{NA}, communicating that you don't want 
#' to allow for any AR effects.
#' @param x_lags Integer denoting the number of lagged covariate effects, 
#' \ifelse{html}{\out{<span class="mathjax-latex">\(q\)</span>}}{\eqn{q}}, 
#' to estimate. Should starts at the value \code{0}, implying that only the 
#' contemporaneous effect is taken into account. Defaults to \code{NA}, 
#' communicating that you don't want to estimate any covariate parameters 
#' @param burnin Logical specifying if the part of the system response due to 
#' the intercept should be burned in analytically. Defaults to \code{FALSE}, as 
#' expected analytically. If set to \code{TRUE}, there will usually be 
#' discrepancies between the model-implied total response 
#' \ifelse{html}{\out{<span class="mathjax-latex">\(y_t\)</span>}}{\eqn{y_t}} 
#' and the observed values of the outcome variable 
#' \ifelse{html}{\out{<span class="mathjax-latex">\(y_t^{obs}\)</span>}}{\eqn{y_t^{obs}}}
#' at the start of the time series.  
#' 
#' @return List containing all information provided by 
#' \code{\link[impulseR]{irf_generator}} as well as the fitted model (under 
#' \code{"fit"}).
#' 
#' @examples 
#' # Generate data
#' set.seed(1)
#' data <- irf_generator(
#'   intercept = 1, 
#'   x_params = c(1, 2, -0.5),
#'   ar_params = c(0.75, -0.15, 0.05),
#'   x = rnorm(100),
#'   innovations = rnorm(100)
#' )$irf
#' 
#' colnames(data) <- c("dependent", "independent")
#' 
#' # Fit a lag-2 ADL model to the data and calculate the cumulative responses
#' result <- irf_empirical(
#'   data = data, 
#'   cols = c("dependent", "independent"),
#'   y_lags = 2,
#'   x_lags = 2
#' ) 
#' 
#' @export
irf_empirical <- function(data = NULL, 
                          cols = c("y", "x"),
                          y_lags = NA, 
                          x_lags = NA,
                          burnin = FALSE){
  
  # Call the estimate function. The parameters are estimated and initial 
  # innovations are fixed appropriately. 
  params <- estimate(
    data, 
    cols = cols,
    y_lags = y_lags, 
    x_lags = x_lags
  )
  
  # Call the irf_generator function  on the observed covariate values and 
  # estimated innovations:
  result <- irf_generator(
    intercept = params$intercept,
    x_params = params$x_params,
    ar_params = params$ar_params,
    x = params$x, 
    innovations = params$innovations,
    burnin = burnin
  )
  result[["fit"]] <- params$fit 

  return(result)
}
