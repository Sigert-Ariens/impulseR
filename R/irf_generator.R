#' Generate impulse response functions
#' 
#' Overhead function that allows users to define impulse-response functions, 
#' either to explore the qualitative behavior of a user-specified model or to 
#' exploration or to decompose their data into a part due to the residuals and a  
#' part due to the covariate. Relies heavily on the \code{\link[impulseR]{irf}}
#' method.
#' 
#' @details 
#' Through its argument, this function distinguishes between four use-cases based
#' on two dimensions. First, the user can decide to either use a pre-specified 
#' parameter set, or to estimate the parameters based on a provided data set.
#' Second, the user can decide to investigate the model's response to a pre-
#' specified impulse in the residuals and/or covariate, or to investigate the 
#' model's cumulative response based on a provided data set. Each of the four
#' use-cases serves its own purpose and can be called in the way specified 
#' below.
#' 
#' \textit{Case 1 - Estimate parameters / Data-based impulses:} To compute the 
#' cumulative response of an estimated model, one should provide a 
#' \code{data.frame} to the argument \code{data}, together with an indication of 
#' how many lags in the residuals (\code{y_lags}) and the covariates 
#' (\code{x_lags}) one wants to include in the model. If the column names of the 
#' dependent and independent variables within the data set are not equal to 
#' \code{"y"} and \code{"x"} respectively, one should also change the value of 
#' the \code{cols} argument, providing it with a vector of two strings denoting 
#' the dependent variable and independent variable in that order. 
#' 
#' Based on this input, the function will estimate the parameters of the 
#' specified model using the \code{lm} function. The parameters will then be 
#' extracted, the values of the covariate and residual impulses computed, and 
#' the cumulative impulse responses will then be computed.
#' 
#' \textit{Case 2 - Estimate parameters / Pre-specified impulses:} In this case, 
#' one still provides a \code{data.frame} to the argument \code{data} and some 
#' values for the \code{y_lags} and \code{x_lags} arguments. Additionally, one 
#' should provide (scaled) impulses through the \code{residuals} and/or \code{x} 
#' arguments, which will serve as the input for the computations. Note that one 
#' can define their own vector, or that they can use the 
#' \code{\link[impulseR]{impulse}} or \code{\link[impulseR]{scaled_impulse}} 
#' functions to create such a vector.
#' 
#' \textit{Case 3 - Pre-specified parameters / Data-based impulses:} In this 
#' case, one still provides a \code{data.frame} to the argument \code{data}, but 
#' doesn't specify the \code{y_lags} and \code{x_lags} arguments. Instead, one 
#' should provide the pre-specified parameters of the model through the arguments
#' \code{intercept} (the intercept of the model), \code{ar_params} (the dynamic
#' parameters of the model), and \code{x_params} (the slopes of the covariates).
#' Note that you should not provide any values for the \code{residuals} and 
#' \code{x} arguments: Otherwise the function will execute Case 4.
#' 
#' \textit{Case 4 - Pre-specified parameters / Pre-specified impulses:} Finally, 
#' this case requires the definition of the parameters (\code{intercept}, 
#' \code{ar_params}, \code{x_params}) and the impulses (\code{residuals} and/or
#' \code{x}). 
#' 
#' @param intercept Numeric denoting the intercept to use for the impulse response 
#' function. Defaults to \code{0}
#' @param ar_params Numeric vector denoting the autoregressive parameters to be
#' used for the impulse response function. Parameters need to be given in order
#' of increased lag (i.e., first element for t - 1, second element for t - 2,...).
#' Defaults to \code{0}
#' @param x_params Numeric vector denoting the values of the slopes for the 
#' exogenous variables. Parameters again need to be given in order of increased
#' lag (i.e., first element for t, second element for t - 1,...). Defaults to 
#' \code{0}
#' @param data Dataframe containing the variables of interest
#' @param cols Character vector denoting the columns containing the variables of
#' interest. First character should denote the dependent variable, the second 
#' one the covariate of interest. Defaults to \code{c("y", "x")}
#' @param y_lags Integer denoting the number of lags to include for the 
#' dependent variable. Starts at the value \code{1}. Defaults to \code{NA}, 
#' communicating that you don't want to use any lagged values of the dependent 
#' variable
#' @param x_lags Integer denoting the number of lags to include for the 
#' covariate. Starts at the value \code{0}. Defaults to \code{NA}, communicating 
#' that you don't want to use any lagged values of the dependent variable
#' @param x Numeric vector denoting the values of the exogenous variables 
#' X at each time t. These values serve as one type of impulses to the system to 
#' be simulated. Defaults to \code{NULL}, which means an empty vector of the same
#' length as \code{residuals} (if provided).
#' @param residuals Numeric vector denoting the values of the residuals at each 
#' time t. These values serve as one type of impulses to the system to be 
#' simulated. Defaults to \code{NULL}, which means an empty vector of the same
#' length as \code{x} (if provided).
#' 
#' @return Dataframe containing the impulse responses. The column \code{"time"} 
#' contains the time step starting at 0. The columns \code{"irf_intercept"}, 
#' \code{"irf_x"}, \code{"irf_eps"} contain the expected impulse responses 
#' for the intercept, the exogeneous variables, and the residuals respectively
#' and sum up to the total impulse response under column \code{"irf"}. Finally,
#' the columns \code{"x"} and \code{"residuals"} contain the provided values 
#' for those arguments.
#' 
#' @examples 
#' # Generate data
#' set.seed(1)
#' data <- irf(
#'   intercept = 1, 
#'   x_params = c(1, 2, -0.5),
#'   ar_params = c(0.9, -0.1, 0.25),
#'   x = rnorm(100),
#'   residuals = rnorm(100)
#' )
#' colnames(data) <- c("dependent", "independent")
#' 
#' # Case 1: Estimated parameters / Data-based impulses
#' irf_generator(
#'   data = data, 
#'   cols = c("dependent", "independent"),
#'   y_lags = 2,
#'   x_lags = 2
#' )
#' 
#' # Case 2: Estimated parameters / Pre-specified impulses
#' irf_generator(
#'   data = data, 
#'   cols = c("dependent", "independent"),
#'   y_lags = 2,
#'   x_lags = 2,
#'   residuals = impulse(10),
#'   x = impulse(10)
#' )
#' 
#' # Case 3: Pre-specified parameters / Data-based impulses
#' irf_generator(
#'   data = data, 
#'   cols = c("dependent", "independent"),
#'   intercept = 1,
#'   ar_params = c(0.7, 0.2),
#'   x_params = c(2, 1)
#' )
#' 
#' # Case 4: Pre-specified parameters / Pre-specified impulses
#' irf_generator(
#'   intercept = 1,
#'   ar_params = c(0.7, 0.2),
#'   x_params = c(2, 1),
#'   residuals = impulse(10),
#'   x = impulse(10)
#' )
#' 
#' @rdname irf_generator
#' 
#' @export
irf_generator <- function(intercept = 0, 
                          ar_params = 0, 
                          x_params = 0,
                          data = NULL, 
                          cols = c("y", "x"),
                          y_lags = NA, 
                          x_lags = NA,
                          x = NULL,
                          residuals = NULL) {
  
  # Call the `irf` function for a given combination of inputs to allow for four 
  # different possibilities, crossing the levels of Impulse Definitions (user-
  # provided vs defined through data) vs Parameter Definitions (user-provided vs
  # estimated through data). 
  #
  # Specifically, we defined:
  #   - If the data are defined and the lags are defined, then we estimate the 
  #     parameters of the model. The function will automatically differentiate 
  #     between using the cumulative responses of the data or using the user-
  #     provided covariate and/or residual impulses
  #   - If data are defined and the user specifies a model through its parameters,
  #     then we will investigate the cumulative responses that come from the data.
  #     Importantly, the values for the covariates and the residuals should be 
  #     NULL
  #   - Otherwise, we assume the user provides both parameters and impulses

  # Cases 1 & 2: 
  #   - Estimate parameters + User-provided impulses
  #   - Estimate parameters + Cumulative responses
  if(!is.null(data) & !is.na(y_lags) & !is.na(x_lags)) {
    return(
      irf(
        data, 
        cols = cols,
        y_lags = y_lags, 
        x_lags = x_lags, 
        x = x, 
        residuals = residuals
      )
    )

  # Case 3:
  #   - User-provided parameters + Cumulative responses
  } else if(!is.null(data) & is.null(residuals) & is.null(x)) {
    # Retrieve covariates and compute residuals of the model with the specified 
    # data.
    x <- data[, cols[2]]
    residuals <- compute_residuals(
      data, 
      cols = cols,
      intercept = intercept, 
      ar_params = ar_params, 
      x_params = x_params
    )

    # Provide to irf
    return(
      irf(
        intercept = intercept, 
        ar_params = ar_params, 
        x_params = x_params, 
        x = x, 
        residuals = residuals,
        burnin = FALSE
      )
    )

  # Case 4: 
  #   - User-provided parameters + User-provided impulses
  } else {
    return(
      irf(
        intercept = intercept, 
        ar_params = ar_params, 
        x_params = x_params, 
        x = x, 
        residuals = residuals,
        burnin = TRUE
      )
    )
  }
}