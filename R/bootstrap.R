#' Bootstrap system responses
#' 
#' This function describes a bootstrapping procedure for the system responses.
#' Specifically, uses the parameter vector (\code{mean}) and a covariance
#' matrix containing their interrelations (\code{covariances}) to sample \code{N}
#' artificial parameter sets, which are then used to generate the system 
#' responses through \code{\link[impulseR]{irf_generator}}. 
#' 
#' @param mean Numeric vector denoting the parameters that serve as mean values
#' for the bootstrap. Should consist of all relevant parameters in the order
#' specified in \code{parameter_names}.
#' @param covariances Numeric matrix denoting the variance - covariance matrix
#' of the parameters provided in \code{mean}. Note that the rows and columns 
#' should be ordered according to the same order in \code{mean} for the bootstrap
#' to make sense. 
#' @param parameter_names Character vector denoting the component to which the 
#' parameters in \code{mean} belong. May contain \code{"intercept"}, 
#' \code{"x_i"}, and \code{"y_i"} where \code{"i"} is replaced by an integer
#' denoting the lag of the parameter. If undefined, these names will be taken 
#' care off by \code{y_lags} and \code{x_lags} in that order. 
#' @param y_lags Integer denoting the number of AR effects, \eqn{p}, to estimate. 
#' Should start at the value `1`, implying a lag of 1 in the dependent variable. 
#' Defaults to `NA`, communicating that no lags in \eqn{y} are included.
#' @param x_lags Integer denoting the number of lagged covariate effects,
#' \eqn{q}, to estimate. Should starts at the value `0`, implying that only
#' the contemporaneous effect is taken into account. Defaults to `NA`, 
#' communicating that no lags in \eqn{x} are included.
#' @param N Integer denoting the number of samples to generate. Defaults to 
#' \code{1000}
#' @param ... Additional arguments provided to the 
#' \code{link[impulseR]{irf_generator}} function.
#' 
#' @return Data.frame of bootstrapped system responses having a structure similar
#' to the output of \code{\link[impulseR]{irf_generator}} with a column 
#' specifying the sample 
#' 
#' @examples 
#' # Example here
#' 
#' @export
bootstrap <- function(
  mean, 
  covariances, 
  y_lags = NA, 
  x_lags = NA,
  parameter_names = NULL,
  N = 1000,
  ...
) {

  # Check whether the covariances are a matrix
  if(!is.matrix(covariances)) {
    stop("Provided covariances should be a matrix.")
  }

  # Check whether the covariances are numeric
  if(!is.numeric(covariances)) {
    stop("Provided covariances should be numeric.")
  }

  # Check the dimensionality of the means and covariances
  d <- length(mean)
  if(nrow(covariances) != d | ncol(covariances) != d) {
    stop("Covariance matrix does not have the same dimensionality as the means.")
  }

  # Check for impossible lags
  if(!is.na(x_lags)) {
    if(x_lags < 0) {
      warning(
        paste(
          "Specified lags in `x` are below its minimal value (0).",
          "Assuming no involvement of the covariate."
        )
      )

      x_lags <- NA
    }
  }

  if(!is.na(y_lags)) {
    if(y_lags < 1) {
      warning(
        paste(
          "Specified lags in `y` are below its minimal value (1).",
          "Assuming no autoregression."
        )
      )

      y_lags <- NA
    }
  }

  # Create the parameter names if have not been created before
  if(is.null(parameter_names)) {
    # Not proud of what I'm about to do, but I don't see another way to do it
    # as easily 
    parameter_names <- c("intercept")

    if(!is.na(y_lags)) {
      parameter_names <- c(
        parameter_names, 
        paste("y_", seq(1, y_lags, 1), sep = "")
      )
    }

    if(!is.na(x_lags)) {
      parameter_names <- c(
        parameter_names, 
        paste("x_", seq(0, x_lags, 1), sep = "")
      )
    }
  }

  # Check whether the parameter names have the same size as the parameter 
  # themselves
  if(length(parameter_names) != d) {
    stop(
      paste(
        "Provided means do not correspond to the lags in x and/or y.",
        "Cannot make the mapping between means and their meaning within the model."
      )
    )
  }

  
}