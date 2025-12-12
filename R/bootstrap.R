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
#
# INTERNAL MESSAGE: Keep `x` in here as an argument: Otherwise R will confuse 
# `x_lags` with `x` when the former is not specified. Not sure why this bug occurs, 
# but used this workaround for now.
bootstrap <- function(
  mean, 
  covariances, 
  y_lags = NA, 
  x_lags = NA,
  parameter_names = NULL,
  N = 1000,
  x = NULL,
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

  # If x_lag and y_lag are both NA, but parameter_names are provided, then we 
  # extract the lag of each variable. Note that if parameter_names is not 
  # provided, that we fall back to the case of only an intercept/mean and no 
  # dynamical or lagged parameters
  if(is.na(x_lags) & is.na(y_lags) & !is.null(parameter_names)) {
    # Extract the lags in y
    idx <- grepl("y_", parameter_names, fixed = TRUE)
    y_lags <- max(
      as.numeric(
        gsub(
          "y_",
          "",
          parameter_names[idx],
          fixed = TRUE
        )
      )
    )

    # Extract the lags in x
    idx <- grepl("x_", parameter_names, fixed = TRUE)
    x_lags <- max(
      as.numeric(
        gsub(
          "x_",
          "",
          parameter_names[idx],
          fixed = TRUE
        )
      )
    )
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

  # Define a variable that contains all parameters needed to compute the system
  # responses according to particular lags. These will help with identifying the
  # values of the correct parameters in the bootstrap, keeping those that are 
  # irrelevant to 0 and providing those that are relevant (in parameter_names)
  # with values for their parameters.
  #
  # Not proud of what I do here, but I don't see another way to do it as easily.
  # Note that the way this is done now, it automatically puts the parameters in 
  # the correct order (as expected by irf_generator) 
  colnames <- c("intercept")

  if(!is.na(y_lags)) {
    colnames <- c(
      colnames, 
      paste("y_", seq(1, y_lags, 1), sep = "")
    )
  }

  if(!is.na(x_lags)) {
    colnames <- c(
      colnames, 
      paste("x_", seq(0, x_lags, 1), sep = "")
    )
  }

  # If the parameter names are not defined, then equate them to the previously 
  # created column names
  browser()
  if(is.null(parameter_names)) {
    parameter_names <- colnames
  }

  # Check whether the parameter names have the same size as the parameter 
  # themselves
  if(length(parameter_names) != d) {
    stop(
      paste(
        "More or less values of the parameters provided compared to their names.",
        "Cannot make the mapping between means and their meaning within the model."
      )
    )
  }

  # Sample the required number of parameters and add them in a data.frame
  parameters <- matrix(0, nrow = N, ncol = length(colnames)) |>
    as.data.frame() |>
    setNames(colnames)
  
  parameters[, parameter_names] <- MASS::mvrnorm(
    N, 
    mean,
    covariances
  )

  # Divide and conquer: Divide up the parameters in their own groups. Makes the 
  # loop a bit less burdensome
  intercept <- parameters$intercept
  ar_params <- parameters[, grepl("y_", colnames, fixed = TRUE)]
  x_params <- parameters[, grepl("x_", colnames, fixed = TRUE)]

  # Once parameters have been simulated, we loop over the different parameters
  # and compute the system responses according to the new set of parameters
  samples <- lapply(
    seq_len(nrow(parameters)),
    function(i) {
      # Use irf_generator to generate the system responses according to this 
      # new set of parameters. Extract only the impulse responses
      responses <- irf_generator(
        intercept = as.numeric(intercept[i]),
        ar_params = as.numeric(ar_params[i, ]),
        x_params = as.numeric(x_params[i, ]),
        x = x,
        ...
      )
      responses <- responses$irf

      # Add a column indicating the sample and return the data.frame
      responses$sample <- i
      return(responses)
    }
  )
  samples <- do.call("rbind", samples)

  # Return the parameters and the bootstrapped samples
  return(
    list(
      "parameters" = parameters,
      "samples" = samples
    )
  )
}