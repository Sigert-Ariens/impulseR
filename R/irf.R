#' Generate impulse response
#' 
#' Create impulse response functions based on either a specific set of parameters
#' or a dataset and specified model.
#' 
#' @details
#' Method that allows for the estimation of the impulse response function. Two 
#' methods exist. First, you can base the impulse response functions on your own
#' a priori chosen set of parameters through the arguments \code{intercept}, 
#' \code{x_params}, and \code{ar_params}. In this case, you also need to provide
#' the impulses to the covariates through \code{"x"} and/or the impulses to the 
#' residuals through \code{"residuals"}.
#' 
#' Second, you can decompose your observed data into a part that describes the 
#' impulses from the covariates and/or a part that describes the impulses from
#' the residuals. To use this, you provide your data as a data.frame to the 
#' \code{object} argument and define the number of lags to use for the covariate
#' through \code{x_lags} (starting at lag 0, that is time t) and the number of 
#' lags for the dependent variable through \code{y_lags} (starting at lag 1, 
#' that is time t - 1). For example, to define an ADL(2, 1), you define 
#' \code{x_lags = 1, y_lags = 2}.
#' 
#' The code \code{NA} can be provided if you don't want to use a particular type 
#' of lag. For example, a linear regression is defined as 
#' \code{x_lags = 0, y_lags = NA}, while a lag-1 autoregressive model is defined 
#' as \code{x_lags = NA, y_lags = 1}.
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
#' #########################
#' # BASED ON DATA
#' 
#' # Generate data
#' set.seed(1)
#' data <- irf(
#'   intercept = 1, 
#'   x_params = c(1, 2, -0.5),
#'   ar_params = c(0.9, -0.1, 0.25),
#'   x = rnorm(100),
#'   residuals = rnorm(100)
#' )
#' 
#' # Decompose based on an ADL(2, 2)
#' irf(
#'   data, 
#'   cols = c("irf", "x"),
#'   x_lags = 2, 
#'   y_lags = 2
#' )
#' 
#' # Use your own impulses to evaluate this model
#' irf(
#'   data, 
#'   cols = c("irf", "x"),
#'   x_lags = 2, 
#'   y_lags = 2, 
#'   x = impulse(100),
#'   residuals = impulse(100)
#' )
#' 
#' 
#' 
#' #########################
#' # BASED ON PARAMETERS
#' 
#' # Create parameters of an ADL(2, 1), meaning having two lags in the residuals
#' # and one lag in the values of x. These will be used for all examples.
#' params <- list(
#'   "intercept" = 1, 
#'   "autoregression" = c(0.5, 0.1),
#'   "slopes" = c(2, 0.5)
#' )
#' 
#' # Use with single impulse of x in the beginning of the study, with burnin
#' irf(
#'   params$intercept, 
#'   params$autoregression,
#'   params$slopes,
#'   x = c(1, rep(0, 9))
#' )
#' 
#' # Use with single impulse of x in the beginning of the study, without burnin
#' irf(
#'   params$intercept, 
#'   params$autoregression,
#'   params$slopes,
#'   x = c(1, rep(0, 9)),
#'   burnin = FALSE
#' )
#' 
#' # Use with multiple values for the residuals, with burnin
#' irf(
#'   params$intercept, 
#'   params$autoregression,
#'   params$slopes,
#'   residuals = rnorm(10)
#' )
#' 
#' # Use with multiple values for the residuals, without burnin
#' irf(
#'   params$intercept, 
#'   params$autoregression,
#'   params$slopes,
#'   residuals = rnorm(10),
#'   burnin = FALSE
#' )
#' 
#' @rdname irf
#' 
#' @export
setGeneric("irf", function(object, ...) standardGeneric("irf"))

#' @param object Dataframe containing the variables of interest
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
#' 
#' @rdname irf
#' 
#' @export
setMethod("irf", signature(object = "data.frame"), function(object,
                                                            cols = c("y", "x"),
                                                            y_lags = NA, 
                                                            x_lags = NA,
                                                            x = NULL,
                                                            residuals = NULL) {
    
    # Estimate the model of the requested specifications
    params <- estimate(
      object, 
      cols = cols,
      y_lags = y_lags, 
      x_lags = x_lags
    )

    # Pass on to the other irf-method to do the remainder of the computations.
    # Differentiate between provided residuals and/or covariate impulses, or 
    # the ones that have been computed in the estimation function.
    if(is.null(x) & is.null(residuals)) {
      return(
        irf(
          intercept = params$intercept,
          x_params = params$x_params,
          ar_params = params$ar_params,
          x = params$x, 
          residuals = params$residuals,
          burnin = FALSE
        )
      )
    } else {
      return(
        irf(
          intercept = params$intercept, 
          x_params = params$x_params, 
          ar_params = params$ar_params,
          x = x, 
          residuals = residuals, 
          burnin = TRUE
        )
      )
    }
  }
)

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
#' @param x Numeric vector denoting the values of the exogenous variables 
#' X at each time t. These values serve as one type of impulses to the system to 
#' be simulated. Defaults to \code{NULL}, which means an empty vector of the same
#' length as \code{residuals} (if provided).
#' @param residuals Numeric vector denoting the values of the residuals at each 
#' time t. These values serve as one type of impulses to the system to be 
#' simulated. Defaults to \code{NULL}, which means an empty vector of the same
#' length as \code{x} (if provided).
#' @param burnin Logical denoting whether to use a burnin for the simulation. 
#' Recommended to be \code{TRUE} when you expect the initial conditions to lie 
#' far away from the mean of the process. Note that we recommend to set this 
#' argument to \code{FALSE} when you provide a vector of regression residuals
#' to the argument \code{residuals}. Defaults to \code{TRUE}.
#' 
#' @rdname irf
#' 
#' @export
setMethod("irf", signature(), function(intercept = 0, 
                                       ar_params = 0, 
                                       x_params = 0, 
                                       x = NULL, 
                                       residuals = NULL,
                                       burnin = TRUE){  
  
    # Check whether only a single intercept is provided.
    if(length(intercept) > 1) {
      warning("More than one intercept provided. Using the first value in this vector.")
      intercept <- intercept[1]
    }

    # Check whether x or residuals (or both) are provided. If not, then we have to 
    # throw an error. 
    #
    # If the one is null while the other is not, then we have to create the other
    # with all zeros.
    if(is.null(x) & is.null(residuals)) {
      stop("Neither `x` nor `residuals` is provided. Cannot proceed.")

    } else if(is.null(x) & !is.null(residuals)) {
      x <- numeric(length(residuals))

    } else if(!is.null(x) & is.null(residuals)) {
      residuals <- numeric(length(x))

    }

    # Determine the sample size (the number of values to generate) based on the 
    # values provided by x and residuals. Make the simulated length of the 
    # impulse response depend on the maximal length of these two.
    Nx <- length(x)
    Ne <- length(residuals)

    if(Nx < Ne) {
      x <- c(x, rep(0, Ne - Nx))
      warning("Length of `x` is smaller than length of `residuals`. Imputing zeros in `x`.")

    } else if(Nx > Ne) {
      residuals <- c(residuals, rep(0, Nx - Ne))
      warning("Length of `residuals` is smaller than length of `x`. Imputing zeros in `residuals`.")
    }

    # Check for NAs
    if(any(is.na(residuals)) | any(is.na(x))) {
      warning("NAs found in the provided `x` and/or `residuals`. Deleting them in a listwise fashion.")

      idx <- !is.na(residuals) & !is.na(x)
      residuals <- residuals[idx]
      x <- x[idx]
    }

    # After all these manipulations, you can proceed.
    N <- length(x)



    # Use Equation 15 to generate the impulse response parameters for the effect 
    # of the residuals epsilon. Specifically, these equations tell us that:
    #
    #   \theta_0 = 1
    #   \theta_1 = \phi_1
    #   ...
    #   \theta_i = \phi_1 \theta_{i - 1} + ... \phi_p \theta_{i - p}
    #   ...
    #
    # where \phi_j is the autoregressive effect for lag j.
    #
    # We create a value theta for each datapoint of the impulse response (length 
    # `N`), each retrieving its value through the values in `ar_params` and the 
    # lag `p`. To aid us in this, we first create a lower-triangular matrix 
    # containing the relevant autoregressive coefficients for each lag. Then, we 
    # loop over each of the datapoints and multiply with the previous values of 
    # \theta, giving us to value of \theta at the current time i.
    #
    # Create the autoregressive matrix
    p <- length(ar_params)
    phi <- rep(ar_params, each = p) |> 
      matrix(nrow = p, ncol = p)
    phi[upper.tri(phi)] <- 0

    # Initialize theta
    theta <- numeric(N)
    theta[1] <- 1

    # Loop over the datapoints and apply Equation 15 to get \theta.
    for(i in 2:N) {
      if(i - p < 1) {
        terms <- phi[(i - 1), 1:(i - 1)] %*% theta[(i - 1):1]
      } else {
        terms <- phi[p, ] %*% theta[(i - 1):(i - p)]
      }

      theta[i] <- sum(terms)
    }
    

    
    # Use Equation 19 to generate the impulse response parameters associated to 
    # the exogeneous variables x. Specifically, these equations tell us that:
    #
    #   \psi_0 = \theta_0 \beta_{L0}
    #   \psi_1 = \theta_1 \beta_{L0} + \theta_0 \beta_{L1}
    #   ...
    #   \psi_i = \theta_i \beta_{L0} + \theta_{i - 1} \beta_{L1} + ... \theta_{i - p} \beta_{LP}
    #   ...
    #
    # where Lj is used to denote lag j, and \beta is the slope of the exogeneous
    # variable at a given lag.
    #
    # We again create a value psi for each datapoint of the impulse response 
    # (length `N`), getting its values through the previously defined theta and 
    # `x_params`. We again use a matrix-like approach, avoiding a double for-loop.
    #
    # Create the slope matrix
    q <- length(x_params)
    beta <- rep(x_params, each = q) |> 
      matrix(nrow = q, ncol = q)
    beta[upper.tri(beta)] <- 0

    # Initialize psi and loop over the datapoints, applying Equation 19 to get 
    # \psi
    psi <- numeric(N)

    for(i in 1:N) {
      if(i - q < 1) {
        terms <- beta[i, 1:i] %*% theta[i:1]
      } else {
        terms <- beta[q, ] %*% theta[i:(i - q + 1)]
      }

      psi[i] <- sum(terms)
    }


    
    # Set up some of the output variables and do the one-sided convolutions of 
    # \psi with the values of x and of \theta with the values of the residuals 
    # \epsilon. We do this based on Equation 20, where:
    #
    #   irf_x = \sum \psi_k L^k x_t
    #   irf_\epsilon = \sum \theta_k L^k \epsilon_t
    #   irf_\text{intercept} = \sum \alpha \theta_k L^k 1_t
    #
    # It is these sums that we are computing here. Note that this is also equivalent
    # to Equation 25, which makes explicit use of the impulse response notation.
    irf_x <- irf_eps <- irf_intercept <- numeric(N)
    for(i in 1:N) {     
      irf_x[i] <- sum(psi[1:i] * x[i:1])
      irf_eps[i] <- sum(theta[1:i] * residuals[i:1])
      irf_intercept[i] <- sum(theta[1:i] * intercept)
    }

    # Use these sums to generate y_t, following Equation 26. First, decide what 
    # to do with the intercept based on the argument `burnin`. If `burnin = TRUE`, 
    # replace the previously computed sum with the long-time limit of the process, 
    # computed as:
    #
    #   \mu = \frac{\alpha}{1 - \sum \phi_i}
    #
    # Otherwise, you keep the sum as defined above.
    if(burnin){
      irf_intercept[] <- intercept / (1 - sum(ar_params))
    }
    
    y <- irf_x + irf_eps + irf_intercept

    # Prepare the output data: Add the time index, the individual impulse 
    # response functions cumulative responses, and the total output y_{t}
    output <- data.frame(
      "time" = 1:N - 1, 
      "irf" = y,
      "irf_intercept" = irf_intercept,
      "irf_x" = irf_x, 
      "irf_eps" = irf_eps, 
      "x" = x, 
      "residuals" = residuals    
    ) 
    
    return(output)  
  }
)

#' @rdname irf 
#' 
#' @export
setMethod("irf", signature(object = "numeric"), function(object, 
                                                         ar_params = 0, 
                                                         x_params = 0, 
                                                         x = NULL, 
                                                         residuals = NULL,
                                                         burnin = TRUE){  
  # Pass on to the non-signature variant
  return(
    irf(
      intercept = object, 
      ar_params = ar_params, 
      x_params = x_params, 
      x = x, 
      residuals = residuals, 
      burnin = burnin
    )
  )
})

