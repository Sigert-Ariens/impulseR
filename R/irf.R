#' Generate impulse response
#' 
#' For the impulses provided to \code{x} and \code{residuals}, we distinguish 
#' between two types, namely singular and cumulative impulses. For the singular 
#' impulses, we recommend providing vectors of the form \code{c(1, 0, 0,...)} as
#' input to one or both of the arguments. For cumulative impulses, you can 
#' provide an arbitrary vector.
#' 
#' @param intercept Numeric denoting the intercept to use for the impulse response 
#' function.
#' @param ar_params Numeric vector denoting the autoregressive parameters to be
#' used for the impulse response function. Parameters need to be given in order
#' of increased lag (i.e., first element for t - 1, second element for t - 2,...)
#' @param x_params Numeric vector denoting the values of the slopes for the 
#' exogenous variables. Parameters again need to be given in order of increased
#' lag (i.e., first element for t, second element for t - 1,...)
#' @param x Numeric vector denoting the values of the exogenous variables 
#' X at each time t. These values serve as one type of impulses to the system to 
#' be simulated.
#' @param residuals Numeric vector denoting the values of the residuals at each 
#' time t. These values serve as one type of impulses to the system to be 
#' simulated.
#' @param burnin Logical denoting whether to use a burnin for the simulation. 
#' Recommended to be \code{TRUE} when you expect the initial conditions to lie 
#' far away from the mean of the process. Note that we recommend to set this 
#' argument to \code{FALSE} when you provide a vector of regression residuals
#' to the argument \code{residuals}. Defaults to \code{TRUE}.
#' 
#' @example 
#' 
#' @export
irf <- function(intercept, 
                ar_params, 
                x_params, 
                x, 
                residuals,
                burnin = TRUE){  
  
  # Determine the sample size (the number of values to generate) based on the 
  # values provided by x and residuals. Make the simulated length of the 
  # impulse response depend on the maximal length of these two.
  Nx <- length(x)
  Ne <- length(residuals)

  if(Nx < Ne) {
    x <- c(x, rep(0, Ne - Nx))
    warning("Length of x is smaller than length of residuals. Imputing zeros in x.")

  } else if(Nx > Ne) {
    residuals <- c(residuals, rep(0, Nx - Ne))
    warning("Length of residuals is smaller than length of x. Imputing zeros in residuals.")
  }

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
  phi <- rep(ar_params, each = p) %>% 
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
  beta <- rep(x_params, each = q) %>% 
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

  # Prepare the output data: Add the time index, the individual impulse response functions cumulative responses, and the total output y_{t}
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

