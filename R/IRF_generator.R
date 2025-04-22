#' Generate impulse response
#' 
#' For the impulses provided to \code{Xvals} and \code{Epsvals}, we distinguish 
#' between two types, namely singular and cumulative impulses. For the singular 
#' impulses, we recommend providing vectors of the form \code{c(1, 0, 0,...)} as
#' input to one or both of the arguments. For cumulative impulses, you can 
#' provide an arbitrary vector.
#' 
#' @param intercept Numeric denoting the intercept to use for the impulse response 
#' function.
#' @param ARparams Numeric vector denoting the autoregressive parameters to be
#' used for the impulse response function. Parameters need to be given in order
#' of increased lag (i.e., first element for t - 1, second element for t - 2,...)
#' @param Xparams Numeric vector denoting the values of the slopes for the 
#' exogenous variables. Parameters again need to be given in order of increased
#' lag (i.e., first element for t, second element for t - 1,...)
#' @param Xvals Numeric vector denoting the values of the exogenous variables 
#' X at each time t. These values serve as one type of impulses to the system to 
#' be simulated.
#' @param Epsvals Numeric vector denoting the values of the residuals at each 
#' time t. These values serve as one type of impulses to the system to be 
#' simulated.
#' @param nt Integer denoting the number of time steps to simulate.
#' @param Burnin Logical denoting whether to use a burnin for the simulation. 
#' Recommended to be \code{TRUE} when you expect the initial conditions to lie 
#' far away from the mean of the process. Note that we recommend to set this 
#' argument to \code{FALSE} when you provide a vector of regression residuals
#' to the argument \code{Epsvals}.
#' 
#' @example 
#' 
#' @export
# 
# TO DO:
#   - Merge nt with Xvals and/or Epsvals: Should all have the same length
#   - Merge the different parameters?
#   - Provide defaults that are informative
#   - Can't we just provide initial conditions that are close to the mean, thus
#     not requiring a burnin? => NOTE: Apparently, this is exactly what it does
#   - Provide comprehensive documentation + small rewrite of variable names
IRF_generator <- function(intercept, 
                          ARparams, 
                          Xparams, 
                          Xvals, 
                          Epsvals,
                          Burnin){  
  
  # Determine the sample size (the number of values to generate) based on the 
  # values provided by Xvals and Epsvals. Make the simulated length of the 
  # impulse response depend on the maximal length of these two.
  Nx <- length(Xvals)
  Ne <- length(Epsvals)

  if(Nx < Ne) {
    Xvals <- c(Xvals, rep(0, Ne - Nx))
    warning("Length of Xvals is smaller than length of Epsvals. Imputing zeros in Xvals.")

  } else if(Nx > Ne) {
    Epsvals <- c(Epsvals, rep(0, Nx - Ne))
    warning("Length of Epsvals is smaller than length of Xvals. Imputing zeros in Epsvals.")
  }

  N <- length(Xvals)

  # Use Equation 15 to generate the impulse response parameters for the effect 
  # of the residuals epsilon. Specifically, these equations tell us that:
  #
  #   \theta_0 = 1
  #   \theta_1 = \phi_1
  #   ...
  #   \theta_i = \phi_1 \theta_{i - 1} + ... \phi_p \theta_{i - p}
  #   ...
  #
  # We create a value theta for each datapoint of the impulse response (length 
  # `N`), each retrieving its value through the values in `ARparams` and the 
  # lag `p`. To aid us in this, we first create a lower-triangular matrix 
  # containing the relevant autoregressive coefficients for each lag. Then, we 
  # loop over each of the datapoints and multiply with the previous values of 
  # \theta, giving us to value of \theta at the current time i.
  #
  # Create the autoregressive matrix
  p <- length(ARparams)
  phi <- rep(ARparams, each = p) %>% 
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
  
  
  ### Generate psi coefficients 
  
  q <- length(Xparams)  # Length of Xparams
  
  # Initialize Psi
  Psi <- rep(0, N)

  # Start from h_{x}(0)
  Psi[1] <- Xparams[1]
  
  
  # Derive the rest using the recurrence relation (eqs 19 & 23)
  for (i in 2:N) {
    Psi[i] <- 0  
    for (j in 1:q) {
      if (i - (j - 1) >= 1) {  
        # TO DO: Can't we do the same in a vectorized way?
        Psi[i] <- Psi[i] + Xparams[j] * theta[i - (j - 1)]
      }
    }
  }
  
  ### Provide implied equillibrium of intercept response (lim t \to \infty \sum_{s=0}^{t}h_{1}§(s)1_{t-s})
  
  limintercept <- intercept/(1-(sum(ARparams)))
  
  ## Set up output variables 
  
  IRF_x <- c()
  IRFeps <- c()
  IRFint_raw <- c()
  
  
  for (t in 1:N) { # equation 22
    
    IRF_x[t] <- sum(Psi[1:t]*Xvals[t:1])  # One sided convolution
    IRFeps[t] <- sum(theta[1:t]*Epsvals[t:1]) # One sided convolution
    IRFint_raw[t] <- sum(theta[1:t]*intercept)
  }
  
  time <- seq(1:N)
  time <- time-1
  
  ### Generate y_{t}, equation 26
  
  ### Decide what to do with intercept. If Burnin == True, replace sum(theta[1:t]*intercept) with intercept/(1-(sum(ARparams)))
  
  if(Burnin == TRUE){
    IRFintercept = limintercept
  }else{
    IRFintercept = IRFint_raw
  }
  
  IRFtot <- IRF_x + IRFeps + IRFintercept

  # Prepare the output data: Add the time index, the individual impulse response functions cumulative responses, and the total output y_{t}
  output <- data.frame(
    "time" = time, 
    "IRFx" = IRF_x, 
    "IRFe" = IRFeps, 
    "X" = Xvals, 
    "Eps" = Epsvals, 
    "IRFtotal" = IRFtot,
    "IRFintercept" = IRFintercept
  )
  
  
  return(output)
  
}

