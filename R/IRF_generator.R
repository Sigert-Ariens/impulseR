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
                          nt, 
                          Burnin){  
  
  ### Generate coefficients of (1-\sum_{i=1}^{k}phi_{k}L^{k})^{-1} (equations 15 & 19)
  
  
  p <- length(ARparams)  # Length of AR parameters
  
  # Initialize theta coefficients
  theta <- rep(0, nt)  
  
  # Manually set the initial conditions 
  theta[1] <- 1  
  theta[2] <- ARparams[1]  
  
  # Equations 15 & 19
  for (i in 3:nt) {
    theta[i] <- 0  # Initialize ARcoefs[i] to 0 before summing
    
    for (j in 1:p) {
      if (i - j >= 1) {  
        # TO DO: Can't we do the same in a vectorized way?
        theta[i] <- theta[i] + ARparams[j] * theta[i - j]
      }
    }
  }
  
  
  ### Generate psi coefficients 
  
  q <- length(Xparams)  # Length of Xparams
  
  # Initialize Psi
  Psi <- rep(0, nt)

  # Start from h_{x}(0)
  Psi[1] <- Xparams[1]
  
  
  # Derive the rest using the recurrence relation (eqs 19 & 23)
  for (i in 2:nt) {
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
  
  
  for (t in 1:nt) { # equation 22
    
    IRF_x[t] <- sum(Psi[1:t]*Xvals[t:1])  # One sided convolution
    IRFeps[t] <- sum(theta[1:t]*Epsvals[t:1]) # One sided convolution
    IRFint_raw[t] <- sum(theta[1:t]*intercept)
  }
  
  time <- seq(1:nt)
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

