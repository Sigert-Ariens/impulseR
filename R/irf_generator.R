#' Core function to calculate system responses
#' 
#' #' @details 
#' This function calculates system responses for an ADL(p,q) model, formalized as:
#' 
#' \eqn{y_t = \alpha + \sum_{i = 1}^p \phi_{i}y_{t-i} + \beta_{x} x_{t} + \sum_{i = 1}^q \beta_{L^{j}x} x_{t - j} + v_{t}}.
#' 
#' @param intercept Numeric denoting the intercept, $\alpha$. Defaults to \code{0}
#' @param ar_params Numeric vector denoting the autoregressive parameters (the $\phi$) parameters of the model.
#' Parameters need to be given in order of increasing lags (i.e., first element for $\phi_{1}$, second element for $\phi_{2}$,...).
#' Defaults to \code{0}
#' @param x_params Numeric vector denoting the covariate parameters of the model (the $\beta$ parameters). 
#' Parameters again need to be given in order of increasing lags(i.e., first element for $\beta_{x}$, second element for $\beta_{Lx}$,...). Defaults to 
#' \code{0}, indicating that there are no covariate parameters. 
#' @param x Numeric vector denoting the values of covariate at each time point. 
#' Depending on the input vectors supplied, different system responses will be returned (see vignette X). 
#' Defaults to \code{NULL}, which returns an empty vector of the same
#' length as \code{innovations} (if provided).
#' @param innovations Numeric vector denoting the values of the innovations at each 
#' time point. Depending on the input vectors supplied, different system responses will be returned (see vignette X).
#' Defaults to \code{NULL}, which returns an empty vector of the same
#' length as \code{x} (if provided).
#' @param burnin Logical (True/False) specifying if the part of the system response due to the intercept should be burned in analytically. Defaults to \code{TRUE}, the system responses 
#' responses will be displayed relative to the hypothetical equilibrium state of the system. If set to \code{FALSE}, initial value dependent behavior will be present. 
#' 
#' @return A dataframe containing the system responses. Within the dataframe, 
#' column \code{"time"} contains the time index starting at 0. The columns 
#' \code{"irf_intercept"}, \code{"irf_x"}, \code{"irf_v"} contain the model implied
#' system responses for the intercept, the covariate, and the innovations, respectively. 
#' The total response is also provided under the column \code{"y"}. 
#' Finally, the columns \code{"x"} and \code{"innovations"} contain 
#' the provided input vectors.
#' 
#' 
#' @rdname irf_generator
#' 
#' @export
irf_generator <- function(intercept = 0, 
                          ar_params = 0, 
                          x_params = 0,
                          x = NULL,
                          innovations = NULL,
                          burnin = TRUE) {
  
  
  # Check whether only a single intercept is provided.
  if(length(intercept) > 1) {
    warning("More than one intercept provided. Using the first value in this vector.")
    intercept <- intercept[1]
  }
  
  # Check whether x or innovations (or both) are provided. If not, then we have to 
  # throw an error. 
  #
  # If the one is null while the other is not, then we have to create the other
  # with all zeros.
  if(is.null(x) & is.null(innovations)) {
    stop("Neither `x` nor `innovations` is provided. Cannot proceed.")
    
  } else if(is.null(x) & !is.null(innovations)) {
    x <- numeric(length(innovations))
    
  } else if(!is.null(x) & is.null(innovations)) {
    innovations <- numeric(length(x))
    
  }
  
  # Determine the sample size (the number of values to generate) based on the 
  # values provided by x and innovations. Make the simulated length of the 
  # system response is given by the maximal length of these two.
  Nx <- length(x)
  Nv <- length(innovations)
  
  if(Nx < Nv) {
    x <- c(x, rep(0, Nv - Nx))
    warning("Length of `x` is smaller than length of `innovations`. Imputing zeros in `x`.")
    
  } else if(Nx > Nv) {
    innovations <- c(innovations, rep(0, Nx - Nv))
    warning("Length of `innovations` is smaller than length of `x`. Imputing zeros in `innovations`.")
  }
  
  # Check for NAs
  if(any(is.na(innovations)) | any(is.na(x))) {
    warning("NAs found in the provided `x` and/or `innovations`. Deleting them in a listwise fashion.")
    
    idx <- !is.na(innovations) & !is.na(x)
    innovations <- innovations[idx]
    x <- x[idx]
  }

  # Finally, check if the roots of $\phi(L)$ are all outside the complex unit circle. If so, provide a warning. 
  
  phiL <- c(1,-ar_params)
  
  mod <- abs(polyroot(phiL)) # Calculate the moduli of the roots
  
  if(min(mod)<1){
    warning("The AR parameters supplied imply a nonstationary process, some of the roots of phi(L) are inside the complex unit circle")
  }
  
  # After all these manipulations, you can proceed with the algebra.
  nt <- length(x)
  
  
  
  # Use Equation 26 to generate the impulse response function for the innovations
  #
  # Create a matrix to avoid double for loops
  p <- length(ar_params)
  phi <- rep(ar_params, each = p) |> 
    matrix(nrow = p, ncol = p)
  phi[upper.tri(phi)] <- 0
  
  # Initialize theta
  theta <- numeric(nt)
  theta[1] <- 1
  
  # Loop over the datapoints and apply Equation 26 to get \theta.
  for(k in 2:nt) {
    if(k - p < 1) {
      terms <- phi[(k - 1), 1:(k - 1)] %*% theta[(k - 1):1]
    } else {
      terms <- phi[p, ] %*% theta[(k - 1):(k - p)]
    }
    
    theta[k] <- sum(terms)
  }
  
  
  
  # Use Equations 30 and 33 to generate the impulse response function towards the covariate
  #
  # Create a matrix to avoid double for loops
  q <- length(x_params)
  beta <- rep(x_params, each = q) |> 
    matrix(nrow = q, ncol = q)
  beta[upper.tri(beta)] <- 0
  
  # Initialize psi and loop over the data, applying Equation 33 to get 
  # \psi
  psi <- numeric(nt)
  
  for(k in 1:nt) {
    if(k - q < 1) {
      terms <- beta[k, 1:k] %*% theta[k:1]
    } else {
      terms <- beta[q, ] %*% theta[k:(k - q + 1)]
    }
    
    psi[k] <- sum(terms)
  }
  
  
  
  # Set up some of the output variables and do the one-sided convolutions of 
  # \psi with the values of x and of \theta with the values of the innovations 
  # \epsilon. We do this based on Equation 20, where:
  #
  #   irf_x = \sum \psi_k L^k x_t
  #   irf_v= \sum \theta_k L^k v_t
  #   irf_\text{intercept} = \sum \alpha \theta_k L^k 1_t
  #
  # It is these sums that we are computing here. Note that this is also equivalent
  # to Equation 38, which makes explicit use of the impulse response notation.
  irf_x <- irf_v <- irf_intercept <- numeric(nt)
  for(t in 1:nt) {     
    irf_x[t] <- sum(psi[1:t] * x[t:1])
    irf_v[t] <- sum(theta[1:t] * innovations[t:1])
    irf_intercept[t] <- sum(theta[1:t] * intercept)
  }
  
  # Use these sums to generate y_t, following Equation 38. First, decide what 
  # to do with the intercept based on the argument `burnin`. If `burnin = TRUE`, 
  # replace the previously computed sum with the long-time limit of the process, 
  # computed as:
  #
  #   \frac{\alpha}{1 - \sum \phi_i}
  #
  # Otherwise, you keep the sum as defined above.
  if(burnin){
    irf_intercept[] <- intercept / (1 - sum(ar_params))
  }
  
  y <- irf_x + irf_v + irf_intercept
  
  # Prepare the output data: Add the time index, the individual impulse 
  # response functions cumulative responses, and the total output y_{t}
  output <- data.frame(
    "time" = 1:nt - 1, 
    "irf" = y,
    "irf_intercept" = irf_intercept,
    "irf_x" = irf_x, 
    "irf_v" = irf_v, 
    "x" = x, 
    "innovations" = innovations    
  ) 
  
  return(output)
    
}

