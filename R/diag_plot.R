#' Diagnostic plots for \eqn{ADL(p, q)}
#' 
#' Diagnostic plots meant to provide information on the fit of the model to 
#' data. Consist of (a) residuals plotted against fitted values, (b) 
#' autocorrelation plots for the residuals, and (c) a time-series plot of the 
#' residuals.
#' 
#' @param fit Fit object as provided by the output of 
#' \code{\link[impulseR]{irf_empirical}} under the label \code{"fit"}
#' 
#' @return Plots visualizing several indices of fit
#' 
#' @examples 
#' # Generate data to be evaluated
#' x <- rnorm(100)
#' y <- 1 + 2 * x - x^2
#' 
#' # Perform a linear regression analysis
#' fit <- lm(y ~ x)
#' 
#' # Create diagnostic plots
#' diag_plot(fit)
#' 
#' @export
diag_plot <- function(fit){
  # Check whether anything is defined
  if(is.null(fit)) {
    stop("No fit object is provided to the function.")
  }

  # Check whether the object is one stemming from lm. 
  #
  # NOTE: May need to change if we are going to go multidimensional and/or 
  # maximum-likelihood
  if (!inherits(fit, "lm")) {
    stop("Input must be an object of class 'lm'")
  }
  
  # Extract values needed for the plots
  res <- residuals(fit)
  fitted_vals <- fitted(fit)
  
  # Save old graphical parameters and restore on exit
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  
  # Arrange plots: 3 rows, 1 column
  par(
    mfrow = c(3, 1), 
    mar = c(4, 4, 3, 1)
  )
  
  # 1) Residuals vs Fitted values
  plot(
    fitted_vals, 
    res,
    xlab = "Fitted values",
    ylab = "Residuals",
    main = "Residuals vs Fitted"
  )
  abline(h = 0, col = "red", lty = 2)
  
  # 2) ACF plot of residuals
  acf(
    res,
    main = "ACF of Residuals"
  )
  
  # 3) Time series plot of residuals
  plot(
    res,
    type = "l",
    xlab = "Time / Index",
    ylab = "Residuals",
    main = "Time Series of Residuals"
  )
  abline(h = 0, col = "red", lty = 2)

  return(NULL)
}