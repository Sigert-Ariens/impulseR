diag_plot <- function(fit){
  # Basic checks
  if (!inherits(fit, "lm")) {
    stop("Input must be an object of class 'lm'")
  }
  
  # Extract values
  res <- residuals(fit)
  fitted_vals <- fitted(fit)
  
  # Save old graphical parameters and restore on exit
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  
  # Arrange plots: 3 rows, 1 column
  par(mfrow = c(3, 1), mar = c(4, 4, 3, 1))
  
  # 1) Residuals vs Fitted values
  plot(
    fitted_vals, res,
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
}