#' Estimate a one-dimensional ADL
#' 
#' Estimate the parameters of an $ADL(p, k)$, where $p$ represents the number of
#' lags in the dependent variable $y$ and $k$ represents the number of lags in 
#' the covariate $x$. Relating this to the mathematics, we get:
#' 
#' \eqn{y_t = \alpha + \sum_{i = 0}^k \beta_i x_{t - i} + \sum_{i = 1}^p \gamma_i y_{t - i} + \epsilon_t}
#' 
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
#' @return Named list containing the estimated model (\code{model}), the 
#' parameters that were estimated as used by the \code{\link[impulseR]{irf}} 
#' function (\code{intercept}, \code{x_params}, \code{ar_params}), and the 
#' impulses obtained from the data (\code{x}, \code{residuals})
#' 
#' @examples 
#' # Define a dataset
#' x <- rnorm(100)
#' data <- data.frame(
#'   DV = 1 + 2 * x + rnorm(100),
#'   IV = x
#' )
#' 
#' # Estimate an ADL(2, 2)
#' estimate(
#'   data, 
#'   cols = c("DV", "IV"),
#'   y_lags = 2, 
#'   x_lags = 2
#' )
#' 
#' @rdname estimate 
#' 
#' @export
estimate <- function(object,
                     cols = c("y", "x"),
                     y_lags = NULL, 
                     x_lags = NULL) {

  # Check whether the columns specified can be found in the dataset
  if(!all(cols %in% colnames(object))) {
    stop("Columns specified in `cols` cannot be found in the supplie dataframe.")
  }

  object <- object[, cols] |>
    `colnames<-` (c("y", "x"))

  # Check whether any lags have been provided. If not, then we have to estimate
  # an empty model, that is one in which only the mean is estimated and all other
  # parameters are 0. This is the immediate result.
  y_lags <- ifelse(is.null(y_lags), NA, y_lags)
  x_lags <- ifelse(is.null(x_lags), NA, x_lags)
  if(is.na(y_lags) & is.na(x_lags)) {
    return(
      list(
        "model" = list(),
        "intercept" = mean(object$y),
        "x_params" = 0,
        "ar_params" = 0,
        "x" = object$x,
        "residuals" = object$y - mean(object$y)
      )
    )
  }



  # Use the lm-function to estimate the parameters of the model. Before doing 
  # this, we create a matrix containing all of the different lagged variables, 
  # which can be supplied to lm
  N <- nrow(object)

  if(!is.na(y_lags)) {
    X1 <- matrix(
      NA, 
      nrow = N,
      ncol = y_lags
    ) 

    for(i in 1:y_lags) {
      idx <- 1:(N - i)
      X1[idx + i, i] <- object$y[idx]
    }

    cols1 <- paste0("ylag_", 1:y_lags)
  }

  if(!is.na(x_lags)) {
    X2 <- matrix(
      NA, 
      nrow = N,
      ncol = x_lags + 1
    ) 

    for(i in 0:x_lags) {
      idx <- 1:(N - i)
      X2[idx + i, i + 1] <- object$x[idx]
    }

    cols2 <- paste0("xlag_", 0:x_lags)
  }

  if(exists("X1") & exists("X2")) {
    X__ <- cbind(X1, X2) |>
      `colnames<-` (c(cols1, cols2))
  } else if(exists("X1")) {
    X__ <- X1 |>
      `colnames<-` (cols1)
  } else {
    X__ <- X2 |>
      `colnames<-` (cols2)
  }

  # Delete NA-values out of the dataset and do the actual analysis
  idx <- !is.na(rowSums(X__))

  y <- object$y[idx]
  X__ <- X__[idx,]

  results <- lm(y ~ X__)



  # Extract all of the parameters and put them in a format that our package uses
  # under the hood
  coefs <- summary(results)$coefficients[, 1]
  
  intercept <- coefs[1]

  if(!is.na(y_lags) & !is.na(x_lags)) {
    ar_params <- coefs[2:(1 + y_lags)]
    x_params <- coefs[(2 + y_lags):length(coefs)]

  } else if(!is.na(y_lags)) {
    ar_params <- coefs[2:(1 + y_lags)]
    x_params <- 0
    
  } else if(!is.na(x_lags)) {
    ar_params <- 0
    x_params <- coefs[2:(2 + x_lags)]

  }



  # Compute the residuals with which the researcher can reproduce their data and 
  # examine which components matter most. Basically, this is a correction for 
  # the impulse response so that the observed data can be used as initial 
  # conditions starting from the correct lag.
  #
  # Parameters are vectorized so that you don't have to think about it too much.
  residuals <- as.numeric(results$residuals)
  n_inx <- N - length(residuals)

  # Adding to the residuals is only needed whenever we're having lagged effects,
  # otherwise we don't need it.
  if(n_inx != 0) {
    y0 <- inx <- object$y[1:n_inx]
    x0 <- object$x[1:n_inx]

    if(!is.na(y_lags)) {
      phi <- matrix(
        rep(ar_params, each = y_lags), 
        nrow = y_lags,
        ncol = length(ar_params)
      )
      phi[upper.tri(phi, diag = FALSE)] <- 0
    }

    if(!is.na(x_lags)) {
      beta <- matrix(
        rep(x_params, each = x_lags + 1),
        nrow = x_lags + 1,
        ncol = length(x_params)
      )
      beta[upper.tri(beta, diag = FALSE)] <- 0
    }
    
    for(i in seq_along(inx)) {
      # Effect of the residuals. Note the reversal of the input (y0) compared to 
      # the parameters. This ensures that the most recent value of y0 is linked 
      # with the correct phi (and all the way down to each lag)
      if(!is.na(y_lags)) {
        if(i == 1) {
          inx[i] <- inx[i] - 0
        } else if(i <= y_lags) {
          inx[i] <- inx[i] - sum(phi[i - 1, 1:(i - 1)] * y0[(i - 1):1])
        } else {
          inx[i] <- inx[i] - sum(phi[nrow(phi), ] * y0[(i - 1):(i - y_lags)])
        }
      }

      # Effect of the covariates. Note the reversal of the input (x0) compared to 
      # the parameters. This ensures that the most recent value of x0 is linked 
      # with the correct beta (and all the way down to each lag)
      #
      # Furthermore note that because you include lag 0, beta and x0 should have 
      # an additional value for each lag
      if(!is.na(x_lags)) {
        if(i <= x_lags) {
          inx[i] <- inx[i] - sum(beta[i, 1:i] * x0[i:1])
        } else {
          inx[i] <- inx[i] - sum(beta[nrow(beta), ] * x0[i:(i - x_lags)])
        }
      }

      # Effect of the intercept
      inx[i] <- inx[i] - intercept
    }

    residuals <- c(inx, residuals)
  }
  
  
  
  # Now that this has been done, return a list containing all of this information
  return(
    list(
      "model" = results,
      "intercept" = intercept,
      "ar_params" = ar_params,
      "x_params" = x_params,
      "x" = object$x, 
      "residuals" = residuals
    )
  )
}