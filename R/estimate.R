#' Estimate a one-dimensional ADL
#' 
#' @details
#' Estimate the parameters of an $ADL(p, k)$, where $p$ represents the number of
#' lags in the dependent variable $y$ and $k$ represents the number of lags in 
#' the covariate $x$. Relating this to the mathematics, we get:
#' 
#' \eqn{y_t = \alpha + \sum_{i = 0}^k \beta_i x_{t - i} + \sum_{i = 1}^p \gamma_i y_{t - i} + \epsilon_t}
#' 
#' @param data Dataframe containing the variables of interest
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
estimate <- function(data,
                     cols = c("y", "x"),
                     y_lags = NULL, 
                     x_lags = NULL) {

  # Check whether only a single column is provided in the data.frame, and whether
  # the person specified no use of x_lags. In this case, we make a pass and allow
  # the user to continue (otherwise, they will get an error)
  if((ncol(data) == 1) & is.null(x_lags)) {
    colnames(data) <- "y"
    data$x <- numeric(nrow(data))
    cols <- c("y", "x")

  } else if(ncol(data) == 1) {
    stop("Only a single column provided, but lags in the covariate specified. Please provide a data.frame with at least two columns.")
  }

  # Check whether the columns specified can be found in the dataset
  if(!all(cols %in% colnames(data))) {
    stop("Columns specified in `cols` cannot be found in the supplied dataframe.")
  }

  # Check for NAs
  if(any(is.na(data))) {
    warning("NAs found in the data. Deleting them in a listwise fashion.")
    
    idx <- !is.na(rowSums(data))
    data <- data[idx, ]
  }

  # Check for impossible lags
  y_lags <- ifelse(is.null(y_lags), NA, y_lags)
  x_lags <- ifelse(is.null(x_lags), NA, x_lags)

  if(!is.na(x_lags)) {
    if(x_lags < 0) {
      warning("Specified lags in `x` are below its minimal value (0). Assuming no involvement of the covariate.")
      x_lags <- NA
    }
  }

  if(!is.na(y_lags)) {
    if(y_lags < 1) {
      warning("Specified lags in `y` are below its minimal value (1). Assuming no autoregression.")
      y_lags <- NA
    }
  }

  # Change column names for easier handling
  data <- data[, cols] |>
    `colnames<-` (c("y", "x"))

  # Check whether any lags have been provided. If not, then we have to estimate
  # an empty model, that is one in which only the mean is estimated and all other
  # parameters are 0. This is the immediate result.
  if(is.na(y_lags) & is.na(x_lags)) {
    return(
      list(
        "model" = list(),
        "intercept" = mean(data$y),
        "x_params" = 0,
        "ar_params" = 0,
        "x" = data$x,
        "residuals" = data$y - mean(data$y)
      )
    )
  }



  # Use the lm-function to estimate the parameters of the model. Before doing 
  # this, we create a matrix containing all of the different lagged variables, 
  # which can be supplied to lm.
  #
  # Before applying lm, filter out the NA-values
  X__ <- prepare_data(
    data, 
    cols = c("y", "x"),
    x_lags = x_lags,
    y_lags = y_lags
  )
  idx <- !is.na(rowSums(X__))

  y <- data$y[idx]
  X__ <- X__[idx,]

  results <- lm(y ~ X__)



  # Extract all of the parameters and put them in a format that our package uses
  # under the hood
  coefs <- summary(results)$coefficients[, 1] |>
    as.numeric() |>
    `names<-` (NULL)
  
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
  
  
  
  # Now that this has been done, return a list containing all of this information
  return(
    list(
      "model" = results,
      "intercept" = intercept,
      "ar_params" = ar_params,
      "x_params" = x_params,
      "x" = data$x, 
      "residuals" = compute_residuals(
        data, 
        cols = c("y", "x"),
        intercept = intercept,
        ar_params = ar_params,
        x_params = x_params, 
        residuals = results$residuals
      )
    )
  )
}

#' Compute residuals of a lagged model
#' 
#' @details
#' In lagged models, residuals are only defined for \code{N - lags} datapoints, 
#' as the value of the predicted y-values depends on a set of initial conditions.
#' This function computes the residuals for all \code{N} datapoints, thus 
#' including the initial conditions in its output. 
#' 
#' Used under the hood in the \code{\link[impulseR]{estimate}} function. 
#' 
#' @param data Dataframe containing the variables of interest
#' @param cols Character vector denoting the columns containing the variables of
#' interest. First character should denote the dependent variable, the second 
#' one the covariate of interest. Defaults to \code{c("y", "x")}
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
#' @param residuals Numeric vector denoting the values of already known values 
#' of the residuals at some time t (e.g., those acquired through the \code{lm} 
#' function). Defaults to \code{NULL}, signalling the creation of a new vector 
#' of residuals based on the parameters that were provided to the function.
#' 
#' @return Numeric vector containing the residuals at a particular time t.
#' 
#' @examples 
#' # Define a dataset
#' x <- rnorm(100)
#' data <- data.frame(
#'   DV = 1 + 2 * x + rnorm(100),
#'   IV = x
#' )
#' 
#' # For these data, compute the residuals of an ADL(2, 2)
#' residuals <- compute_residuals(
#'   data, 
#'   cols = c("DV", "IV"),
#'   intercept = 0.5, 
#'   ar_params = c(0.75, 0.25),
#'   x_params = c(1.5, 0.25)
#' )
#' 
#' @export 
compute_residuals <- function(data, 
                              cols = c("y", "x"),
                              intercept = 0, 
                              ar_params = 0, 
                              x_params = 0, 
                              residuals = NULL) {

  # Define the lags in x and y based on the parameters that are provided.
  x_lags <- ifelse(
    (x_params[1] != 0),
    length(x_params) - 1,
    NA
  )
  y_lags <- ifelse(
    (ar_params[1] != 0),
    length(ar_params),
    NA
  )

  # Check whether only a single column is provided in the data.frame, and whether
  # the person specified no use of x_lags. In this case, we make a pass and allow
  # the user to continue (otherwise, they will get an error)
  if((ncol(data) == 1) & is.na(x_lags)) {
    colnames(data) <- "y"
    data$x <- numeric(nrow(data))
    cols <- c("y", "x")

  } else if(ncol(data) == 1) {
    stop("Only a single column provided, but lags in the covariate specified. Please provide a data.frame with at least two columns.")
  }

  # Check whether columns are contained in the data
  if(!all(cols %in% colnames(data))) {
    stop("Columns specified in `cols` cannot be found in the supplied dataframe.")
  }

  # Check for NAs
  if(any(is.na(data))) {
    warning("NAs found in the data. Deleting them in a listwise fashion.")
    
    idx <- !is.na(rowSums(data))
    data <- data[idx, ]
  }
  
  # Change column names for easier handling
  data <- data[, cols] |>
    `colnames<-` (c("y", "x"))

  # Compute the number of datapoints
  N <- nrow(data)

  # Compute the residuals that are defined by the model. Do this through 
  # subtracting actual data from predictions based on the model.
  #
  # Only perform this computation if residuals are not defined yet.
  if(is.null(residuals)) {
    # Prepare the data and parameters for the prediction step.
    X <- prepare_data(
      data, 
      cols = c("y", "x"),
      x_lags = x_lags,
      y_lags = y_lags
    )

    if(is.na(x_lags) & is.na(y_lags)) {
      # If no lags are defined, then our best guess of y_hat is the intercept.
      # Therefore, the residuals are defined as y - intercept
      residuals <- data$y - intercept

    } else {
      # Otherwise, we can differentiate between several cases
      if(!is.na(x_lags) & !is.na(y_lags)) {
        params <- c(ar_params, x_params)

      } else if(!is.na(x_lags)) {
        params <- x_params

      } else if(!is.na(y_lags)) {
        params <- ar_params

      } 

      # Delete NAs from X
      idx <- !is.na(rowSums(X))
      X <- X[idx, , drop = FALSE]

      # Compute the residuals based on the predicted values for y.
      y_hat <- intercept + X %*% params
      residuals <- data$y[(N - nrow(X) + 1):N] - y_hat
    }    
  }

  # Compute the residuals with which the researcher can reproduce their data and 
  # examine which components matter most. Basically, this is a correction for 
  # the impulse response so that the observed data can be used as initial 
  # conditions starting from the correct lag.
  #
  # Parameters are vectorized so that you don't have to think about it too much.
  n_inx <- N - length(residuals)

  # Adding to the residuals is only needed whenever we're having lagged effects,
  # otherwise we don't need it.
  if(n_inx != 0) {
    y0 <- inx <- data$y[1:n_inx]
    x0 <- data$x[1:n_inx]

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

  return(residuals)
}

#' Prepare data for analysis
#' 
#' @details
#' Transforms a data.frame in such a way that we can estimate the parameters 
#' for the specified number of lags in y and x.
#' 
#' Used under the hood in the \code{\link[impulseR]{estimate}} function. 
#' 
#' @param data Dataframe containing the variables of interest
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
#' @return Numeric matrix containing the values of the independent and dependent
#' variables that are used as predictors in the estimation routine.
#' 
#' @examples 
#' # Define a dataset
#' x <- rnorm(100)
#' data <- data.frame(
#'   DV = 1 + 2 * x + rnorm(100),
#'   IV = x
#' )
#' 
#' # For these data, prepare the data for estimation for an ADL(2, 2)
#' prepared_data <- prepared_data(
#'   data, 
#'   cols = c("DV", "IV"),
#'   y_lags = 2, 
#'   x_lags = 2
#' )
#' 
#' @export 
prepare_data <- function(data, 
                         cols = c("y", "x"),
                         x_lags = NULL,
                         y_lags = NULL) {

  # Check whether only a single column is provided in the data.frame, and whether
  # the person specified no use of x_lags. In this case, we make a pass and allow
  # the user to continue (otherwise, they will get an error)
  if((ncol(data) == 1) & is.null(x_lags)) {
    colnames(data) <- "y"
    data$x <- numeric(nrow(data))
    cols <- c("y", "x")

  } else if(ncol(data) == 1) {
    stop("Only a single column provided, but lags in the covariate specified. Please provide a data.frame with at least two columns.")
  }

  # Check whether columns are contained in the data
  if(!all(cols %in% colnames(data))) {
    stop("Columns specified in `cols` cannot be found in the supplied dataframe.")
  }

  # Check for impossible lags
  y_lags <- ifelse(is.null(y_lags), NA, y_lags)
  x_lags <- ifelse(is.null(x_lags), NA, x_lags)

  if(!is.na(x_lags)) {
    if(x_lags < 0) {
      warning("Specified lags in `x` are below its minimal value (0). Assuming no involvement of the covariate.")
      x_lags <- NA
    }
  }

  if(!is.na(y_lags)) {
    if(y_lags < 1) {
      warning("Specified lags in `y` are below its minimal value (1). Assuming no autoregression.")
      y_lags <- NA
    }
  }

  # Check whether the number of lags are defined for either one of the variables.
  # If not, then we cannot prepare the data, provide a warning, and return the 
  # whole thing.
  if(is.na(y_lags) & is.na(x_lags)) {
    warning("No lags specified for `y` or `x`. Returning original data.frame.")
    return(data)
  }

  # Check for NAs
  if(any(is.na(data))) {
    warning("NAs found in the data. Deleting them in a listwise fashion.")
    
    idx <- !is.na(rowSums(data))
    data <- data[idx, ]
  }

  # Change column names for easy handling
  data <- data[, cols] |>
    `colnames<-` (c("y", "x"))

  # Extract the number of data-points.  
  N <- nrow(data)

  # If some lags in y are specified, create a variable X1 that contains the data
  # across each of those lags.
  if(!is.na(y_lags)) {
    X1 <- matrix(
      NA, 
      nrow = N,
      ncol = y_lags
    ) 

    for(i in 1:y_lags) {
      idx <- 1:(N - i)
      X1[idx + i, i] <- data$y[idx]
    }

    cols1 <- paste0("ylag_", 1:y_lags)
  }

  # If some lags in x are specified, create a variable X2 that contains the data
  # across each of those lags.
  if(!is.na(x_lags)) {
    X2 <- matrix(
      NA, 
      nrow = N,
      ncol = x_lags + 1
    ) 

    for(i in 0:x_lags) {
      idx <- 1:(N - i)
      X2[idx + i, i + 1] <- data$x[idx]
    }

    cols2 <- paste0("xlag_", 0:x_lags)
  }

  # Check for the existence of either of these variables and create a variable
  # X that contains those values that are of interest for the current model
  # (either lagged y, lagged x, or both).
  if(exists("X1") & exists("X2")) {
    X <- cbind(X1, X2) |>
      `colnames<-` (c(cols1, cols2))
  } else if(exists("X1")) {
    X <- X1 |>
      `colnames<-` (cols1)
  } else {
    X <- X2 |>
      `colnames<-` (cols2)
  }

  return(X)
}