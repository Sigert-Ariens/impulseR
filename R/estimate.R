#' Estimate the parameters of an $ADL(p, q)$ model
#' 
#' This function estimates the parameters of an $ADL(p, q)$ and prepares the 
#' output for the calculation of system responses in other functions of the 
#' package. Always estimates an intercept. 
#' 
#' @details
#' Estimate the parameters of an $ADL(p, q)$, where $p$ represents the number of
#' autoregressive effects (effects of lags of the dependent variable $y_{t}$) 
#' and $q$ represents the number of lagged covariate effects. The $ADL(p, q)$ is 
#' formalized as: 
#' 
#' \eqn{y_t = \alpha + \sum_{i = 1}^p \phi_{i}y_{t-i} + \beta_{x} x_{t} + 
#' \sum_{i = 1}^q \beta_{L^{j}x} x_{t - j} + v_{t}}
#' 
#' @param data Dataframe containing the variables of interest
#' @param cols Character vector denoting the columns containing the variables of
#' interest. First character should denote the dependent variable, the second 
#' one the covariate of interest. Defaults to \code{c("y", "x")}
#' @param y_lags Integer denoting the number of AR effects to estimate freely. 
#' Starts at the value \code{1}, allowing for an AR(1) structure. Defaults to 
#' \code{NA}, communicating that you don't want to allow for any AR effects.
#' @param x_lags Integer denoting the number of lagged covariate effects. Starts 
#' at the value \code{0}, implying that only the contemporaneous effect, 
#' $\beta_{x}$, is estimated freely. Defaults to \code{NA}, communicating 
#' that you don't want to estimate any covariate parameters 
#' 
#' @return Named list containing the estimated model (\code{fit}), the 
#' parameter estimates (\code{intercept}, \code{x_params}, \code{ar_params}), 
#' the observed covariate values (\code{x}) and the estimated innovations 
#' \code{innovations}
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
        "fit" = list(),
        "intercept" = mean(data$y),
        "x_params" = 0,
        "ar_params" = 0,
        "x" = data$x,
        "innovations" = data$y - mean(data$y)
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
      "fit" = results,
      "intercept" = intercept,
      "ar_params" = ar_params,
      "x_params" = x_params,
      "x" = data$x, 
      "innovations" = compute_innovations(
        data, 
        cols = c("y", "x"),
        intercept = intercept,
        ar_params = ar_params,
        x_params = x_params, 
        innovations = results$innovations
      )
    )
  )
}

#' Compute estimated innovations of an $ADL(p,q)$ model
#' 
#' This function computes the innovations of an $ADL(p, q)$ model, fixing initial 
#' innovations in order to construct empirical trajectory plots. 
#' 
#' @details
#' If there are $p$ AR effects, the fit object will only return innovations for 
#' the last \code{N- p} datapoints. Least squares treats the initial values, 
#' $y_{0},...,y_{p}$ and $x_{0},...,x_{p}$ as known for parameter estimation. 
#' To generate empirical trajectory plots starting from the first measurement 
#' occasion (t = 0), we can fix the 'missing' innovations by using knowledge of 
#' the initial values and the parameter estimates (see appendix X). The following 
#' function fixes the initial innovations appropriately for the user, and 
#' returns a list containing all \code{N} innovations. 
#' 
#' Used under the hood in the \code{\link[impulseR]{estimate}} and 
#' \code{\link[impulseR]{irf_generator}} function. 
#' 
#' @param data Dataframe containing the variables of interest
#' @param cols Character vector denoting the columns containing the variables of
#' interest. First character should denote the dependent variable, the second 
#' one the covariate of interest. Defaults to \code{c("y", "x")}
#' @param intercept Numeric denoting the intercept to use for the impulse response 
#' function. Defaults to \code{0}
#' @param ar_params Numeric vector denoting the estimated AR parameters. 
#' Parameters need to be given in order of increased lag (i.e., first element 
#' for $\phi_{1}$, second element for $\phi_{2}$,...). Defaults to \code{0}
#' @param x_params Numeric vector denoting the estimated covariate parameters. 
#' Parameters again need to be given in order of increased lag (i.e., first 
#' element for $\beta_{x}$, second element for $\beta_{Lx}$,...). Defaults to 
#' \code{0}, implying no covariate parameters.
#' @param innovations Numeric vector denoting the values of estimated innovations 
#' up to time point t (e.g., those acquired through the \code{lm} function). 
#' Defaults to \code{NULL}, signalling the estimated innovations are first 
#' calculated from the parameter estimates and observed data values $y$ and $x$. 
#' 
#' @return Numeric vector of the same length as the data containing the 
#' innovations for the provided model
#' 
#' @examples 
#' # Define a dataset
#' x <- rnorm(100)
#' data <- data.frame(
#'   DV = 1 + 2 * x + rnorm(100),
#'   IV = x
#' )
#' 
#' # For these data, compute the innovations of an ADL(2, 2)
#' innovations <- compute_innovations(
#'   data, 
#'   cols = c("DV", "IV"),
#'   intercept = 0.5, 
#'   ar_params = c(0.75, 0.25),
#'   x_params = c(1.5, 0.25)
#' )
#' 
#' @export 
compute_innovations <- function(data, 
                                cols = c("y", "x"),
                                intercept = 0, 
                                ar_params = 0, 
                                x_params = 0, 
                                innovations = NULL) {
  
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
  
  # Compute the estimated innovations ($\hat{v}_{t} = y_{t} - \hat{y}_{t}) 
  #
  # Only perform this computation if innovations are not provided yet in the form of a lm() output object.
  if(is.null(innovations)) {
    # Prepare the data and parameters for the prediction step.
    X <- prepare_data(
      data, 
      cols = c("y", "x"),
      x_lags = x_lags,
      y_lags = y_lags
    )
    
    if(is.na(x_lags) & is.na(y_lags)) {
      # If no AR effects or covariate parameters are defined, then y_hat is simply the intercept.
      innovations <- data$y - intercept
      
    } else {
      # Otherwise, we can differentiate between several cases
      if(!is.na(x_lags) & !is.na(y_lags)) {
        params <- c(ar_params, x_params)
        
      } else if(!is.na(x_lags)) {
        params <- x_params
        
      } else if(!is.na(y_lags)) {
        params <- ar_params
        
      } 
      
      # Delete NAs from X (missing covariate values are treated by listwise deletion)
      idx <- !is.na(rowSums(X))
      X <- X[idx, , drop = FALSE]
      
      # Compute the estimated innovations based on the predicted values for y.
      y_hat <- intercept + X %*% params
      innovations <- data$y[(N - nrow(X) + 1):N] - y_hat
    }    
  }
  
  # Deduce the implied first $p$ innovations. This allows our cumulative responses to start from the first measurement occasion, i.e. at $t = 0$.
  # Parameters are vectorized so that you don't have to think about it too much. For details, see X appendix X. 
  n_inx <- N - length(innovations)
  
  # Fixing the first $p$ innovations is only needed whenever $p \neq 0$,
  # otherwise the innovation at t=0 will be provided by the estimation software.
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
      # Supply the first $p$ values of the outcome variable (set as the vector y0 above), and use the estimates to fix part of the innovations
      if(!is.na(y_lags)) {
        if(i == 1) {
          inx[i] <- inx[i] - 0
        } else if(i <= y_lags) {
          inx[i] <- inx[i] - sum(phi[i - 1, 1:(i - 1)] * y0[(i - 1):1])
        } else {
          inx[i] <- inx[i] - sum(phi[nrow(phi), ] * y0[(i - 1):(i - y_lags)])
        }
      }
      
      # Supply the first $p$ values of the covariate variable (set as the vector x0 above), and use the estimates to fix another part of the innovations
      if(!is.na(x_lags)) {
        if(i <= x_lags) {
          inx[i] <- inx[i] - sum(beta[i, 1:i] * x0[i:1])
        } else {
          inx[i] <- inx[i] - sum(beta[nrow(beta), ] * x0[i:(i - x_lags)])
        }
      }
      
      # Supply the intercept parameter to fix the final part of the innovations. 
      inx[i] <- inx[i] - intercept
    }
    
    innovations <- c(inx, innovations)
  }
  
  return(innovations)
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
#' prepared_data <- prepare_data(
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









