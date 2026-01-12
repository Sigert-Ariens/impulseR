#' Estimate the parameters of an \eqn{ADL(p, q)} model
#'
#' This function estimates the parameters of an \eqn{ADL(p, q)} and prepares the
#' output for the calculation of system responses in other functions of the
#' package. Always estimates an intercept.
#'
#' @details
#' Estimate the parameters of an \eqn{ADL(p, q)}, where \eqn{p} represents the
#' number of autoregressive effects (effects of lags of the dependent variable
#' \eqn{y_t} and \eqn{q} represents the number of lagged covariate effects. The
#' \eqn{ADL(p, q)} is formalized as:
#'
#' \deqn{y_t = \alpha + \sum_{i = 1}^p \phi_{i}y_{t-i} + \beta_{x} x_{t} +
#' \sum_{i = 1}^q \beta_{L^{j}x} x_{t - j} + v_{t}}
#'
#' @param data Dataframe containing the variables of interest
#' @param cols Character vector denoting the columns containing the variables of
#' interest. First character should denote the dependent variable, the second
#' one the covariate of interest. Defaults to `c("y", "x")`.
#' @param y_lags Integer denoting the number of AR effects to estimate freely.
#' Starts at the value `1`, allowing for an AR(1) structure. Defaults to
#' \code{NA}, communicating that you don't want to allow for any AR effects.
#' @param x_lags Integer denoting the number of lagged covariate effects. Starts
#' at the value `0`, implying that only the contemporaneous effect,
#' \eqn{\beta_x}, is estimated freely. Defaults to `NA`, communicating that
#' you don't want to estimate any covariate parameters
#' @param na_action Character denoting how \code{NA}s should be removed from the
#' data. Either \code{"listwise"}, \code{"casewise"}, \code{"pairwise"}, or
#' \code{"partial"}. Listwise and casewise deletion consists of deletion of 
#' full rows of data when one or more of the the matched variables contains an 
#' \code{NA}, including the values of the lagged variables. This method may lead
#' to a lot of deleted data, especially when estimating \eqn{ADL}s with many 
#' lags in their predictor variables. To alleviate this difficulty, we also allow 
#' users to specify partial deletion -- the deletion of rows when \code{NA} is 
#' found in the contemporaneous values of the variables, meaning you bridge the 
#' gap created by \code{NA}s -- or pairwise deletion -- the pairwise use of 
#' for the estimation of the relevant parameters whenever they are not \code{NA},
#' not implemented yet. Defaults to \code{"listwise"}.  
#'
#' @return Named list containing the estimated model (`fit`), the
#' parameter estimates (`intercept`, `x_params`, `ar_params`),
#' the observed covariate values (`x`) and the estimated innovations
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
#' @export
estimate <- function(
  data,
  cols = c("y", "x"),
  y_lags = NULL,
  x_lags = NULL,
  na_action = "listwise"
) {
  # Check whether only a single column is provided in the data.frame, and whether
  # the person specified no use of x_lags. In this case, we make a pass and allow
  # the user to continue (otherwise, they will get an error)
  if ((ncol(data) == 1) & is.null(x_lags)) {
    colnames(data) <- "y"
    data$x <- numeric(nrow(data))
    cols <- c("y", "x")
  } else if (ncol(data) == 1) {
    stop(
      paste(
        "Only a single column provided, but lags in the covariate specified.",
        "Please provide a data.frame with at least two columns."
      )
    )
  }

  # Check whether the columns specified can be found in the dataset
  if (!all(cols %in% colnames(data))) {
    stop("Columns specified in `cols` cannot be found in the supplied dataframe.")
  }

  # Check for impossible lags
  y_lags <- ifelse(is.null(y_lags), NA, y_lags)
  x_lags <- ifelse(is.null(x_lags), NA, x_lags)

  if (!is.na(x_lags)) {
    if (x_lags < 0) {
      warning(
        paste(
          "Specified lags in `x` are below its minimal value (0).",
          "Assuming no involvement of the covariate."
        )
      )
      x_lags <- NA
    }
  }

  if (!is.na(y_lags)) {
    if (y_lags < 1) {
      warning("Specified lags in `y` are below its minimal value (1). Assuming no autoregression.")
      y_lags <- NA
    }
  }

  # Change column names for easier handling
  data <- data[, cols] |>
    `colnames<-`(c("y", "x"))

  # Check whether any lags have been provided. If not, then we have to estimate
  # an empty model, that is one in which only the mean is estimated and all other
  # parameters are 0. This is the immediate result.
  if (is.na(y_lags) & is.na(x_lags)) {
    return(
      list(
        "fit" = list(),
        "intercept" = mean(data$y, na.rm = TRUE),
        "x_params" = 0,
        "ar_params" = 0,
        "x" = data$x,
        "innovations" = data$y - mean(data$y, na.rm = TRUE)
      )
    )
  }

  # Use the lm-function to estimate the parameters of the model. Before doing
  # this, we create a matrix containing all of the different lagged variables,
  # which can be supplied to lm.
  #
  # Before applying lm, filter out the NA-values
  prepared <- prepare_data(
    data,
    cols = c("y", "x"),
    x_lags = x_lags,
    y_lags = y_lags,
    na_action = na_action
  )
  y <- prepared$y
  X__ <- prepared$X

  results <- stats::lm(y ~ X__)

  # Extract all of the parameters and put them in a format that our package uses
  # under the hood
  coefs <- summary(results)$coefficients[, 1] |>
    as.numeric() |>
    `names<-`(NULL)

  intercept <- coefs[1]

  if (!is.na(y_lags) & !is.na(x_lags)) {
    ar_params <- coefs[2:(1 + y_lags)]
    x_params <- coefs[(2 + y_lags):length(coefs)]
  } else if (!is.na(y_lags)) {
    ar_params <- coefs[2:(1 + y_lags)]
    x_params <- 0
  } else if (!is.na(x_lags)) {
    ar_params <- 0
    x_params <- coefs[2:(2 + x_lags)]
  }

  # Compute the input based on the model
  input <- compute_input(
    data,
    cols = c("y", "x"),
    intercept = intercept,
    ar_params = ar_params,
    x_params = x_params,
    innovations = results$innovations,
    na_action = na_action
  ) |>
    suppressWarnings()

  # Now that this has been done, return a list containing all of this information
  return(
    list(
      "fit" = results,
      "intercept" = intercept,
      "ar_params" = ar_params,
      "x_params" = x_params,
      "x" = input$x,
      "innovations" = input$innovations
    )
  )
}

#' Prepare data for analysis
#'
#' @details
#' Transforms a data.frame in such a way that we can estimate the parameters
#' for the specified number of lags in y and x.
#'
#' Used under the hood in the [estimate()] function.
#'
#' @param data Dataframe containing the variables of interest
#' @param cols Character vector denoting the columns containing the variables of
#' interest. First character should denote the dependent variable, the second
#' one the covariate of interest. Defaults to `c("y", "x")`.
#' @param y_lags Integer denoting the number of lags to include for the
#' dependent variable. Starts at the value `1`. Defaults to `NA`,
#' communicating that you don't want to use any lagged values of the dependent
#' variable
#' @param x_lags Integer denoting the number of lags to include for the
#' covariate. Starts at the value `0`. Defaults to `NA`, communicating
#' that you don't want to use any lagged values of the dependent variable
#' @param na_action Character denoting how \code{NA}s should be removed from the
#' data. Either \code{"listwise"}, \code{"casewise"}, \code{"pairwise"}, or
#' \code{"partial"}. Listwise and casewise deletion consists of deletion of 
#' full rows of data when one or more of the the matched variables contains an 
#' \code{NA}, including the values of the lagged variables. This method may lead
#' to a lot of deleted data, especially when estimating \eqn{ADL}s with many 
#' lags in their predictor variables. To alleviate this difficulty, we also allow 
#' users to specify partial deletion -- the deletion of rows when \code{NA} is 
#' found in the contemporaneous values of the variables, meaning you bridge the 
#' gap created by \code{NA}s -- or pairwise deletion -- the pairwise use of 
#' for the estimation of the relevant parameters whenever they are not \code{NA},
#' not implemented yet. Defaults to \code{"listwise"}. 
#' 
#' @returns Named list containing the values of the dependent variable 
#' (\code{"y"}) and a matched numeric matrix containing the values of the 
#' independent and dependent variables that are used as predictors in the 
#' estimation routine.
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
prepare_data <- function(
  data,
  cols = c("y", "x"),
  x_lags = NULL,
  y_lags = NULL,
  na_action = "listwise"
) {
  # Check whether only a single column is provided in the data.frame, and whether
  # the person specified no use of x_lags. In this case, we make a pass and allow
  # the user to continue (otherwise, they will get an error)
  if ((ncol(data) == 1) & is.null(x_lags)) {
    colnames(data) <- "y"
    data$x <- numeric(nrow(data))
    cols <- c("y", "x")
  } else if (ncol(data) == 1) {
    stop(
      paste(
        "Only a single column provided, but lags in the covariate specified.",
        "Please provide a data.frame with at least two columns."
      )
    )
  }

  # Check whether columns are contained in the data
  if (!all(cols %in% colnames(data))) {
    stop("Columns specified in `cols` cannot be found in the supplied dataframe.")
  }

  # Check for NAs. Only includes less stringent types of deletion, namely
  #   - Partial listwise deletion: Only deleting NAs at a particular timepoint, 
  #     but not extending it to lagged NAs
  #   - Casewise deletion: Only deleting NAs case per case, which is automatically
  #     handled by lm
  #
  # If partial deletion, we already need to delete the NAs in the data to 
  # bridge any gaps and connect datapoints that follow before and after the 
  # gap
  if (any(is.na(data)) & na_action %in% c("partial")) {
    warning("NAs found in the data. Deleting them in a partial fashion.")

    missing <- !is.na(rowSums(data))
    data <- data[missing, ]

  # If pairwise deletion, then throw an error and tell users that we still have
  # to implement this
  } else if(any(is.na(data)) & na_action == "pairwise") {
    stop("NAs found in the data. Wanting to delete them in a pairwise fashion, but has not been implemented yet.")

  # If listwise/casewise deletion, then throw a warning but do not delete the 
  # NAs yet. Only do so after making the relevant matrix of values, ensuring we 
  # delete all cases that are paired with an NA value
  } else if(any(is.na(data)) & na_action %in% c("listwise", "casewise")) {
    warning("NAs found in the data. Deleting them in a listwise/casewise fashion")

  } else if(any(is.na(data)) & na_action %in% c("none")) {
    warning("NAs found in the data, but leaving them in.")

  # If the user asked something else, throw an error and ensure that they know 
  # the options we have for handling NAs
  } else if(any(is.na(data)) & !(na_action %in% c("listwise", "partial", "casewise", "pairwise"))) {
    stop(
      paste(
        "NAs found in the data, but proposed method is not known.", 
        "Please specify 'listwise', 'casewise', 'partial', or 'pairwise' for the `na_action` argument."
      )
    )
  }

  # Check for impossible lags
  y_lags <- ifelse(is.null(y_lags), NA, y_lags)
  x_lags <- ifelse(is.null(x_lags), NA, x_lags)

  if (!is.na(x_lags)) {
    if (x_lags < 0) {
      warning(
        paste(
          "Specified lags in `x` are below its minimal value (0).",
          "Assuming no involvement of the covariate."
        )
      )
      x_lags <- NA
    }
  }

  if (!is.na(y_lags)) {
    if (y_lags < 1) {
      warning("Specified lags in `y` are below its minimal value (1). Assuming no autoregression.")
      y_lags <- NA
    }
  }

  # Check whether the number of lags are defined for either one of the variables.
  # If not, then we cannot prepare the data, provide a warning, and return the
  # whole thing.
  if (is.na(y_lags) & is.na(x_lags)) {
    warning("No lags specified for `y` or `x`. Returning original data.frame.")
    return(data)
  }

  # Change column names for easy handling
  data <- data[, cols] |>
    `colnames<-`(c("y", "x"))

  # Extract the number of data-points.
  N <- nrow(data)

  # If some lags in y are specified, create a variable X1 that contains the data
  # across each of those lags.
  if (!is.na(y_lags)) {
    X1 <- matrix(
      NA,
      nrow = N,
      ncol = y_lags
    )

    for (i in 1:y_lags) {
      idx <- 1:(N - i)
      X1[idx + i, i] <- data$y[idx]
    }

    cols1 <- paste0("ylag_", 1:y_lags)
  }

  # If some lags in x are specified, create a variable X2 that contains the data
  # across each of those lags.
  if (!is.na(x_lags)) {
    X2 <- matrix(
      NA,
      nrow = N,
      ncol = x_lags + 1
    )

    for (i in 0:x_lags) {
      idx <- 1:(N - i)
      X2[idx + i, i + 1] <- data$x[idx]
    }

    cols2 <- paste0("xlag_", 0:x_lags)
  }

  # Check for the existence of either of these variables and create a variable
  # X that contains those values that are of interest for the current model
  # (either lagged y, lagged x, or both).
  if (exists("X1") & exists("X2")) {
    X <- cbind(X1, X2) |>
      `colnames<-`(c(cols1, cols2))
  } else if (exists("X1")) {
    X <- X1 |>
      `colnames<-`(cols1)
  } else {
    X <- X2 |>
      `colnames<-`(cols2)
  }

  # Delete paired observations that contain NA in them
  if(!(na_action %in% c("none"))) {
    idx <- rowSums(is.na(X)) == 0
    X <- X[idx, , drop = FALSE]
    y <- data$y[idx]

  } else {
    y <- data$y
  }

  # Create a list of missing values depending on the na_action that was chosen
  if(na_action == "partial") {
    missing <- list(
      which(!missing),
      which(!idx)
    )
  } else if(na_action %in% c("listwise", "casewise")) {
    missing <- list(which(!idx))

  } else {
    missing <- list(which(rowSums(is.na(data)) != 0))
  }
  
  return(
    list(
      "y" = y, 
      "X" = X,
      "missing" = missing
    )
  )
}
