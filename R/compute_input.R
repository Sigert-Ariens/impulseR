#' Compute estimated innovations of an \eqn{ADL(p,q)} model
#'
#' This function computes the innovations of an \eqn{ADL(p,q)} model, fixing
#' initial innovations in order to construct empirical trajectory plots.
#'
#' @details
#' If there are \eqn{p} AR effects, the fit object will only return innovations
#' for the last `N - p` datapoints. Least squares treats the initial values,
#' \eqn{y_0,...,y_p} and \eqn{x_0,...,x_p} as known for parameter estimation. To
#' generate empirical trajectory plots starting from the first measurement
#' occasion (\eqn{t = 0}), we can fix the 'missing' innovations by using
#' knowledge of the initial values and the parameter estimates.
#'
#' This function fixes the initial innovations appropriately for the user, and
#' returns a list containing all `N` innovations. This function is primarily
#' used under the hood of the [estimate()] and [irf_generator()] function.
#'
#' @param data Dataframe containing the variables of interest
#' @param cols Character vector denoting the columns containing the variables of
#' interest. First character should denote the dependent variable, the second
#' one the covariate of interest. Defaults to `c("y", "x")`.
#' @param intercept Numeric denoting the intercept to use for the impulse response
#' function. Defaults to `0`.
#' @param ar_params Numeric vector denoting the estimated AR parameters.
#' Parameters need to be given in order of increased lag (i.e., first element
#' for \eqn{\phi_1}, second element for \eqn{\phi_2},...). Defaults to `0`.
#' @param x_params Numeric vector denoting the estimated covariate parameters.
#' Parameters again need to be given in order of increased lag (i.e., first
#' element for \eqn{\beta_x}, second element for \eqn{\beta_{Lx}},...).
#' Defaults to `0`, implying no covariate parameters.
#' @param innovations Numeric vector denoting the values of estimated innovations
#' up to time point \eqn{t} (e.g., those acquired through the `lm`.
#' function). Defaults to `NULL`, signaling the estimated innovations are
#' first calculated from the parameter estimates and observed data values
#' \eqn{y} and \eqn{x}.
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
#' innovations <- compute_input(
#'   data,
#'   cols = c("DV", "IV"),
#'   intercept = 0.5,
#'   ar_params = c(0.75, 0.25),
#'   x_params = c(1.5, 0.25)
#' )
#'
#' @export
#
# TO DO: Make possible to already provide the prepared data. Will avoid a second 
# time the warning is called and is a reasonably easy fix
compute_input <- function(
  data,
  cols = c("y", "x"),
  intercept = 0,
  ar_params = 0,
  x_params = 0,
  innovations = NULL,
  na_action = "listwise"
) {

  ##############################################################################
  # CHECK ARGUMENTS

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
  if ((ncol(data) == 1) & is.na(x_lags)) {
    colnames(data) <- "y"
    data$x <- numeric(nrow(data))
    cols <- c("y", "x")
  } else if (ncol(data) == 1) {
    stop(
      paste(
        "Only a single column provided, but lags in the covariate specified.",
        " Please provide a data.frame with at least two columns."
      )
    )
  }

  # Check whether columns are contained in the data
  if (!all(cols %in% colnames(data))) {
    stop("Columns specified in `cols` cannot be found in the supplied dataframe.")
  }

  # Change column names for easier handling
  data <- data[, cols] |>
    `colnames<-`(c("y", "x"))

  # Compute the number of datapoints
  N <- nrow(data)

  # Create a version of the y and X variables that contains the NAs in them 
  # instead of deleting them. Will make it easier to provide actions to ensure
  # the innovations are fixed accordingly
  contains_missing <- prepare_data(
    data,
    cols = c("y", "x"),
    x_lags = x_lags,
    y_lags = y_lags,
    na_action = "none"
  ) |>
    suppressWarnings()



  ##############################################################################
  # FIX COVARIATE

  # Based on the values of y, we can determine what values of the covariate 
  # exist and can be used as input to decompose the system responses. 
  idx <- !(1:nrow(data) %in% contains_missing$missing)
  x_fixed <- data$x[idx]
  


  ##############################################################################
  # FIX INNOVATIONS

  # Compute the estimated innovations ($\hat{v}_{t} = y_{t} - \hat{y}_{t})
  # Only perform this computation if innovations are not provided yet in the form
  # of a lm() output object.
  #
  # Note that this represents the innovations that we know off when using only 
  # the full data, that is using all information we have. When NAs are present 
  # in the data, this represents only a part of the data.
  if (is.null(innovations)) {
    # Prepare the data for analysis. Importantly, this only uses the information 
    # that you provided to the estimation itself. Note that if na_action is 
    # "none", that there will be issues here! Therefore changes to "listwise"
    # (the default way of dealing with NAs in `lm`)
    prepared <- prepare_data(
      data,
      cols = c("y", "x"),
      x_lags = x_lags,
      y_lags = y_lags,
      na_action = ifelse(
        na_action %in% c("none"),
        "listwise",
        na_action
      )
    ) |>
      suppressWarnings()

    # If no AR effects or covariate parameters are defined, then y_hat is simply 
    # the intercept.
    if (is.na(x_lags) & is.na(y_lags)) {
      innovations <- prepared$y - intercept

    # Otherwise, we can differentiate between several cases
    } else {
      # There are both lags in the covariate and the dependent variable
      if (!is.na(x_lags) & !is.na(y_lags)) {
        params <- c(ar_params, x_params)

      # There is only a lag in the covariate
      } else if (!is.na(x_lags)) {
        params <- x_params

      # There is only a lag in the dependent variable
      } else if (!is.na(y_lags)) {
        params <- ar_params
      }

      # Compute the estimated innovations based on the predicted values for y.
      X <- prepared$X
      y_hat <- intercept + X %*% params
      innovations <- prepared$y - y_hat
    }
  }

  # Deduce the implied first $p$ innovations. This allows our cumulative
  # responses to start from the first measurement occasion, i.e. at $t = 0$.
  # Parameters are vectorized so that you don't have to think about it too much.
  #
  # Ensure that this is robust against no specifications of the lags in x and y.
  lag <- max(
    c(y_lags, x_lags),
    na.rm = TRUE
  ) |>
    suppressWarnings()

  # If no lag is specified, then we can already return the results, as the 
  # innovations that we computed before are accurate. For some reason, `max` 
  # returns -Inf when all of its values contain NA, so we need to prepare for 
  # this here.
  if(lag < 0) {
    return(
      list(
        "innovations" = innovations,
        "x" = x
      )
    )
  }

  # Create a placeholder that will contain the innovations. Important to note 
  # that the only innovations that we can fix are those for which a value of 
  # y is known, hence we indicate at which locations there are unknown values 
  # for the dependent variable.
  placeholder <- numeric(nrow(data))
  placeholder[is.na(rowSums(data))] <- NA

  # Get the starting values for where we can start correcting/fixing the 
  # innovations. These occur at `lag` lags before the first complete pair again.
  # Here, we thus proceed as follows:
  #   - Find out at which locations we don't have complete pairs
  #   - Find out when we switch from an NA to a complete pair to an NA
  #   - Define the starting values from which we can start fixing the innovations
  #     as the locations of these switches - the lag.
  #
  # For interpretation, note that this works in indexing as we only fix those
  # innovations for which we have complete values. For example, if an NA is 
  # found on location 6 and this propagates to other lags, but location 7 is 
  # a complete value, then we can start fixing innovations only starting at 
  # location 7. Similarly, imagine that there are NAs on both locations 6 and 
  # 7, then we can only start fixing innovations starting at location 8. This is
  # automatically accounted for in this code.
  idx <- !(is.na(rowSums(contains_missing$X)))
  missing_changes <- c(0, diff(idx))
  start <- which(missing_changes == 1) - lag

  # Fill the placeholder with those innovations that we already know about, 
  # namely those that do not fall within the `start` and its lags problems. 
  # Note that we need to correct the indices by subtracting 1. Furthermore 
  # remove the lags that cannot be corrected
  to_fix <- sapply(
    start, 
    function(idy) if(idy == 1) {
      return(idy:(idy + lag - 1))
    } else {
      return((idy - 1):(idy + lag - 1))
    }
  ) |>
    unlist() |>
    as.numeric()

  idx <- which(!(1:length(placeholder) %in% to_fix))
  placeholder[idx] <- innovations

  # Loop over the different starting values
  for(i in start) {
    # Define the indices of interest
    to_fix <- i:(i + lag - 1)

    # Get the initial conditions from which to start correcting within the 
    # dataset
    y0 <- data$y[to_fix]
    x0 <- data$x[to_fix]

    # Create matrices that can be used to compute the system responses, depending
    # on whether x_lags and y_lags is defined
    if (!is.na(y_lags)) {
      phi <- matrix(
        rep(ar_params, each = y_lags),
        nrow = y_lags,
        ncol = length(ar_params)
      )
      phi[upper.tri(phi, diag = FALSE)] <- 0
    }

    if (!is.na(x_lags)) {
      beta <- matrix(
        rep(x_params, each = x_lags + 1),
        nrow = x_lags + 1,
        ncol = length(x_params)
      )
      beta[upper.tri(beta, diag = FALSE)] <- 0
    }

    # Loop over the initial conditions to fix in the placeholder
    for(j in seq_along(to_fix)) {
      # Define the initial condition to start from
      inx <- data$y[to_fix[j]]

      # Supply the first $p$ values of the outcome variable (set as the vector
      # y0 above), and use the estimates to fix part of the innovations
      if (!is.na(y_lags)) {
        if (j == 1) {
          inx <- inx - 0
        } else if (j <= y_lags) {
          inx <- inx - sum(phi[j - 1, 1:(j - 1)] * y0[(j - 1):1])
        } else {
          inx <- inx - sum(phi[nrow(phi), ] * y0[(j - 1):(j - y_lags)])
        }
      }

      # Supply the first $p$ values of the covariate variable (set as the vector
      # x0 above), and use the estimates to fix another part of the innovations
      if (!is.na(x_lags)) {
        if (j <= x_lags) {
          inx <- inx - sum(beta[j, 1:j] * x0[j:1])
        } else {
          inx <- inx - sum(beta[nrow(beta), ] * x0[j:(j - x_lags)])
        }
      }

      # Supply the intercept parameter to fix the final part of the innovations.
      placeholder[to_fix[j]] <- inx - intercept
    }
  }
  innovations_fixed <- placeholder[!is.na(placeholder)]

  return(
    list(
        "innovations" = innovations_fixed,
        "x" = x_fixed
    )
  )
}