#' Calculate system responses
#'
#' Compute system responses based on a particular set of parameters of the general
#' \eqn{ADL(p, q)} model.
#'
#' @details
#' This function calculates system responses for an \eqn{ADL(p, q)} model,
#' formalized as:
#'
#' \deqn{y_t = \alpha + \sum_{i = 1}^p \phi_{i}y_{t-i} + \beta_{x} x_{t} +
#' \sum_{j = 1}^q \beta_{L^{j}x} x_{t - j} + v_{t}}.
#'
#' One should provide an a priori chosen set of parameters through the arguments
#' `intercept`, `x_params`, and `ar_params`.
#'
#' Additionally, one also needs to provide the impulses to the system. One can
#' achieve this in two ways. First, one can specify the impulses to the covariates
#' through `x` and/or the impulses to the innovations through
#' `innovations`. Second, one can provide a data set to the argument
#' `data`, which is then used to derive the (cumulative) impulses of
#' `x` and `innovations` automatically. In this case, it is
#' recommended to put `burnin` to `FALSE`.
#'
#' When both options are specified, the data takes precedence.
#'
#' @param intercept Numeric denoting the intercept, \eqn{\alpha}. Defaults to `0`.
#' @param ar_params Numeric vector denoting the autoregressive parameters (the
#' \eqn{\phi}) parameters of the model. Parameters need to be given in order of
#' increasing lags (i.e., first element for \eqn{\phi_{1}}, second element for
#' \eqn{\phi_{2}},...). Defaults to `0`.
#' @param x_params Numeric vector denoting the covariate parameters of the model
#' (the \eqn{\beta} parameters). Parameters again need to be given in order of
#' increasing lags(i.e., first element for \eqn{\beta_{x}}, second element for
#' \eqn{\beta_{Lx}},...). Defaults to `0`, indicating that there are no
#' covariate parameters.
#' @param x Numeric vector denoting the values of covariate at each time point.
#' Depending on the input vectors supplied, different system responses will be
#' returned. Defaults to `NULL`, which returns an empty vector of the same
#' length as `innovations` (if provided).
#' @param innovations Numeric vector denoting the values of the innovations at
#' each time point. Depending on the input vectors supplied, different system
#' responses will be returned. Defaults to `NULL`, which returns an empty
#' vector of the same length as `x` (if provided).
#' @param data Dataframe containing the variables of interest. Defaults to
#' \code{NULL}, meaning there are no data attached
#' @param cols Character vector denoting the columns containing the variables of
#' interest. First character should denote the dependent variable, the second
#' one the covariate of interest. Defaults to `c("y", "x")`.
#' @param burnin Logical specifying if the part of the system response due to
#' the intercept should be burned in analytically. Defaults to `TRUE`, the
#' system responses responses will be displayed relative to the hypothetical
#' equilibrium state of the system. If set to `FALSE`, initial value
#' dependent behavior will be present.
#' @param na_action Character denoting how \code{NA}s should be removed from the
#' data. Either \code{"listwise"}, \code{"casewise"}, \code{"pairwise"}, or
#' \code{"partial"} (see \code{\link[impulseR]{estimate}} for more information).
#' Ignored whenever innovations do not need to be computed. Defaults to 
#' \code{"listwise"}.
#'
#' @returns List containing the parameters that were used for the generation of
#' the system responses (under `"intercept"`, `"x_params"`, and
#' `"ar_params"`, and a data.frame containing the model-implied total
#' responses over the observation period (under `"irf"`). Within the
#' data.frame, column `"time"` contains the time index starting at 0. The
#' columns `"irf_intercept"`, `"irf_x"`, and `"irf_v"` contain
#' the cumulative responses towards the unit vector, covariate, and innovations
#' respectively. These partial responses sum up to the total response \eqn{y_{t}},
#' which is provided in the `"irf"` column. Finally, the columns `"x"`
#' and `"innovations"` contain the values of the covariate and the
#' innovations.
#'
#' @examples
#' # Create parameters of an ADL(2, 1), meaning having two lags in the residuals
#' # and one lag in the values of x. These will be used for all examples.
#' params <- list(
#'   "intercept" = 1,
#'   "autoregression" = c(0.5, 0.1),
#'   "slopes" = c(2, 0.5)
#' )
#'
#'
#'
#' ########################
#' # PRE-SPECIFIED IMPULSES
#'
#' # Use with single impulse of x in the beginning of the study, with burnin
#' irf_generator(
#'   params$intercept,
#'   params$autoregression,
#'   params$slopes,
#'   x = c(1, rep(0, 9))
#' )
#'
#' # Use with single impulse of x in the beginning of the study, without burnin
#' irf_generator(
#'   params$intercept,
#'   params$autoregression,
#'   params$slopes,
#'   x = c(1, rep(0, 9)),
#'   burnin = FALSE
#' )
#'
#' # Use with multiple values for the innovations, with burnin
#' irf_generator(
#'   params$intercept,
#'   params$autoregression,
#'   params$slopes,
#'   innovations = rnorm(10)
#' )
#'
#' # Use with multiple values for the innovations, without burnin
#' irf_generator(
#'   params$intercept,
#'   params$autoregression,
#'   params$slopes,
#'   innovations = rnorm(10),
#'   burnin = FALSE
#' )
#'
#'
#'
#' ########################
#' # IMPULSES BASED ON DATA
#'
#' # Generate data
#' set.seed(1)
#' data <- irf_generator(
#'   intercept = 1,
#'   x_params = c(1, 2, -0.5),
#'   ar_params = c(0.9, -0.1, 0.25),
#'   x = rnorm(100),
#'   innovations = rnorm(100)
#' )$irf
#'
#' # Use the data to determine the values for x and y.
#' #
#' # Using "irf" as the dependent variable and "x" as the independent variable
#' # in the original data set.
#' irf_generator(
#'   params$intercept,
#'   params$autoregression,
#'   params$slopes,
#'   data = data,
#'   cols = c("irf", "x"),
#'   burnin = FALSE
#' )
#'
#' @export
irf_generator <- function(
  intercept = 0,
  ar_params = 0,
  x_params = 0,
  x = NULL,
  innovations = NULL,
  data = NULL,
  cols = c("y", "x"),
  burnin = TRUE,
  na_action = "listwise"
) {
  # Check whether only a single intercept is provided.
  if (length(intercept) > 1) {
    warning("More than one intercept provided. Using the first value in this vector.")
    intercept <- intercept[1]
  }

  # Check whether the data are specified. If so, then we will recompute x and
  # the innovations
  if (!is.null(data) & is.null(innovations) & is.null(x)) {
    # Check whether the columns can be found in the data.frame
    if (!all(cols %in% colnames(data))) {
      stop("Columns specified in `cols` cannot be found in the supplied dataframe.")
    }

    # Redefine the data so that it fits our internal structure
    data <- data[, cols] |>
      `colnames<-` (c("y", "x"))
    cols <- c("y", "x")

    # Define a time-variable. Is used later on when creating the results of this
    # function
    time_variable <- 1:nrow(data)
    time_variable <- time_variable[!is.na(rowSums(data))]

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

    # Divide up the data in different parts based on missing values (if present).
    #
    # Note that we require different approaches because of different na_actions.
    # Specifically, when the na_action is "partial", then we bridge missing 
    # values by coupling to non-consecutive observations to each other. In this
    # case, irf_generator will automatically lead to the correct solution and 
    # we can ask `prepare_data` to just delete the missing values altogether
    # (specifying na_action = "partial").
    #
    # If NA deletion is done in a listwise fashion, however, then we need the 
    # NAs to be present in the prepared values of y so that we can correctly 
    # relate innovations and x (as coming from `compute_input`) to the values 
    # of y in the dataset, correctly doing the decomposition. If this is the 
    # case, then we will create a list of the data split at exactly those values
    # that are missing, which can then be taken into account when computing the 
    # system responses.
    if(na_action %in% "partial") {
      y <- list(data$y[!is.na(rowSums(data))])

    } else {
      # Define the points at which to split the data.frame
      data$missing <- cumsum(is.na(rowSums(data)))
      y <- split(
        data,
        data$missing
      )

      # Loop over the different data.frame's in y and remove the NA values.
      # Additionally, if there is no data left, then we can remove the whole 
      # instance in the list
      y <- lapply(
        y, 
        function(data) {
          return(
            data$y[!is.na(rowSums(data))]
          )
        }
      )
      y[sapply(y, length) == 0] <- NULL
    }

    # Compute the indices that we need to compute the system responses. These 
    # define which values for the covariate and for the innovations can be 
    # used to decompose the observed values in y correctly and while keeping the
    # preferred na_action in mind. These indices consist of a starting value and 
    # an ending value in the first and second column resp.
    indices <- sapply(y, length) |>
      `names<-` (NULL)

    # Compute the covariate and the innovations based on the data and the taken
    # NA action. 
    input <- compute_input(
      data,
      cols = cols,
      intercept = intercept,
      ar_params = ar_params,
      x_params = x_params,
      innovations = innovations,
      na_action = na_action
    )
    x <- input$x
    innovations <- input$innovations

  # We also define the indices for the loop in case either the data are not 
  # defined or the innovations and x are already provided by the user. This 
  # allows us to use the same code for both cases instead of having to create 
  # separate functions: The indices will be used in either cases to define 
  # which innovations and/or covariates should be used for the necessary
  # computations.
  #
  # Note that we do not need to know how the exact indices here, as the for-loop
  # automatically stops whenever we reach the end. 
  } else {
    indices <- Inf
  }

  # Check whether x or innovations (or both) are provided. If not, then we have to
  # throw an error.
  #
  # If the one is null while the other is not, then we have to create the other
  # with all zeros.
  if (is.null(x) & is.null(innovations)) {
    stop("Neither `x` nor `innovations` is provided. Cannot proceed.")
  } else if (is.null(x) & !is.null(innovations)) {
    x <- numeric(length(innovations))
  } else if (!is.null(x) & is.null(innovations)) {
    innovations <- numeric(length(x))
  }

  # Determine the sample size (the number of values to generate) based on the
  # values provided by x and innovations. Make the simulated length of the
  # system response is given by the maximal length of these two.
  Nx <- length(x)
  Nv <- length(innovations)

  if (Nx < Nv) {
    x <- c(x, rep(0, Nv - Nx))
    warning("Length of `x` is smaller than length of `innovations`. Imputing zeros in `x`.")
  } else if (Nx > Nv) {
    innovations <- c(innovations, rep(0, Nx - Nv))
    warning(
      "Length of `innovations` is smaller than length of `x`. Imputing zeros in `innovations`."
    )
  }

  # Check for NAs
  if (any(is.na(innovations)) | any(is.na(x))) {
    warning(
      "NAs found in the provided `x` and/or `innovations`. Deleting them from the vectors."
    )

    idx <- !is.na(innovations) & !is.na(x)
    innovations <- innovations[idx]
    x <- x[idx]
  }

  # Finally, check if the roots of $\phi(L)$ are all outside the complex unit
  # circle. If at least one root is inside the unit circle, provide a warning.
  phi <- c(1, -ar_params)
  moduli <- abs(polyroot(phi))

  if (length(moduli) >= 1) {
    if (min(moduli) < 1) {
      warning(
        paste(
          "The AR parameters imply a nonstationary process;",
          "Some of the roots of phi(L) are inside the complex unit circle"
        )
      )
    }
  }

  # After all these manipulations, you can proceed with the algebra.
  nt <- length(x)

  # Create matrices of the parameters to avoid double for-loops. One matrix 
  # contains the AR parameters while the other contains the covariate parameters
  p <- length(ar_params)
  phi <- rep(ar_params, each = p) |>
    matrix(nrow = p, ncol = p)
  phi[upper.tri(phi)] <- 0

  q <- length(x_params)
  beta <- rep(x_params, each = q) |>
    matrix(nrow = q, ncol = q)
  beta[upper.tri(beta)] <- 0

  # Split the values for the innovations and covariates into separate lists, 
  # similar to how the indices are used. This will allow a close mapping of these
  # values to those parts that are relevant. In case there are no NAs in either
  # the data or the provided innovations/x, then this step does not really 
  # matter. However, in case of data with NAs, this step is crucial to get the 
  # correct results
  indices[is.infinite(indices)] <- nt
  idx <- cbind(
    c(0, cumsum(indices[2:length(indices) - 1])) + 1, 
    cumsum(indices)
  )

  x <- lapply(
    seq_len(nrow(idx)),
    function(i) x[idx[i, 1]:idx[i, 2]]
  )
  innovations <- lapply(
    seq_len(nrow(idx)),
    function(i) innovations[idx[i, 1]:idx[i, 2]]
  )

  # Loop over the previously defined indices. Specifically, we loop over the rows
  # within this matrix to ensure that each part (as separated by potential NAs)
  # is handled separately for computing the system responses.
  responses <- lapply(
    seq_along(indices),
    function(i) {
      # Define the ending index the second loop should go
      end <- min(indices[i], nt)

      # Extract the values for the innovations and the covariates which are 
      # relevant for this part of the data (or for the response as a whole, 
      # depending on the user's arguments)
      x_i <- x[[i]]
      innovations_i <- innovations[[i]]

      # Based on this index, allocate memory for local definitions of theta and 
      # psi, which will contain the system responses for the innovations and 
      # covariates respectively
      theta <- numeric(end)
      psi <- numeric(end)

      # Perform the second loop within which system responses will be handled.
      for(j in seq_len(end)) {
        # Apply Equation 26 to get \theta, that is the system responses in response
        # to the innovations. Note that \theta is 1 for the first iteration, and
        # only partially complete for the first few observations (as long as j is
        # smaller than the number of lags)
        if(j == 1) {
          terms <- 1
        } else if(j <= p) {
          terms <- phi[(j - 1), 1:(j - 1)] %*% theta[(j - 1):1]
        } else {
          terms <- phi[p, ] %*% theta[(j - 1):(j - p)]
        }
        theta[j] <- sum(terms)

        # Apply Equation 30 and 33 to get \psi, that is the system responses in 
        # response to the covariate. Note that \psi is only partially complete 
        # for the first few observations (as long j is smaller than the number of 
        # lags)
        if (j <= q) {
          terms <- beta[j, 1:j] %*% theta[j:1]
        } else {
          terms <- beta[q, ] %*% theta[j:(j - q + 1)]
        }
        psi[j] <- sum(terms)
      }

      # Once defined, compute the system responses. Set up some of the output 
      # variables and do the one-sided convolutions of \psi with the values of x 
      # and of \theta with the values of the innovations \epsilon. We do this 
      # based on Equation 20, where:
      #
      #   irf_x = \sum \psi_k L^k x_t
      #   irf_v= \sum \theta_k L^k v_t
      #   irf_\text{intercept} = \sum \alpha \theta_k L^k 1_t
      #
      # It is these sums that we are computing here. Note that this is also 
      # equivalent to Equation 38, which makes explicit use of the impulse 
      # response notation.
      irf_x <- irf_v <- irf_intercept <- numeric(end)
      for (t in seq_len(end)) {
        irf_x[t] <- sum(psi[1:t] * x_i[t:1])
        irf_v[t] <- sum(theta[1:t] * innovations_i[t:1])
        irf_intercept[t] <- sum(theta[1:t] * intercept)
      }

      # Once performed, put everything in a data.frame and return
      return(
        data.frame(
          "irf_intercept" = irf_intercept,
          "irf_x" = irf_x,
          "irf_v" = irf_v,
          "x" = x_i,
          "innovations" = innovations_i
        )
      )
    }
  )
  responses <- do.call("rbind", responses)

  # Use these sums to generate y_t, following Equation 38. First, decide what
  # to do with the intercept based on the argument `burnin`. If `burnin = TRUE`,
  # replace the previously computed sum with the long-time limit of the process,
  # computed as:
  #
  #   \frac{\alpha}{1 - \sum \phi_i}
  #
  # Otherwise, you keep the sum as defined above.
  if (burnin) {
    responses$irf_intercept[] <- intercept / (1 - sum(ar_params))
  }

  responses$irf <- responses$irf_x + responses$irf_v + responses$irf_intercept

  # Add the time variable to this data.frame
  if(exists("time_variable")) {
    responses$time <- time_variable
  } else {
    responses$time <- 1:nrow(responses) - 1
  }

  # Rearrange the columns of the data.frame
  cols <- c("time", "irf", "irf_intercept", "irf_x", "irf_v", "x", "innovations")
  responses <- responses[, cols]

  return(
    list(
      "fit" = NA,
      "intercept" = intercept,
      "x_params" = x_params,
      "ar_params" = ar_params,
      "irf" = responses
    )
  )
}
