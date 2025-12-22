#' Estimate and compute system responses for an \eqn{ADL(p, q)}
#'
#' This function can be used to construct empirical trajectory plots. First, an
#' \eqn{ADL(p, q)} model is fitted to the data using the  [estimate()] function, where \eqn{p}
#' is the number of AR parameters and \eqn{q} the number of lagged covariate
#' effects. Then, the function creates cumulative responses through the [irf_generator()] function.
#'
#' @details
#' Estimation is currently performed through OLS, using the base [stats::lm()]
#' function.
#'
#' @param data Dataframe containing the variables of interest
#' @param cols Character vector denoting the columns containing the variables of
#' interest. First character should denote the dependent variable, the second
#' one the covariate of interest. Defaults to `c("y", "x")`.
#' @param y_lags Integer denoting the number of AR effects,
#' \eqn{p}, to estimate. Should start at the value `1`, implying a lag of 1
#' in the dependent variable. Defaults to `NA`, communicating that you
#' don't want to allow for any AR effects.
#' @param x_lags Integer denoting the number of lagged covariate effects,
#' \eqn{q}, to estimate. Should starts at the value `0`, implying that only
#' the contemporaneous effect is taken into account. Defaults to `NA`,
#' communicating that you don't want to estimate any covariate parameters.
#' @param x Numeric vector denoting the values of covariate at each time point.
#' Depending on the input vectors supplied, different system responses will be
#' returned. Defaults to `NULL`, in which case the observed covariate values will
#' be used to decompose the observed data in its composite system responses.
#' @param innovations Numeric vector denoting the values of the innovations at
#' each time point. Depending on the input vectors supplied, different system
#' responses will be returned. Defaults to `NULL`, in which case the derived 
#' residuals of the observed data will be used to decompose the observed data in 
#' its composite system responses.
#' @param burnin Logical specifying if the part of the system response due to
#' the intercept should be burned in analytically. Defaults to `FALSE`, as
#' expected analytically. If set to `TRUE`, there will usually be
#' discrepancies between the model-implied total response \eqn{y_t}
#' and the observed values of the outcome variable \eqn{y_t^{obs}} at the start
#' of the time series.
#' @param confidence_interval Logical denoting whether to include the lower and 
#' upper bound of the \eqn{100 (1 - \alpha)}% confidence interval in the 
#' generated system responses. Confidence intervals will be computed using a
#' a bootstrap based on the estimated parameters and their standard errors.
#' Defaults to \code{FALSE}.
#' @param alpha Numeric between \code{0} and \code{1} denoting the specificity
#' of the confidence interval. Ignored if \code{confidence_interval = FALSE}. 
#' Defaults to \code{0.05}, leading to a 95% bootstrapped confidence interval.
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
#' @param ... Additional arguments provided to 
#' \code{\link[impulseR]{bootstrap}}, defining the procedure used to derive the 
#' confidence intervals
#'
#' @return List containing all information provided by
#' [irf_generator()] as well as the fitted model (under `"fit"`). Note that if
#' confidence intervals are computed, the data.frame provided under `"irf"` is
#' extended with these confidence intervals for each component, denoted by 
#' `"_lower"` and `"_upper"`.
#'
#' @examples
#' # Generate data
#' set.seed(1)
#' data <- irf_generator(
#'   intercept = 1,
#'   x_params = c(1, 2, -0.5),
#'   ar_params = c(0.75, -0.15, 0.05),
#'   x = rnorm(100),
#'   innovations = rnorm(100)
#' )$irf
#'
#' colnames(data) <- c("dependent", "independent")
#'
#' # Fit a lag-2 ADL model to the data and calculate the cumulative responses
#' result <- irf_empirical(
#'   data = data,
#'   cols = c("dependent", "independent"),
#'   y_lags = 2,
#'   x_lags = 2
#' )
#' 
#' # Fit an ADL(2, 2) and approximate the 99% confidence interval around the 
#' # cumulative responses through a bootstrap with 1000 samples
#' result <- irf_empirical(
#'   data = data,
#'   cols = c("dependent", "independent"),
#'   y_lags = 2,
#'   x_lags = 2,
#'   confidence_interval = TRUE,
#'   alpha = 0.01,
#'   N = 1000
#' )
#'
#' @export
irf_empirical <- function(
  data = NULL,
  cols = c("y", "x"),
  y_lags = NA,
  x_lags = NA,
  burnin = FALSE,
  x = NULL,
  innovations = NULL,
  confidence_interval = FALSE,
  alpha = 0.05,
  na_action = "listwise",
  ...
) {
  # Call the estimate function. The parameters are estimated and initial
  # innovations are fixed appropriately.
  params <- estimate(
    data,
    cols = cols,
    y_lags = y_lags,
    x_lags = x_lags,
    na_action = na_action
  )

  # Define the covariate effects and innovations to use. We need to create a 
  # new variable here to ensure that the bootstrap uses the correct input 
  # (a NULL when the innovations are not defined, the actual innovations when 
  # they are defined, and the same for the covariates)
  if(is.null(innovations)) {
    innovation_gen <- params$innovations
  } else {
    innovation_gen <- innovations
  }
  if(is.null(x)) {
    x_gen <- params$x
  } else {
    x_gen <- x
  }

  # Call the irf_generator function  on the observed covariate values and
  # estimated innovations:
  result <- irf_generator(
    intercept = params$intercept,
    x_params = params$x_params,
    ar_params = params$ar_params,
    x = x_gen,
    innovations = innovation_gen,
    burnin = burnin
  )
  result[["fit"]] <- params$fit

  # Check whether you have to approximate the confidence intervals through a 
  # bootstrap. If not, then return the results as is. If you do, then perform 
  # the bootstrap and adjust the results
  if(confidence_interval) {
    # Extract parameters, covariances of the parameters, and information on the 
    # lags. Importantly, the order of parameters matters and corresponds to the 
    # order used in the estimation! If not kept like this, we will have to 
    # specify the parameter names as well
    means <- c(
      params$intercept, 
      params$ar_params,
      params$x_params
    )
    covariances <- vcov(params$fit)

    y_lags <- length(params$ar_params)
    x_lags <- length(params$x_params) - 1

    # Perform the bootstrap
    samples <- bootstrap(
      means,
      covariances,
      y_lags = y_lags,
      x_lags = x_lags,
      x = x,
      innovations = innovations,
      data = data, 
      cols = cols,
      burnin = burnin,
      na_action = na_action,
      ...
    )

    # Use the provided alpha to approximate the confidence intervals through
    # finding the quantiles
    samples <- samples$samples |>
      dplyr::group_by(time) |>
      dplyr::summarize(
        dplyr::across(
          irf:irf_v, 
          list(
            "lower" = function(x) quantile(x, prob = alpha/2),
            "upper" = function(x) quantile(x, prob = 1 - alpha/2)
          )
        )
      ) |>
      dplyr::ungroup()

    # Combine with the data.frame in the results
    result$irf <- result$irf |>
      dplyr::full_join(
        samples,
        by = "time"
      )

    # Add information on the confidence intervals
    result$alpha <- alpha 
  }

  return(result)
}
