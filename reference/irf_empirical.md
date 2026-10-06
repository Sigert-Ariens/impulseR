# Estimate and compute system responses for an \\ADL(p, q)\\

This function can be used to construct empirical trajectory plots.
First, an \\ADL(p, q)\\ model is fitted to the data using the
[`estimate()`](https://Sigert-Ariens.github.io/impulseR/reference/estimate.md)
function, where \\p\\ is the number of AR parameters and \\q\\ the
number of lagged covariate effects. Then, the function creates
cumulative responses through the
[`irf_generator()`](https://Sigert-Ariens.github.io/impulseR/reference/irf_generator.md)
function.

## Usage

``` r
irf_empirical(
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
)
```

## Arguments

- data:

  Dataframe containing the variables of interest

- cols:

  Character vector denoting the columns containing the variables of
  interest. First character should denote the dependent variable, the
  second one the covariate of interest. Defaults to `c("y", "x")`.

- y_lags:

  Integer denoting the number of AR effects, \\p\\, to estimate. Should
  start at the value `1`, implying a lag of 1 in the dependent variable.
  Defaults to `NA`, communicating that you don't want to allow for any
  AR effects.

- x_lags:

  Integer denoting the number of lagged covariate effects, \\q\\, to
  estimate. Should starts at the value `0`, implying that only the
  contemporaneous effect is taken into account. Defaults to `NA`,
  communicating that you don't want to estimate any covariate
  parameters.

- burnin:

  Logical specifying if the part of the system response due to the
  intercept should be burned in analytically. Defaults to `FALSE`, as
  expected analytically. If set to `TRUE`, there will usually be
  discrepancies between the model-implied total response \\y_t\\ and the
  observed values of the outcome variable \\y_t^{obs}\\ at the start of
  the time series.

- x:

  Numeric vector denoting the values of covariate at each time point.
  Depending on the input vectors supplied, different system responses
  will be returned. Defaults to `NULL`, in which case the observed
  covariate values will be used to decompose the observed data in its
  composite system responses.

- innovations:

  Numeric vector denoting the values of the innovations at each time
  point. Depending on the input vectors supplied, different system
  responses will be returned. Defaults to `NULL`, in which case the
  derived residuals of the observed data will be used to decompose the
  observed data in its composite system responses.

- confidence_interval:

  Logical denoting whether to include the lower and upper bound of the
  \\100 (1 - \alpha)\\% confidence interval in the generated system
  responses. Confidence intervals will be computed using a a bootstrap
  based on the estimated parameters and their standard errors. Defaults
  to `FALSE`.

- alpha:

  Numeric between `0` and `1` denoting the specificity of the confidence
  interval. Ignored if `confidence_interval = FALSE`. Defaults to
  `0.05`, leading to a 95% bootstrapped confidence interval.

- na_action:

  Character denoting how `NA`s should be removed from the data. Either
  `"listwise"`, `"casewise"`, `"pairwise"`, or `"partial"`. Listwise and
  casewise deletion consists of deletion of full rows of data when one
  or more of the the matched variables contains an `NA`, including the
  values of the lagged variables. This method may lead to a lot of
  deleted data, especially when estimating \\ADL\\s with many lags in
  their predictor variables. To alleviate this difficulty, we also allow
  users to specify partial deletion – the deletion of rows when `NA` is
  found in the contemporaneous values of the variables, meaning you
  bridge the gap created by `NA`s – or pairwise deletion – the pairwise
  use of for the estimation of the relevant parameters whenever they are
  not `NA`, not implemented yet. Defaults to `"listwise"`.

- ...:

  Additional arguments provided to
  [`bootstrap`](https://Sigert-Ariens.github.io/impulseR/reference/bootstrap.md),
  defining the procedure used to derive the confidence intervals

## Value

List containing all information provided by
[`irf_generator()`](https://Sigert-Ariens.github.io/impulseR/reference/irf_generator.md)
as well as the fitted model (under `"fit"`). Note that if confidence
intervals are computed, the data.frame provided under `"irf"` is
extended with these confidence intervals for each component, denoted by
`"_lower"` and `"_upper"`.

## Details

Estimation is currently performed through OLS, using the base
[`stats::lm()`](https://rdrr.io/r/stats/lm.html) function.

## Examples

``` r
# Generate data
set.seed(1)
data <- irf_generator(
  intercept = 1,
  x_params = c(1, 2, -0.5),
  ar_params = c(0.75, -0.15, 0.05),
  x = rnorm(100),
  innovations = rnorm(100)
)$irf

colnames(data) <- c("dependent", "independent")

# Fit a lag-1 ADL model to the data and calculate the cumulative responses
result <- irf_empirical(
  data = data,
  cols = c("dependent", "independent"),
  y_lags = 1,
  x_lags = 1
)
#> Warning: essentially perfect fit: summary may be unreliable

# Fit an ADL(1, 1) and approximate the 99% confidence interval around the 
# cumulative responses through a bootstrap with 1000 samples
result <- irf_empirical(
  data = data,
  cols = c("dependent", "independent"),
  y_lags = 1,
  x_lags = 1,
  confidence_interval = TRUE,
  alpha = 0.01,
  N = 1000
)
#> Warning: essentially perfect fit: summary may be unreliable
#> Warning: essentially perfect fit: summary may be unreliable
```
