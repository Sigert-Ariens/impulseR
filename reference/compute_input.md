# Compute estimated innovations of an \\ADL(p,q)\\ model

This function computes the innovations of an \\ADL(p,q)\\ model, fixing
initial innovations in order to construct empirical trajectory plots.

## Usage

``` r
compute_input(
  data,
  cols = c("y", "x"),
  intercept = 0,
  ar_params = 0,
  x_params = 0,
  innovations = NULL,
  na_action = "listwise"
)
```

## Arguments

- data:

  Dataframe containing the variables of interest

- cols:

  Character vector denoting the columns containing the variables of
  interest. First character should denote the dependent variable, the
  second one the covariate of interest. Defaults to `c("y", "x")`.

- intercept:

  Numeric denoting the intercept to use for the impulse response
  function. Defaults to `0`.

- ar_params:

  Numeric vector denoting the estimated AR parameters. Parameters need
  to be given in order of increased lag (i.e., first element for
  \\\phi_1\\, second element for \\\phi_2\\,...). Defaults to `0`.

- x_params:

  Numeric vector denoting the estimated covariate parameters. Parameters
  again need to be given in order of increased lag (i.e., first element
  for \\\beta_x\\, second element for \\\beta\_{Lx}\\,...). Defaults to
  `0`, implying no covariate parameters.

- innovations:

  Numeric vector denoting the values of estimated innovations up to time
  point \\t\\ (e.g., those acquired through the `lm`. function).
  Defaults to `NULL`, signaling the estimated innovations are first
  calculated from the parameter estimates and observed data values \\y\\
  and \\x\\.

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

## Value

Numeric vector of the same length as the data containing the innovations
for the provided model

## Details

If there are \\p\\ AR effects, the fit object will only return
innovations for the last `N - p` datapoints. Least squares treats the
initial values, \\y_0,...,y_p\\ and \\x_0,...,x_p\\ as known for
parameter estimation. To generate empirical trajectory plots starting
from the first measurement occasion (\\t = 0\\), we can fix the
'missing' innovations by using knowledge of the initial values and the
parameter estimates.

This function fixes the initial innovations appropriately for the user,
and returns a list containing all `N` innovations. This function is
primarily used under the hood of the
[`estimate()`](https://Sigert-Ariens.github.io/impulseR/reference/estimate.md)
and
[`irf_generator()`](https://Sigert-Ariens.github.io/impulseR/reference/irf_generator.md)
function.

## Examples

``` r
# Define a dataset
x <- rnorm(100)
data <- data.frame(
  DV = 1 + 2 * x + rnorm(100),
  IV = x
)

# For these data, compute the innovations of an ADL(2, 2)
innovations <- compute_input(
  data,
  cols = c("DV", "IV"),
  intercept = 0.5,
  ar_params = c(0.75, 0.25),
  x_params = c(1.5, 0.25)
)
```
