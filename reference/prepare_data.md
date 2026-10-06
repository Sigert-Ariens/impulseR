# Prepare data for analysis

Prepare data for analysis

## Usage

``` r
prepare_data(
  data,
  cols = c("y", "x"),
  x_lags = NULL,
  y_lags = NULL,
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

- x_lags:

  Integer denoting the number of lags to include for the covariate.
  Starts at the value `0`. Defaults to `NA`, communicating that you
  don't want to use any lagged values of the dependent variable

- y_lags:

  Integer denoting the number of lags to include for the dependent
  variable. Starts at the value `1`. Defaults to `NA`, communicating
  that you don't want to use any lagged values of the dependent variable

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

Named list containing the values of the dependent variable (`"y"`) and a
matched numeric matrix containing the values of the independent and
dependent variables that are used as predictors in the estimation
routine.

## Details

Transforms a data.frame in such a way that we can estimate the
parameters for the specified number of lags in y and x.

Used under the hood in the
[`estimate()`](https://Sigert-Ariens.github.io/impulseR/reference/estimate.md)
function.

## Examples

``` r
# Define a dataset
x <- rnorm(100)
data <- data.frame(
  DV = 1 + 2 * x + rnorm(100),
  IV = x
)

# For these data, prepare the data for estimation for an ADL(2, 2)
prepared_data <- prepare_data(
  data,
  cols = c("DV", "IV"),
  y_lags = 2,
  x_lags = 2
)
```
