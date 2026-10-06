# Estimate the parameters of an \\ADL(p, q)\\ model

This function estimates the parameters of an \\ADL(p, q)\\ and prepares
the output for the calculation of system responses in other functions of
the package. Always estimates an intercept.

## Usage

``` r
estimate(
  data,
  cols = c("y", "x"),
  y_lags = NULL,
  x_lags = NULL,
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

- y_lags:

  Integer denoting the number of AR effects to estimate freely. Starts
  at the value `1`, allowing for an AR(1) structure. Defaults to `NA`,
  communicating that you don't want to allow for any AR effects.

- x_lags:

  Integer denoting the number of lagged covariate effects. Starts at the
  value `0`, implying that only the contemporaneous effect, \\\beta_x\\,
  is estimated freely. Defaults to `NA`, communicating that you don't
  want to estimate any covariate parameters

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

Named list containing the estimated model (`fit`), the parameter
estimates (`intercept`, `x_params`, `ar_params`), the observed covariate
values (`x`) and the estimated innovations `innovations`

## Details

Estimate the parameters of an \\ADL(p, q)\\, where \\p\\ represents the
number of autoregressive effects (effects of lags of the dependent
variable \\y_t\\ and \\q\\ represents the number of lagged covariate
effects. The \\ADL(p, q)\\ is formalized as:

\$\$y_t = \alpha + \sum\_{i = 1}^p \phi\_{i}y\_{t-i} + \beta\_{x}
x\_{t} + \sum\_{i = 1}^q \beta\_{L^{j}x} x\_{t - j} + v\_{t}\$\$

## Examples

``` r
# Define a dataset
x <- rnorm(100)
data <- data.frame(
  DV = 1 + 2 * x + rnorm(100),
  IV = x
)

# Estimate an ADL(2, 2)
estimate(
  data,
  cols = c("DV", "IV"),
  y_lags = 2,
  x_lags = 2
)
#> $fit
#> 
#> Call:
#> stats::lm(formula = y ~ X__)
#> 
#> Coefficients:
#> (Intercept)    X__ylag_1    X__ylag_2    X__xlag_0    X__xlag_1    X__xlag_2  
#>    0.898840    -0.003956     0.087453     2.035333     0.019286    -0.065494  
#> 
#> 
#> $intercept
#> [1] 0.8988403
#> 
#> $ar_params
#> [1] -0.003956095  0.087453448
#> 
#> $x_params
#> [1]  2.03533346  0.01928573 -0.06549435
#> 
#> $x
#>   [1]  0.401326040  0.893820730  1.520416011  1.323237794 -0.408808670
#>   [6] -0.548744384 -0.654509719 -0.648960863 -0.073765129  0.400341214
#>  [11] -1.870165426  0.034155566 -0.573507652 -0.861220997 -0.969873014
#>  [16] -1.367086380  0.001955397  0.320311581  1.277729506 -1.519922490
#>  [21] -0.795869028  0.023281847  0.943611763  0.417835156 -0.961517136
#>  [26]  0.209701793 -0.340457428 -0.611354577 -0.523541190 -1.040006787
#>  [31]  1.525558158  0.951102592  0.330333286 -0.701780130  0.857834900
#>  [36] -0.251356046 -0.203244368 -0.514577212 -0.312994780  1.204892038
#>  [41] -0.577936882  0.153315192 -1.384527992  1.801380822  0.184980837
#>  [46]  0.164361865  1.079078339 -0.043309595  0.174072463 -0.323891849
#>  [51] -0.727434380  1.044550781  1.275684850 -1.046887761  1.196067015
#>  [56]  0.816798828 -0.478966603 -0.588460611  0.391109752  0.473150354
#>  [61] -0.453113205 -0.127477565  1.346941131  0.591315433  0.326371227
#>  [66]  0.351518394 -2.721735075 -1.512743923  0.040215638 -1.717848252
#>  [71] -0.214734035 -0.194580334  1.508217745  0.854064841  0.708336137
#>  [76]  1.134016249  1.856987231  1.193324371  0.640406482 -0.197165685
#>  [81]  0.892797358 -1.317458927  0.912994876  0.447892048  0.084610879
#>  [86]  1.278956610 -0.059177121  1.059832770  1.017867165 -1.132071680
#>  [91] -0.829434541 -0.747846029 -0.510617743 -1.701340460 -0.263726924
#>  [96] -0.952922703 -1.691789730 -1.027854545 -0.214893700 -1.614187631
#> 
#> $innovations
#>   [1] -0.53655207  1.55200715  0.97013056  0.90250404  1.19168007  0.95075698
#>   [7]  0.51401686  0.54474087  0.97860243  1.00198990 -1.25955219  0.73088625
#>  [13] -0.45881812  1.08350570 -2.56366569  0.20613022 -0.35721417 -0.96915226
#>  [19] -1.21836990  0.16581148 -0.76686980  0.15007996  0.39858642  0.08509747
#>  [25]  2.71821689  1.44035991  0.15236011  1.26919687 -0.79391263 -1.12598702
#>  [31]  0.12669253  0.83251450  0.82309222 -0.79099366  0.83430708  3.60941200
#>  [37] -1.77029059 -0.37952066  0.03066191  0.34957336 -0.12599271  1.29819199
#>  [43] -0.95381698  0.77908156 -1.62309941 -2.38803299  0.70172702  0.94699371
#>  [49] -1.43947383 -0.77001215  0.08935074  0.26755054 -0.47116837  2.58630974
#>  [55] -1.58621777 -0.09067363  0.01129020 -1.02928349  0.83947325  1.15501916
#>  [61]  0.09114424 -0.79296898 -0.56537259 -0.92859205 -1.63204569 -1.87884320
#>  [67]  0.20733231  1.51048970 -0.68721160  0.13909297 -0.24152427 -0.15354303
#>  [73] -0.11217276 -0.33706407  0.09703318 -0.70328271 -0.10468658 -0.03837866
#>  [79] -1.04007061 -0.41644182  1.09847865 -0.63745432 -0.44273439  0.09845700
#>  [85] -0.63691589 -0.27493104 -0.24012984  0.82046409  0.32475465  0.73911942
#>  [91] -1.38145612  0.08510405 -0.66901597  0.23584621  1.04539915 -0.28412148
#>  [97] -0.27198008  1.08772090 -0.25782121 -0.65545409
#> 
```
