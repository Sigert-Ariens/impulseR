# Diagnostic plots for \\ADL(p, q)\\

Diagnostic plots meant to provide information on the fit of the model to
data. Consist of (a) residuals plotted against fitted values, (b)
autocorrelation plots for the residuals, and (c) a time-series plot of
the residuals.

## Usage

``` r
diag_plot(fit)
```

## Arguments

- fit:

  Fit object as provided by the output of
  [`irf_empirical`](https://Sigert-Ariens.github.io/impulseR/reference/irf_empirical.md)
  under the label `"fit"`

## Value

Plots visualizing several indices of fit

## Examples

``` r
# Generate data to be evaluated
x <- rnorm(100)
y <- 1 + 2 * x - x^2

# Perform a linear regression analysis
fit <- lm(y ~ x)

# Create diagnostic plots
diag_plot(fit)

#> NULL
```
