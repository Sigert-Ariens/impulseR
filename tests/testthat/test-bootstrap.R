test_that(
  "Testing known errors",
  {
    params <- rep(1, 5)

    # Covariances are not numeric matrix
    sigma <- matrix("test", nrow = 5, ncol = 5)
    expect_error(bootstrap(params, sigma))

    sigma <- matrix("0", nrow = 5, ncol = 5)
    expect_error(bootstrap(params, sigma))

    sigma <- rep(1, 5)
    expect_error(bootstrap(params, sigma))

    # Covariances are not positive definite

    # Covariances do not correspond to parameter vector
    sigma <- matrix(0, nrow = 5, ncol = 2)
    expect_error(bootstrap(params, sigma))

    sigma <- matrix(0, nrow = 2, ncol = 5)
    expect_error(bootstrap(params, sigma))

    # Lags do not correspond to parameter vector
    sigma <- diag(5)
    expect_error(bootstrap(params, sigma, y_lags = 2, x_lags = 3))
    expect_error(bootstrap(params, sigma, y_lags = 3, x_lags = 2))
    expect_error(bootstrap(params, sigma, y_lags = 1, x_lags = 1))
    expect_error(bootstrap(params, sigma, y_lags = 2, x_lags = 0))

    expect_no_error(bootstrap(params, sigma, y_lags = 2, x_lags = 1))
    expect_no_error(bootstrap(params, sigma, y_lags = 1, x_lags = 2))
    expect_no_error(bootstrap(params, sigma, y_lags = 3, x_lags = 0))
    expect_no_error(bootstrap(params, sigma, y_lags = 4, x_lags = NA))
    expect_no_error(bootstrap(params, sigma, y_lags = NA, x_lags = 3))

    # Parameter names do not correspond to parameter vector
    expect_error(bootstrap(params, sigma, parameter_names = rep("test", 4)))
    expect_error(bootstrap(params, sigma, parameter_names = rep("test", 6)))
  }
)

test_that(
  "Testing known warnings",
  {
    params <- rep(1, 5)
    sigma <- diag(5)

    # Impossible lags are specified
    expect_warning(bootstrap(params, sigma, y_lags = 0, x_lags = 3))
    expect_warning(bootstrap(params, sigma, y_lags = 4, x_lags = -1))
  }
)
