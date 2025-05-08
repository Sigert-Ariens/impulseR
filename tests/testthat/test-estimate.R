testthat::test_that(
  "Testing known errors",
  {
    # Wrong columns in estimate
    testthat::expect_error(
      impulseR::estimate(
        object = data.frame(Y = numeric(100), X = numeric(100)),
        cols = c("y", "x"),
        x_lags = 2,
        y_lags = 2
      )
    )

    # Wrong columns in compute_residuals
    testthat::expect_error(
      impulseR::compute_residuals(
        data.frame(Y = numeric(100), X = numeric(100),
        cols = c("y", "x"))
      )
    )

    # Wrong columns in prepare_data
    testthat::expect_error(
      impulseR::prepare_data(
        data.frame(Y = numeric(100), X = numeric(100),
        cols = c("y", "x"))
      )
    )
  }
)

testthat::test_that(
  "Testing known warnings",
  {
    # Create data to be used in this test
    data <- data.frame(y = rnorm(100), x = rnorm(100))

    na_data <- data 
    na_data$y[1:3] <- NA 
    na_data$x[4:6] <- NA 

    # No lags specified. Also testing output
    testthat::expect_warning(impulseR::prepare_data(data))

    tst <- suppressWarnings(impulseR::prepare_data(data))
    testthat::expect_equal(tst, data)

    # NAs in data in `estimate`. Also testing output
    testthat::expect_warning(impulseR::estimate(na_data))

    tst <- suppressWarnings(impulseR::estimate(na_data))
    testthat::expect_equal(tst$x, na_data$x[-c(1:6)])
    testthat::expect_equal(
      tst$residuals + tst$intercept, 
      na_data$y[-c(1:6)]
    )

    # NAs in data in `compute_residuals`. Also testing output
    testthat::expect_warning(
      impulseR::compute_residuals(
        na_data,
        ar_params = c(0.5, 0.25),
        x_params = c(2, 2)
      )
    )

    tst <- suppressWarnings(
      impulseR::compute_residuals(
        na_data, 
        ar_params = c(0.5, 0.25),
        x_params = c(2, 2)
      )
    )
    testthat::expect_equal(length(tst), 100 - 6)

    # NAs in data in `prepare_data`. Also testing output
    testthat::expect_warning(
      impulseR::prepare_data(
        na_data, 
        x_lags = 2, 
        y_lags = 2
      )
    )

    tst <- suppressWarnings(
      impulseR::prepare_data(
        na_data, 
        x_lags = 2, 
        y_lags = 2
      )
    )
    testthat::expect_equal(nrow(tst), 100 - 6)
  }
)

testthat::test_that(
  "Testing the recovery of estimation",
  {
    tst <- logical(length(parameters))

    # Loop over each of the parameters
    set.seed(1)
    for(i in seq_along(parameters)) {
      # Generate data
      y <- impulseR::irf(
        intercept = parameters[[i]]$intercept,
        x_params = parameters[[i]]$x,
        ar_params = parameters[[i]]$eps,
        x = rnorm(1000),
        residuals = rnorm(1000)
      )

      # Estimate the parameters
      results <- impulseR::estimate(
        y, 
        cols = c("irf", "x"),
        y_lags = est_lags[i, 1],
        x_lags = est_lags[i, 2]
      )

      # Check whether all of the parameters are smaller than a given tolerance
      # level
      params <- c(
        parameters[[i]]$intercept - results$intercept, 
        parameters[[i]]$x_params - results$x_params, 
        parameters[[i]]$ar_params - results$ar_params
      )

      tst[i] <- all(params <= 10^(-1))
    }

    # Do the actual check
    testthat::expect_true(all(tst))
  }
)

testthat::test_that(
  "Testing creation of initial conditions",
  {
    lags <- c(NA, 1, 2, 3, 4)
    lags <- data.frame(
      y_lags = rep(lags, each = length(lags)),
      x_lags = rep(lags, times = length(lags))
    )

    tst_y <- tst_x <- matrix(
      FALSE,
      nrow = length(parameters),
      ncol = nrow(lags)
    )

    # Idea behind this test: We should be able to exactly replicate the observed
    # data `y` if we correctly compute the residuals in `estimate`. Residuals 
    # and other impulse response functions may deviate, however, due to nonexact
    # recovery of the parameters.
    #
    # To make this point even clearer, This analysis is done for different types
    # of models that do not necessarily correspond to the original generating 
    # model
    for(i in seq_along(parameters)) {
      # Generate data
      y <- impulseR::irf(
        intercept = parameters[[i]]$intercept,
        x_params = parameters[[i]]$x,
        ar_params = parameters[[i]]$eps,
        x = rnorm(100),
        residuals = rnorm(100)
      )

      # Loop over all possibilities of the lags
      for(j in seq_len(nrow(lags))) {
        # Estimate the parameters and retrieve the results
        results <- impulseR::estimate(
          y, 
          cols = c("irf", "x"),
          y_lags = lags$y_lags[j],
          x_lags = ifelse(is.na(lags$x_lags[j]), NA, lags$x_lags[j] - 1)
        )

        # Create the impulse response functions
        results <- impulseR::irf(
          intercept = results$intercept,
          x_params = results$x_params,
          ar_params = results$ar_params,
          x = results$x,
          residuals = results$residuals,
          burnin = FALSE
        )

        # Check whether `irf` corresponds to `y`, and whether `x` corresponds to
        # `x`
        tst_y[i, j] <- all(results$irf == y$y)
        tst_x[i, j] <- all(results$x == y$x)
      }
    }

    # Do the actual check
    testthat::expect_true(all(tst_y))
    testthat::expect_true(all(tst_x))
  }
)

testthat::test_that(
  "Testing creation of initial conditions without any estimated ones",
  {
    lags <- c(NA, 1, 2, 3, 4)
    lags <- data.frame(
      y_lags = rep(lags, each = length(lags)),
      x_lags = rep(lags, times = length(lags))
    )

    tst <- matrix(
      FALSE,
      nrow = length(parameters),
      ncol = nrow(lags)
    )

    # Idea behind this test: We should be able to exactly replicate the observed
    # data `y` if we correctly compute the residuals in `estimate`. Residuals 
    # and other impulse response functions may deviate, however, due to nonexact
    # recovery of the parameters.
    #
    # To make this point even clearer, This analysis is done for different types
    # of models that do not necessarily correspond to the original generating 
    # model
    for(i in seq_along(parameters)) {
      # Generate data
      y <- impulseR::irf(
        intercept = parameters[[i]]$intercept,
        x_params = parameters[[i]]$x,
        ar_params = parameters[[i]]$eps,
        x = rnorm(100),
        residuals = rnorm(100)
      )

      # Loop over all possibilities of the lags
      for(j in seq_len(nrow(lags))) {
        # Estimate the parameters and retrieve the results
        results <- impulseR::estimate(
          y, 
          cols = c("irf", "x"),
          y_lags = lags$y_lags[j],
          x_lags = ifelse(is.na(lags$x_lags[j]), NA, lags$x_lags[j] - 1)
        ) |>
          suppressWarnings()

        # Create the impulse response functions
        residuals <- impulseR::compute_residuals(
          y, 
          cols = c("irf", "x"),
          intercept = results$intercept,
          x_params = results$x_params,
          ar_params = results$ar_params,
          residuals = NULL
        ) |>
          suppressWarnings()

        # Check whether the computed residuals are the same in both cases
        tst[i, j] <- all(round(residuals, 4) == round(results$residuals, 4))
      }
    }

    # Do the actual check
    testthat::expect_true(all(tst))
  }
)
