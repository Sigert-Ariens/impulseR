testthat::test_that(
  "Testing known errors",
  {
    testthat::expect_error(impulseR::estimate(
      object = data.frame(Y = numeric(100), X = numeric(100)),
      cols = c("y", "x"),
      x_lags = 2,
      y_lags = 2
    ))
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
