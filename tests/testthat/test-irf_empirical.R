# Test expected warnings
testthat::test_that(
  "Testing expected warning: NAs in the data",
  {
    # Create a data set and invoke some NAs
    data <- impulseR::irf_generator(
        intercept = 1,
        ar_params = c(0.7, 0.2),
        x_params = c(2, 2),
        x = rnorm(100),
        innovations = rnorm(100)
    )$irf 

    data$x[seq(1, 100, 10)] <- NA 

    # Do the test
    testthat::expect_warning(impulseR::irf_empirical(data = data, cols = c("irf", "x")))

    # Test of the output can be added here, but is already being tested elsewhere
  }
)

# Test properties of the output.
testthat::test_that(
  "Testing properties of output",
  {
    # Create a data set to be used
    data <- impulseR::irf_generator(
        intercept = 1,
        ar_params = c(0.7, 0.2),
        x_params = c(2, 2),
        x = rnorm(100),
        innovations = rnorm(100)
    )$irf 

    # Use irf_empirical and extract the information of interest
    tst <- impulseR::irf_empirical(
        data = data, 
        cols = c("irf", "x"),
        x_lags = 0, 
        y_lags = 1
    )

    irf <- tst$irf 
    fit <- tst$fit

    # Tests for irf
    testthat::expect_true(is.data.frame(irf))
    testthat::expect_equal(
      colnames(irf),
      c("time", "irf", "irf_intercept", "irf_x", "irf_v", "x", "innovations")
    )
    testthat::expect_equal(nrow(irf), 100)

    # Tests for fit
    testthat::expect_true(is.list(fit))
    testthat::expect_equal(
        names(fit),
        c("coefficients", "residuals", "effects", "rank", "fitted.values", "assign",
          "qr", "df.residual", "xlevels", "call", "terms", "model")
    )
    testthat::expect_equal(length(fit$coefficients), 3)
  }
)

testthat::test_that(
  "Testing estimation through the irf function",
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
    # data `y` if we correctly compute the innovations in `estimate`. innovations 
    # and other impulse response functions may deviate, however, due to nonexact
    # recovery of the parameters.
    #
    # To make this point even clearer, This analysis is done for different types
    # of models that do not necessarily correspond to the original generating 
    # model
    for(i in seq_along(parameters)) {
      # Generate data
      y <- impulseR::irf_generator(
        intercept = parameters[[i]]$intercept,
        x_params = parameters[[i]]$x,
        ar_params = parameters[[i]]$eps,
        x = rnorm(100),
        innovations = rnorm(100)
      )$irf

      # Loop over all possibilities of the lags
      for(j in seq_len(nrow(lags))) {
        # Estimate the parameters and retrieve the results
        results <- impulseR::irf_empirical(
          data = y, 
          cols = c("irf", "x"),
          y_lags = lags$y_lags[j],
          x_lags = ifelse(is.na(lags$x_lags[j]), NA, lags$x_lags[j] - 1)
        )$irf

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

# Currently not supported, but might be useful to put it back in at some point
# testthat::test_that(
#   "Testing estimation of parameters and providing own impulses",
#   {
#     lags <- c(NA, 1, 2, 3, 4)
#     lags <- data.frame(
#       y_lags = rep(lags, each = length(lags)),
#       x_lags = rep(lags, times = length(lags))
#     )

#     tst_y <- tst_x <- matrix(
#       FALSE,
#       nrow = length(parameters),
#       ncol = nrow(lags)
#     )

#     # Idea behind this test: We should be able to exactly replicate the observed
#     # data `y` if we correctly compute the innovations in `estimate`. innovations 
#     # and other impulse response functions may deviate, however, due to nonexact
#     # recovery of the parameters.
#     #
#     # To make this point even clearer, This analysis is done for different types
#     # of models that do not necessarily correspond to the original generating 
#     # model
#     set.seed(1)
#     for(i in seq_along(parameters)) {
#       # Generate data
#       y <- impulseR::irf(
#         intercept = parameters[[i]]$intercept,
#         x_params = parameters[[i]]$x,
#         ar_params = parameters[[i]]$eps,
#         x = rnorm(100),
#         innovations = rnorm(100)
#       )$irf

#       # Loop over all possibilities of the lags
#       for(j in seq_len(nrow(lags))) {
#         # Create current impulses from a normal distribution. Provides a stronger
#         # check for whether the correct information is passed on to the 
#         # correct functions. 
#         impulses <- rnorm(100)

#         # Estimate the parameters and retrieve the results
#         results <- impulseR::irf(
#           y, 
#           cols = c("irf", "x"),
#           y_lags = lags$y_lags[j],
#           x_lags = ifelse(is.na(lags$x_lags[j]), NA, lags$x_lags[j] - 1),
#           x = impulses,
#           innovations = impulses
#         )$irf

#         # Check whether `x` and `innovations` are correctly passed on
#         tst_y[i, j] <- all(results$x == impulses)
#         tst_x[i, j] <- all(results$innovations == impulses)
#       }
#     }

#     # Do the actual check
#     testthat::expect_true(all(tst_y))
#     testthat::expect_true(all(tst_x))
#   }
# )