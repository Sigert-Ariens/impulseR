# Test expected warnings
test_that(
  "Testing expected warning: NAs in the data",
  {
    # Create a data set and invoke some NAs
    data <- irf_generator(
      intercept = 1,
      ar_params = c(0.7, 0.2),
      x_params = c(2, 2),
      x = rnorm(100),
      innovations = rnorm(100)
    )$irf

    data$x[seq(1, 100, 10)] <- NA

    # Do the test
    expect_warning(
      irf_empirical(
        data = data, 
        cols = c("irf", "x"),
        na_action = "listwise"
      )
    )
    expect_warning(
      irf_empirical(
        data = data, 
        cols = c("irf", "x"),
        na_action = "casewise"
      )
    )

    # Test of the output can be added here, but is already being tested elsewhere
  }
)

# Test properties of the output.
test_that(
  "Testing properties of output",
  {
    # Create a data set to be used
    data <- irf_generator(
      intercept = 1,
      ar_params = c(0.7, 0.2),
      x_params = c(2, 2),
      x = rnorm(100),
      innovations = rnorm(100)
    )$irf

    # Use irf_empirical and extract the information of interest
    tst <- irf_empirical(
      data = data,
      cols = c("irf", "x"),
      x_lags = 0,
      y_lags = 1
    )

    irf <- tst$irf
    fit <- tst$fit

    # Tests for irf
    expect_true(is.data.frame(irf))
    expect_equal(
      colnames(irf),
      c("time", "irf", "irf_intercept", "irf_x", "irf_v", "x", "innovations")
    )
    expect_equal(nrow(irf), 100)

    # Tests for fit
    expect_true(is.list(fit))
    expect_equal(
      names(fit),
      c(
        "coefficients", "residuals", "effects", "rank", "fitted.values", "assign",
        "qr", "df.residual", "xlevels", "call", "terms", "model"
      )
    )
    expect_equal(length(fit$coefficients), 3)
  }
)

test_that(
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
    for (i in seq_along(parameters)) {
      # Generate data
      y <- irf_generator(
        intercept = parameters[[i]]$intercept,
        x_params = parameters[[i]]$x,
        ar_params = parameters[[i]]$eps,
        x = rnorm(100),
        innovations = rnorm(100)
      )$irf

      # Loop over all possibilities of the lags
      for (j in seq_len(nrow(lags))) {
        # Estimate the parameters and retrieve the results
        results <- irf_empirical(
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
    expect_true(all(tst_y))
    expect_true(all(tst_x))
  }
)

test_that(
  "Testing properties of the output: Bootstrapping confidence intervals for data",
  {
    intercept <- 1
    ar_params <- c(0.5, 0.1, -.1)
    x_params <- c(2, 1, -0.5)

    # Generate a dataset
    set.seed(1)
    y <- irf_generator(
      intercept = intercept,
      ar_params = ar_params,
      x_params = x_params, 
      x = rnorm(100),
      innovations = rnorm(100)
    )$irf

    # Estimate an ADL(1, 1) on these data and compute the bootstrapped interval 
    # as well. Do so for 90%, 95%, and 99%
    set.seed(1)
    results_90 <- irf_empirical(
      y, 
      cols = c("irf", "x"),
      y_lags = 1,
      x_lags = 1,
      confidence_interval = TRUE,
      alpha = 0.10,
      N = 1000
    )

    set.seed(1)
    results_95 <- irf_empirical(
      y, 
      cols = c("irf", "x"),
      y_lags = 1,
      x_lags = 1,
      confidence_interval = TRUE,
      alpha = 0.05,
      N = 1000
    )

    set.seed(1)
    results_99 <- irf_empirical(
      y, 
      cols = c("irf", "x"),
      y_lags = 1,
      x_lags = 1,
      confidence_interval = TRUE,
      alpha = 0.01,
      N = 1000
    )

    # Test: Check whether the confidence intervals are larger for the lower values
    # of alpha
    expect_true(
      all(
        sapply(
          c("irf_intercept_lower", "irf_x_lower", "irf_v_lower"),
          function(x) results_99$irf[, x] <= results_95$irf[, x]
        )
      )
    )
    expect_true(
      all(
        sapply(
          c("irf_intercept_lower", "irf_x_lower", "irf_v_lower"),
          function(x) results_99$irf[, x] <= results_90$irf[, x]
        )
      )
    )
    expect_true(
      all(
        sapply(
          c("irf_intercept_lower", "irf_x_lower", "irf_v_lower"),
          function(x) results_95$irf[, x] <= results_90$irf[, x]
        )
      )
    )

    # Test: Check whether the confidence intervals contain the actual system 
    # responses
    expect_true(
      all(
        sapply(
          c("irf_intercept", "irf_x", "irf_v"),
          function(x) 
            results_99$irf[, x] <= results_99$irf[, paste0(x, "_upper")] & 
            results_99$irf[, x] >= results_99$irf[, paste0(x, "_lower")]
        )
      )
    )
    expect_true(
      all(
        sapply(
          c("irf_intercept", "irf_x", "irf_v"),
          function(x) 
            results_95$irf[, x] <= results_95$irf[, paste0(x, "_upper")] & 
            results_95$irf[, x] >= results_95$irf[, paste0(x, "_lower")]
        )
      )
    )
    expect_true(
      all(
        sapply(
          c("irf_intercept", "irf_x", "irf_v"),
          function(x) 
            results_90$irf[, x] <= results_90$irf[, paste0(x, "_upper")] & 
            results_90$irf[, x] >= results_90$irf[, paste0(x, "_lower")]
        )
      )
    )
  }
)

test_that(
  "Testing properties of the output: Bootstrapping confidence intervals for impulses",
  {
    intercept <- 1
    ar_params <- c(0.5, 0.1, -.1)
    x_params <- c(2, 1, -0.5)

    # Generate a dataset
    set.seed(1)
    y <- irf_generator(
      intercept = intercept,
      ar_params = ar_params,
      x_params = x_params, 
      x = rnorm(100),
      innovations = rnorm(100)
    )$irf

    # Estimate an ADL(1, 1) on these data and compute the bootstrapped interval 
    # as well. Do so for 90%, 95%, and 99%
    set.seed(1)
    results_90 <- irf_empirical(
      y, 
      cols = c("irf", "x"),
      y_lags = 1,
      x_lags = 1,
      x = impulse(10),
      innovations = impulse(10),
      confidence_interval = TRUE,
      alpha = 0.10,
      N = 1000
    )

    set.seed(1)
    results_95 <- irf_empirical(
      y, 
      cols = c("irf", "x"),
      y_lags = 1,
      x_lags = 1,
      x = impulse(10),
      innovations = impulse(10),
      confidence_interval = TRUE,
      alpha = 0.05,
      N = 1000
    )

    set.seed(1)
    results_99 <- irf_empirical(
      y, 
      cols = c("irf", "x"),
      y_lags = 1,
      x_lags = 1,
      x = impulse(10),
      innovations = impulse(10),
      confidence_interval = TRUE,
      alpha = 0.01,
      N = 1000
    )

    # Test: Check whether the confidence intervals are larger for the lower values
    # of alpha
    expect_true(
      all(
        sapply(
          c("irf_lower", "irf_intercept_lower", "irf_x_lower", "irf_v_lower"),
          function(x) results_99$irf[, x] <= results_95$irf[, x]
        )
      )
    )
    expect_true(
      all(
        sapply(
          c("irf_lower", "irf_intercept_lower", "irf_x_lower", "irf_v_lower"),
          function(x) results_99$irf[, x] <= results_90$irf[, x]
        )
      )
    )
    expect_true(
      all(
        sapply(
          c("irf_lower", "irf_intercept_lower", "irf_x_lower", "irf_v_lower"),
          function(x) results_95$irf[, x] <= results_90$irf[, x]
        )
      )
    )

    # Test: Check whether the confidence intervals contain the actual system 
    # responses
    expect_true(
      all(
        sapply(
          c("irf", "irf_intercept", "irf_x", "irf_v"),
          function(x) 
            results_99$irf[, x] <= results_99$irf[, paste0(x, "_upper")] & 
            results_99$irf[, x] >= results_99$irf[, paste0(x, "_lower")]
        )
      )
    )
    expect_true(
      all(
        sapply(
          c("irf", "irf_intercept", "irf_x", "irf_v"),
          function(x) 
            results_95$irf[, x] <= results_95$irf[, paste0(x, "_upper")] & 
            results_95$irf[, x] >= results_95$irf[, paste0(x, "_lower")]
        )
      )
    )
    expect_true(
      all(
        sapply(
          c("irf", "irf_intercept", "irf_x", "irf_v"),
          function(x) 
            results_90$irf[, x] <= results_90$irf[, paste0(x, "_upper")] & 
            results_90$irf[, x] >= results_90$irf[, paste0(x, "_lower")]
        )
      )
    )
  }
)

test_that(
  "Check presence of bug: Results from estimation are same within and outside of irf_empirical",
  {
    # Create data that can be used for estimation
    set.seed(1)
    data <- irf_generator(
      intercept = 0, 
      ar_params = c(0.75, 0.25, -0.1),
      x_params = c(2, 1, 0.5),
      x = rnorm(50),
      innovations = rnorm(50)
    )$irf |>
      dplyr::select(irf, x) |>
      dplyr::rename(y = irf)

    # Perform estimation using the lag-function and estimate the parameters of 
    # this model outside of the irf_empirical function
    lag <- dplyr::lag
    ref <- lm(
      data = data,
      y ~ lag(y, 1) + lag(y, 2) + x + lag(x, 1) + lag(x, 2)
    )

    # Perform estimation through the irf_empirical function and extract the 
    # results of the estimation procedure
    tst <- irf_empirical(
      data = data, 
      cols = c("y", "x"),
      y_lags = 2,
      x_lags = 2,
      burnin = FALSE
    )$fit |>
      suppressWarnings()

    # Test how well each aspect of the results corresponds between the two 
    # methods
    expect_equal(
      as.matrix(ref$coefficients) |>
        `dimnames<-`(NULL), 
      as.matrix(tst$coefficients) |>
        `dimnames<-`(NULL)
    )

    expect_equal(
      as.numeric(ref$residuals), 
      as.numeric(tst$residuals)
    )

    # Test how well each aspect of the summary holds up between the two methods
    ref <- summary(ref)
    tst <- summary(tst)

    expect_equal(
      as.matrix(ref$coefficients) |>
        `dimnames<-`(NULL),
      as.matrix(tst$coefficients) |>
        `dimnames<-`(NULL)
    )

    expect_equal(
      as.matrix(ref$cov.unscaled) |>
        `dimnames<-`(NULL),
      as.matrix(tst$cov.unscaled) |>
        `dimnames<-`(NULL)
    )

    expect_equal(
      ref$r.squared,
      tst$r.squared
    )

    expect_equal(
      ref$adj.r.squared,
      tst$adj.r.squared
    )
  }
)

test_that(
  "Check presence of bug: Results from estimation with NAs imputed",
  {
    # Create data that can be used for estimation
    set.seed(1)
    data <- irf_generator(
      intercept = 0, 
      ar_params = c(0.75, 0.25, -0.1),
      x_params = c(2, 1, 0.5),
      x = rnorm(50),
      innovations = rnorm(50)
    )$irf |>
      dplyr::select(irf, x) |>
      dplyr::rename(y = irf)

    # Impute some NAs
    data[seq(0, 50, 10), ] <- NA

    # Perform estimation using the lag-function and estimate the parameters of 
    # this model outside of the irf_empirical function
    lag <- dplyr::lag
    ref <- lm(
      data = data,
      y ~ lag(y, 1) + lag(y, 2) + x + lag(x, 1) + lag(x, 2)
    )

    # Perform estimation through the irf_empirical function and extract the 
    # results of the estimation procedure.
    #
    # Importantly, NAs are removed in a casewise/listwise fashion, in which case 
    # we do not expect any differences between the `lm` outside and inside of 
    # `irf_empirical` 
    tst <- irf_empirical(
      data = data, 
      cols = c("y", "x"),
      y_lags = 2,
      x_lags = 2,
      burnin = FALSE,
      na_action = "listwise"
    )$fit |>
      suppressWarnings()

    # Test how well each aspect of the results corresponds between the two 
    # methods
    expect_equal(
      as.matrix(ref$coefficients) |>
        `dimnames<-`(NULL), 
      as.matrix(tst$coefficients) |>
        `dimnames<-`(NULL)
    )

    expect_equal(
      as.numeric(ref$residuals), 
      as.numeric(tst$residuals)
    )

    # Test how well each aspect of the summary holds up between the two methods
    ref_summ <- summary(ref)
    tst_summ <- summary(tst)

    expect_equal(
      as.matrix(ref_summ$coefficients) |>
        `dimnames<-`(NULL),
      as.matrix(tst_summ$coefficients) |>
        `dimnames<-`(NULL)
    )

    expect_equal(
      as.matrix(ref_summ$cov.unscaled) |>
        `dimnames<-`(NULL),
      as.matrix(tst_summ$cov.unscaled) |>
        `dimnames<-`(NULL)
    )

    expect_equal(
      ref_summ$r.squared,
      tst_summ$r.squared
    )

    expect_equal(
      ref_summ$adj.r.squared,
      tst_summ$adj.r.squared
    )

    # Perform estimation through the irf_empirical function and extract the 
    # results of the estimation procedure.
    #
    # Importantly, NAs are removed in a partial fashion, in which case we do 
    # expect differences between the `lm` outside and inside of `irf_empirical` 
    tst <- irf_empirical(
      data = data, 
      cols = c("y", "x"),
      y_lags = 2,
      x_lags = 2,
      burnin = FALSE,
      na_action = "partial"
    )$fit |>
      suppressWarnings()

    # Test how well each aspect of the results corresponds between the two 
    # methods
    expect_failure(
      expect_equal(
        as.matrix(ref$coefficients) |>
          `dimnames<-`(NULL), 
        as.matrix(tst$coefficients) |>
          `dimnames<-`(NULL)
      )
    )

    expect_failure(
      expect_equal(
        as.numeric(ref$residuals), 
        as.numeric(tst$residuals)
      )
    )

    # Test how well each aspect of the summary holds up between the two methods
    ref_summ <- summary(ref)
    tst_summ <- summary(tst)

    expect_failure(
      expect_equal(
        as.matrix(ref_summ$coefficients) |>
          `dimnames<-`(NULL),
        as.matrix(tst_summ$coefficients) |>
          `dimnames<-`(NULL)
      )
    )

    expect_failure(
      expect_equal(
        as.matrix(ref_summ$cov.unscaled) |>
          `dimnames<-`(NULL),
        as.matrix(tst_summ$cov.unscaled) |>
          `dimnames<-`(NULL)
      )
    )

    expect_failure(
      expect_equal(
        ref_summ$r.squared,
        tst_summ$r.squared
      )
    )

    expect_failure(
      expect_equal(
        ref_summ$adj.r.squared,
        tst_summ$adj.r.squared
      )
    )
  }
)

# Currently not supported, but might be useful to put it back in at some point
# test_that(
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
#       y <- irf(
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
#         results <- irf(
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
#     expect_true(all(tst_y))
#     expect_true(all(tst_x))
#   }
# )
