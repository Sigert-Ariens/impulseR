# Test expected error
testthat::test_that(
  "Testing expected error: No provided x and residuals",
  {
    testthat::expect_error(impulseR::irf(0, 0.5, 1))

    testthat::expect_no_error(impulseR::irf(0, 0.5, 1, x = rep(1, 10)))
    testthat::expect_no_error(impulseR::irf(0, 0.5, 1, residuals = rep(1, 10)))
  }
)

# Test expected warnings 1
testthat::test_that(
  "Testing expected warning: Deviations in length of x and residuals",
  {
    # Create a function that only takes in x and residuals Makes the test 
    # easier to read
    fx <- function(x, residuals) {
      return(
        impulseR::irf(
          parameters[[50]]$intercept,
          parameters[[50]]$eps,
          parameters[[50]]$x,
          x,
          residuals,
          TRUE
        )$irf
      )
    }

    # When x has too few values
    vals_x <- rep(1, 5)
    vals_e <- rep(1, 10)

    testthat::expect_warning(fx(vals_x, vals_e))

    tst <- suppressWarnings(fx(vals_x, vals_e))
    testthat::expect_equal(tst$x, rep(c(1, 0), each = 5))
    testthat::expect_equal(tst$residuals, rep(1, each = 10))

    # When epsilon has too few values
    vals_x <- rep(1, 10)
    vals_e <- rep(1, 5)

    testthat::expect_warning(fx(vals_x, vals_e))

    tst <- suppressWarnings(fx(vals_x, vals_e))
    testthat::expect_equal(tst$x, rep(1, each = 10))
    testthat::expect_equal(tst$residuals, rep(c(1, 0), each = 5))
  }
)

# Test expected warnings 2
testthat::test_that(
  "Testing expected warning: NAs in x and/or residuals",
  {
    # Create x and residuals to be used in the tests
    x <- rnorm(100)
    residuals <- rnorm(100)

    x[1:3] <- NA 
    residuals[4:6] <- NA 

    # Create a temporary function that already has the parameters filled out
    fx <- function(x = NULL, residuals = NULL) {
      return(
        impulseR::irf(
          intercept = 1, 
          ar_params = c(0.75, 0.75),
          x_params = c(2, 2),
          x = x, 
          residuals = residuals
        )$irf
      )
    }

    # Check the warning
    testthat::expect_warning(fx(x = x))
    testthat::expect_warning(fx(residuals = residuals))
    testthat::expect_warning(fx(x = x, residuals = residuals))

    # Check the output
    tst <- suppressWarnings(fx(x = x))
    testthat::expect_equal(tst$x, x[-c(1:3)])
    testthat::expect_equal(tst$residuals, numeric(97))

    tst <- suppressWarnings(fx(residuals = residuals))
    testthat::expect_equal(tst$x, numeric(97))
    testthat::expect_equal(tst$residuals, residuals[-c(4:6)])

    tst <- suppressWarnings(fx(x = x, residuals = residuals))
    testthat::expect_equal(tst$x, x[-c(1:6)])
    testthat::expect_equal(tst$residuals, residuals[-c(1:6)])
  }
)

# Test expected warnings 3
testthat::test_that(
  "Testing expected warning: More than one intercept",
  {
    testthat::expect_warning(
      impulseR::irf(
        intercept = c(1, 2),
        ar_params = c(0.75, 0.5),
        x_params = c(2, 1),
        x = impulseR::impulse(10)
      )
    )

    tst <- suppressWarnings(
      impulseR::irf(
        intercept = c(-10, 10),
        ar_params = 0.75,
        x_params = c(2, 1),
        x = impulseR::impulse(10)
      )$irf
    )
    testthat::expect_true(all(tst$irf_intercept < 0))

    tst <- suppressWarnings(
      impulseR::irf(
        intercept = c(10, -10),
        ar_params = 0.75,
        x_params = c(2, 1),
        x = impulseR::impulse(10)
      )$irf
    )
    testthat::expect_true(all(tst$irf_intercept > 0))
  }
)

# Test properties of the output.
testthat::test_that(
  "Testing properties of output",
  {
    # Based on x
    vals <- c(1, rep(0, 9))
    tst <- impulseR::irf(
      intercept = parameters[[50]]$intercept,
      ar_params = parameters[[50]]$eps,
      x_params = parameters[[50]]$x, 
      x = vals, 
      residuals = rep(0, 10),
      burnin = TRUE
    )$irf

    testthat::expect_true(is.data.frame(tst))
    testthat::expect_equal(
      colnames(tst),
      c("time", "irf", "irf_intercept", "irf_x", "irf_eps", "x", "residuals")
    )
    testthat::expect_equal(nrow(tst), 10)
    testthat::expect_equal(tst$x, vals)
    testthat::expect_equal(tst$residuals, rep(0, 10))

    # Based on eps
    vals <- rep(c(1, -1), times = 5)
    tst <- impulseR::irf(
      intercept = parameters[[50]]$intercept,
      ar_params = parameters[[50]]$eps,
      x_params = parameters[[50]]$x, 
      x = rep(0, 10), 
      residuals = vals,
      burnin = TRUE
    )$irf

    testthat::expect_equal(tst$x, rep(0, 10))
    testthat::expect_equal(tst$residuals, vals)

    # Based on both
    vals_x <- c(1, rep(0, 9))
    vals_eps <- rep(c(1, -1), times = 5)
    tst <- impulseR::irf(
      intercept = parameters[[50]]$intercept,
      ar_params = parameters[[50]]$eps,
      x_params = parameters[[50]]$x, 
      x = vals_x, 
      residuals = vals_eps,
      burnin = TRUE
    )$irf

    testthat::expect_equal(tst$x, vals_x)
    testthat::expect_equal(tst$residuals, vals_eps)
  }
)

# Test for some implicit handling of variables, based on the defaults of the 
# code
testthat::test_that(
  "Testing defaults of x and residuals",
  {
    # Defaults for residuals
    tst <- impulseR::irf(
      parameters[[50]]$intercept,
      parameters[[50]]$eps,
      parameters[[50]]$x,
      x = rep(1, 10)
    )$irf

    testthat::expect_equal(tst$residuals, rep(0, 10))

    # Defaults for x
    tst <- impulseR::irf(
      parameters[[50]]$intercept,
      parameters[[50]]$eps,
      parameters[[50]]$x,
      residuals = rep(1, 10)
    )$irf

    testthat::expect_equal(tst$x, rep(0, 10))
  }
)

testthat::test_that(
  "Testing effect of burnin",
  {
    # Create burned in and none-burned in data.
    burned <- impulseR::irf(
      1, 
      c(0.5, 0.25), 
      1,
      x = rep(1, 10),
      burnin = TRUE
    )$irf$irf_intercept

    not_burned <- impulseR::irf(
      1, 
      c(0.5, 0.25), 
      1,
      x = rep(1, 10),
      burnin = FALSE
    )$irf$irf_intercept

    # Compare the values of `irf_intercept` based on analytic values of the 
    # non-zero intercept
    testthat::expect_equal(burned, rep(1/0.25, 10))
    testthat::expect_equal(
      not_burned,
      c(1, 1.5, 2, 2.375, 2.688, 2.938, 3.141, 3.305, 3.438, 3.545),
      tolerance = 10^(-2)
    )
  }
)

# Test for variations in x. Changing the values of x over time and the impulse-
# response in response to these variations.
#
# For references, strip rownames, column names, etc. Allows us to make changes 
# to these metadata without tests failing
testthat::test_that(
  "Testing variations in x",
  {
    #############################
    # Single impulse in beginning
    vals <- c(1, rep(0, 9))
    tst <- lapply(
      parameters, 
      \(x) impulseR::irf(
        x$intercept, 
        x$eps,
        x$x, 
        vals, 
        rep(0, 10),
        TRUE
      )$irf[, c("time", "irf")] |> 
        as.matrix() |> 
        `rownames<-` (NULL) |> 
        `colnames<-` (NULL)
    )

    ref <- readRDS(
      testthat::test_path("ref", "ref_x_single.Rds")
    )

    testthat::expect_equal(tst, ref)



    #############################
    # Two impulses in beginning
    vals <- c(1, -1, rep(0, 8))
    tst <- lapply(
      parameters, 
      \(x) impulseR::irf(
        x$intercept, 
        x$eps,
        x$x, 
        vals, 
        rep(0, 10),
        TRUE
      )$irf[, c("time", "irf")] |> 
        as.matrix() |> 
        `rownames<-` (NULL) |> 
        `colnames<-` (NULL)
    )

    ref <- readRDS(
      testthat::test_path("ref", "ref_x_double.Rds")
    )

    testthat::expect_equal(tst, ref)



    #############################
    # Multiple impulses
    vals <- rep(c(1, -1), times = 5)
    tst <- lapply(
      parameters, 
      \(x) impulseR::irf(
        x$intercept, 
        x$eps,
        x$x, 
        vals, 
        rep(0, 10),
        TRUE
      )$irf[, c("time", "irf")] |> 
        as.matrix() |> 
        `rownames<-` (NULL) |> 
        `colnames<-` (NULL)
    )

    ref <- readRDS(
      testthat::test_path("ref", "ref_x_multiple.Rds")
    )

    testthat::expect_equal(tst, ref)
  }
)

testthat::test_that(
  "Testing variations in epsilon",
  {
    #############################
    # Single impulse in beginning
    vals <- c(1, rep(0, 9))
    tst <- lapply(
      parameters, 
      \(x) impulseR::irf(
        x$intercept, 
        x$eps,
        x$x, 
        rep(0, 10), 
        vals,
        TRUE
      )$irf[, c("time", "irf")] |> 
        as.matrix() |> 
        `rownames<-` (NULL) |> 
        `colnames<-` (NULL)
    )

    ref <- readRDS(
      testthat::test_path("ref", "ref_eps_single.Rds")
    )

    testthat::expect_equal(tst, ref)


    #############################
    # Multiple impulses
    vals <- rep(c(1, -1), times = 5)
    tst <- lapply(
      parameters, 
      \(x) impulseR::irf(
        x$intercept, 
        x$eps,
        x$x, 
        rep(0, 10), 
        vals,
        TRUE
      )$irf[, c("time", "irf")] |> 
        as.matrix() |> 
        `rownames<-` (NULL) |> 
        `colnames<-` (NULL)
    )

    ref <- readRDS(
      testthat::test_path("ref", "ref_eps_multiple.Rds")
    )

    testthat::expect_equal(tst, ref)
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
      )$irf

      # Loop over all possibilities of the lags
      for(j in seq_len(nrow(lags))) {
        # Estimate the parameters and retrieve the results
        results <- impulseR::irf(
          y, 
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

testthat::test_that(
  "Testing estimation of parameters and providing own impulses",
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
    set.seed(1)
    for(i in seq_along(parameters)) {
      # Generate data
      y <- impulseR::irf(
        intercept = parameters[[i]]$intercept,
        x_params = parameters[[i]]$x,
        ar_params = parameters[[i]]$eps,
        x = rnorm(100),
        residuals = rnorm(100)
      )$irf

      # Loop over all possibilities of the lags
      for(j in seq_len(nrow(lags))) {
        # Create current impulses from a normal distribution. Provides a stronger
        # check for whether the correct information is passed on to the 
        # correct functions. 
        impulses <- rnorm(100)

        # Estimate the parameters and retrieve the results
        results <- impulseR::irf(
          y, 
          cols = c("irf", "x"),
          y_lags = lags$y_lags[j],
          x_lags = ifelse(is.na(lags$x_lags[j]), NA, lags$x_lags[j] - 1),
          x = impulses,
          residuals = impulses
        )$irf

        # Check whether `x` and `residuals` are correctly passed on
        tst_y[i, j] <- all(results$x == impulses)
        tst_x[i, j] <- all(results$residuals == impulses)
      }
    }

    # Do the actual check
    testthat::expect_true(all(tst_y))
    testthat::expect_true(all(tst_x))
  }
)
