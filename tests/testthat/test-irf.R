# Create some parameter sets to be used in the test. Contains no lag, single
# lag, two lags for epsilon and/or x. Values for these lags can also be negative.
# Slopes for x can be positive, negative, or no effect. 
eps_params <- list(
  0, 
  0.5, 
  c(0.5, 0.25),
  -0.5, 
  c(-0.5, 0.25)
)

x_params <- append(
  lapply(eps_params, \(x) 2 * x), 
  append(
    lapply(eps_params, \(x) 0 * x), 
    lapply(eps_params, \(x) -2 * x)
  )
)

parameters <- list()
for(i in eps_params) {
  for(j in x_params) {
    parameters <- append(
      parameters, 
      list(list("intercept" = 0, "eps" = i, "x" = j))
    )
  }
}

# Test expected error
testthat::test_that(
  "Testing expected error: No provided x and residuals",
  {
    testthat::expect_error(impulseR::irf(0, 0.5, 1))

    testthat::expect_no_error(impulseR::irf(0, 0.5, 1, x = rep(1, 10)))
    testthat::expect_no_error(impulseR::irf(0, 0.5, 1, residuals = rep(1, 10)))
  }
)

# Test expected warnings
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
        )
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

# Test properties of the output.
testthat::test_that(
  "Testing properties of output",
  {
    # Based on x
    vals <- c(1, rep(0, 9))
    tst <- impulseR::irf(
      parameters[[50]]$intercept,
      parameters[[50]]$eps,
      parameters[[50]]$x, 
      vals, 
      rep(0, 10),
      TRUE
    )

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
      parameters[[50]]$intercept,
      parameters[[50]]$eps,
      parameters[[50]]$x, 
      rep(0, 10),
      vals,
      TRUE
    )

    testthat::expect_equal(tst$x, rep(0, 10))
    testthat::expect_equal(tst$residuals, vals)

    # Based on both
    vals_x <- c(1, rep(0, 9))
    vals_eps <- rep(c(1, -1), times = 5)
    tst <- impulseR::irf(
      parameters[[50]]$intercept,
      parameters[[50]]$eps,
      parameters[[50]]$x, 
      vals_x,
      vals_eps,
      TRUE
    )

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
    )

    testthat::expect_equal(tst$residuals, rep(0, 10))

    # Defaults for x
    tst <- impulseR::irf(
      parameters[[50]]$intercept,
      parameters[[50]]$eps,
      parameters[[50]]$x,
      residuals = rep(1, 10)
    )

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
    )$irf_intercept

    not_burned <- impulseR::irf(
      1, 
      c(0.5, 0.25), 
      1,
      x = rep(1, 10),
      burnin = FALSE
    )$irf_intercept

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
      ) |> 
        dplyr::select(time, irf) |> 
        as.matrix() |> 
        `rownames<-` (NULL) |> 
        `colnames<-` (NULL)
    )

    ref <- readRDS(
      file.path("ref", "ref_x_single.Rds")
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
      ) |> 
        dplyr::select(time, irf) |> 
        as.matrix() |> 
        `rownames<-` (NULL) |> 
        `colnames<-` (NULL)
    )

    ref <- readRDS(
      file.path("ref", "ref_x_double.Rds")
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
      ) |> 
        dplyr::select(time, irf) |> 
        as.matrix() |> 
        `rownames<-` (NULL) |> 
        `colnames<-` (NULL)
    )

    ref <- readRDS(
      file.path("ref", "ref_x_multiple.Rds")
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
      ) |> 
        dplyr::select(time, irf) |> 
        as.matrix() |> 
        `rownames<-` (NULL) |> 
        `colnames<-` (NULL)
    )

    ref <- readRDS(
      file.path("ref", "ref_eps_single.Rds")
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
      ) |> 
        dplyr::select(time, irf) |> 
        as.matrix() |> 
        `rownames<-` (NULL) |> 
        `colnames<-` (NULL)
    )

    ref <- readRDS(
      file.path("ref", "ref_eps_multiple.Rds")
    )

    testthat::expect_equal(tst, ref)
  }
)
