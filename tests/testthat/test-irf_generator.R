# Test expected error
testthat::test_that(
  "Testing expected error: No provided x and innovations",
  {
    testthat::expect_error(impulseR::irf_generator(0, 0.5, 1))

    testthat::expect_no_error(impulseR::irf_generator(0, 0.5, 1, x = rep(1, 10)))
    testthat::expect_no_error(impulseR::irf_generator(0, 0.5, 1, innovations = rep(1, 10)))
  }
)

# Test expected warnings 1
testthat::test_that(
  "Testing expected warning: Deviations in length of x and innovations",
  {
    # Create a function that only takes in x and innovations Makes the test 
    # easier to read
    fx <- function(x, innovations) {
      return(
        impulseR::irf_generator(
          intercept = parameters[[50]]$intercept,
          ar_params = parameters[[50]]$eps,
          x_params = parameters[[50]]$x,
          x = x,
          innovations = innovations,
          burnin = TRUE
        )$irf
      )
    }

    # When x has too few values
    vals_x <- rep(1, 5)
    vals_e <- rep(1, 10)

    testthat::expect_warning(fx(vals_x, vals_e))

    tst <- suppressWarnings(fx(vals_x, vals_e))
    testthat::expect_equal(tst$x, rep(c(1, 0), each = 5))
    testthat::expect_equal(tst$innovations, rep(1, each = 10))

    # When epsilon has too few values
    vals_x <- rep(1, 10)
    vals_e <- rep(1, 5)

    testthat::expect_warning(fx(vals_x, vals_e))

    tst <- suppressWarnings(fx(vals_x, vals_e))
    testthat::expect_equal(tst$x, rep(1, each = 10))
    testthat::expect_equal(tst$innovations, rep(c(1, 0), each = 5))
  }
)

# Test expected warnings 2
testthat::test_that(
  "Testing expected warning: NAs in x and/or innovations",
  {
    # Create x and innovations to be used in the tests
    x <- rnorm(100)
    innovations <- rnorm(100)

    x[1:3] <- NA 
    innovations[4:6] <- NA 

    # Create a temporary function that already has the parameters filled out
    fx <- function(x = NULL, innovations = NULL) {
      return(
        impulseR::irf_generator(
          intercept = 1, 
          ar_params = c(0.7, 0.2),
          x_params = c(2, 2),
          x = x, 
          innovations = innovations
        )$irf
      )
    }

    # Check the warning
    testthat::expect_warning(fx(x = x))
    testthat::expect_warning(fx(innovations = innovations))
    testthat::expect_warning(fx(x = x, innovations = innovations))

    # Check the output
    tst <- suppressWarnings(fx(x = x))
    testthat::expect_equal(tst$x, x[-c(1:3)])
    testthat::expect_equal(tst$innovations, numeric(97))

    tst <- suppressWarnings(fx(innovations = innovations))
    testthat::expect_equal(tst$x, numeric(97))
    testthat::expect_equal(tst$innovations, innovations[-c(4:6)])

    tst <- suppressWarnings(fx(x = x, innovations = innovations))
    testthat::expect_equal(tst$x, x[-c(1:6)])
    testthat::expect_equal(tst$innovations, innovations[-c(1:6)])
  }
)

# Test expected warnings 3
testthat::test_that(
  "Testing expected warning: More than one intercept",
  {
    testthat::expect_warning(
      impulseR::irf_generator(
        intercept = c(1, 2),
        ar_params = 0.75,
        x_params = c(2, 1),
        x = impulseR::impulse(10)
      )
    )

    tst <- suppressWarnings(
      impulseR::irf_generator(
        intercept = c(-10, 10),
        ar_params = 0.75,
        x_params = c(2, 1),
        x = impulseR::impulse(10)
      )$irf
    )
    testthat::expect_true(all(tst$irf_intercept < 0))

    tst <- suppressWarnings(
      impulseR::irf_generator(
        intercept = c(10, -10),
        ar_params = 0.75,
        x_params = c(2, 1),
        x = impulseR::impulse(10)
      )$irf
    )
    testthat::expect_true(all(tst$irf_intercept > 0))
  }
)

# Test expected warnings 4
testthat::test_that(
  "Testing expected warning: Nonstationarity",
  {
    testthat::expect_warning(
        impulseR::irf_generator(
            intercept = 0, 
            ar_params = c(0.8, 0.2),
            x_params = 1, 
            x = impulseR::impulse(10)
        )
    )

    testthat::expect_warning(
        impulseR::irf_generator(
            intercept = 0, 
            ar_params = c(0.8, 0.3),
            x_params = 1, 
            x = impulseR::impulse(10)
        )
    )

    testthat::expect_no_warning(
        impulseR::irf_generator(
            intercept = 0, 
            ar_params = c(0.8, 0.1),
            x_params = 1, 
            x = impulseR::impulse(10)
        )
    )
  }
)

# Test properties of the output.
testthat::test_that(
  "Testing properties of output",
  {
    # Based on x
    vals <- c(1, rep(0, 9))
    tst <- impulseR::irf_generator(
      intercept = parameters[[50]]$intercept,
      ar_params = parameters[[50]]$eps,
      x_params = parameters[[50]]$x, 
      x = vals, 
      innovations = rep(0, 10),
      burnin = TRUE
    )$irf

    testthat::expect_true(is.data.frame(tst))
    testthat::expect_equal(
      colnames(tst),
      c("time", "irf", "irf_intercept", "irf_x", "irf_v", "x", "innovations")
    )
    testthat::expect_equal(nrow(tst), 10)
    testthat::expect_equal(tst$x, vals)
    testthat::expect_equal(tst$innovations, rep(0, 10))

    # Based on eps
    vals <- rep(c(1, -1), times = 5)
    tst <- impulseR::irf_generator(
      intercept = parameters[[50]]$intercept,
      ar_params = parameters[[50]]$eps,
      x_params = parameters[[50]]$x, 
      x = rep(0, 10), 
      innovations = vals,
      burnin = TRUE
    )$irf

    testthat::expect_equal(tst$x, rep(0, 10))
    testthat::expect_equal(tst$innovations, vals)

    # Based on both
    vals_x <- c(1, rep(0, 9))
    vals_eps <- rep(c(1, -1), times = 5)
    tst <- impulseR::irf_generator(
      intercept = parameters[[50]]$intercept,
      ar_params = parameters[[50]]$eps,
      x_params = parameters[[50]]$x, 
      x = vals_x, 
      innovations = vals_eps,
      burnin = TRUE
    )$irf

    testthat::expect_equal(tst$x, vals_x)
    testthat::expect_equal(tst$innovations, vals_eps)
  }
)

# Test for some implicit handling of variables, based on the defaults of the 
# code
testthat::test_that(
  "Testing defaults of x and innovations",
  {
    # Defaults for innovations
    tst <- impulseR::irf_generator(
      intercept = parameters[[50]]$intercept,
      ar_params = parameters[[50]]$eps,
      x_params = parameters[[50]]$x,
      x = rep(1, 10)
    )$irf

    testthat::expect_equal(tst$innovations, rep(0, 10))

    # Defaults for x
    tst <- impulseR::irf_generator(
      intercept = parameters[[50]]$intercept,
      ar_params = parameters[[50]]$eps,
      x_params = parameters[[50]]$x,
      innovations = rep(1, 10)
    )$irf

    testthat::expect_equal(tst$x, rep(0, 10))
  }
)

testthat::test_that(
  "Testing effect of burnin",
  {
    # Create burned in and none-burned in data.
    burned <- impulseR::irf_generator(
      intercept = 1, 
      ar_params = c(0.5, 0.25), 
      x_params = 1,
      x = rep(1, 10),
      burnin = TRUE
    )$irf$irf_intercept

    not_burned <- impulseR::irf_generator(
      intercept = 1, 
      ar_params = c(0.5, 0.25), 
      x_params = 1,
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
      \(x) impulseR::irf_generator(
        intercept = x$intercept, 
        ar_params = x$eps,
        x_params = x$x, 
        x = vals, 
        innovations = rep(0, 10),
        burnin = TRUE
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
      \(x) impulseR::irf_generator(
        intercept = x$intercept, 
        ar_params = x$eps,
        x_params = x$x, 
        x = vals, 
        innovations = rep(0, 10),
        burnin = TRUE
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
      \(x) impulseR::irf_generator(
        intercept = x$intercept, 
        ar_params = x$eps,
        x_params = x$x, 
        x = vals, 
        innovations = rep(0, 10),
        burnin = TRUE
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
      \(x) impulseR::irf_generator(
        intercept = x$intercept, 
        ar_params = x$eps,
        x_params = x$x, 
        x = rep(0, 10), 
        innovations = vals,
        burnin = TRUE
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
      \(x) impulseR::irf_generator(
        intercept = x$intercept, 
        ar_params = x$eps,
        x_params = x$x, 
        x = rep(0, 10), 
        innovations = vals,
        burnin = TRUE
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
  "Testing cumulative responses through data",
  {
    tst <- sapply(
      parameters,
      function(x) {
        # Generate responses
        data <- impulseR::irf_generator(
          intercept = x$intercept,
          ar_params = x$eps,
          x_params = x$x,
          x = rnorm(100),
          innovations = rnorm(100)
        )$irf

        # Use these data with the same set of parameters
        output <- impulseR::irf_generator(
          intercept = x$intercept,
          ar_params = x$eps,
          x_params = x$x,
          data = data,
          cols = c("irf", "x"),
          burnin = FALSE
        )$irf |>
          suppressWarnings()

        # Total response, innovations, and x should be the same. Some precision 
        # errors in the creation of the residuals, but these are minimal
        return(
          all(abs(output$irf - data$irf) <= 10^(-4)) &
          all(output$x == data$x) &
          all(abs(output$innovations - data$innovations) <= 10^(-4))
        )
      }
    )

    # Do the actual check
    testthat::expect_true(all(tst))
  }
)
