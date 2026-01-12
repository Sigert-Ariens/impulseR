# Test expected errors
test_that(
  "Testing expected error: No provided x and innovations",
  {
    expect_error(irf_generator(0, 0.5, 1))

    expect_no_error(irf_generator(0, 0.5, 1, x = rep(1, 10)))
    expect_no_error(irf_generator(0, 0.5, 1, innovations = rep(1, 10)))
  }
)

test_that(
  "Testing expected error: Column names in data cannot be found",
  {
    expect_error(
      irf_generator(
        0, 
        0.5, 
        1, 
        data = data.frame(
          x = rnorm(100),
          y = rnorm(100)
        ),
        cols = c("dependent", "independent")
      )
    )
  }
)

# Test expected warnings 1
test_that(
  "Testing expected warning: Deviations in length of x and innovations",
  {
    # Create a function that only takes in x and innovations Makes the test
    # easier to read
    fx <- function(x, innovations) {
      return(
        irf_generator(
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

    expect_warning(fx(vals_x, vals_e))

    tst <- suppressWarnings(fx(vals_x, vals_e))
    expect_equal(tst$x, rep(c(1, 0), each = 5))
    expect_equal(tst$innovations, rep(1, each = 10))

    # When epsilon has too few values
    vals_x <- rep(1, 10)
    vals_e <- rep(1, 5)

    expect_warning(fx(vals_x, vals_e))

    tst <- suppressWarnings(fx(vals_x, vals_e))
    expect_equal(tst$x, rep(1, each = 10))
    expect_equal(tst$innovations, rep(c(1, 0), each = 5))
  }
)

# Test expected warnings 2
test_that(
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
        irf_generator(
          intercept = 1,
          ar_params = c(0.7, 0.2),
          x_params = c(2, 2),
          x = x,
          innovations = innovations
        )$irf
      )
    }

    # Check the warning
    expect_warning(fx(x = x))
    expect_warning(fx(innovations = innovations))
    expect_warning(fx(x = x, innovations = innovations))

    # Check the output
    tst <- suppressWarnings(fx(x = x))
    expect_equal(tst$x, x[-c(1:3)])
    expect_equal(tst$innovations, numeric(97))

    tst <- suppressWarnings(fx(innovations = innovations))
    expect_equal(tst$x, numeric(97))
    expect_equal(tst$innovations, innovations[-c(4:6)])

    tst <- suppressWarnings(fx(x = x, innovations = innovations))
    expect_equal(tst$x, x[-c(1:6)])
    expect_equal(tst$innovations, innovations[-c(1:6)])
  }
)

# Test expected warnings 3
test_that(
  "Testing expected warning: More than one intercept",
  {
    expect_warning(
      irf_generator(
        intercept = c(1, 2),
        ar_params = 0.75,
        x_params = c(2, 1),
        x = impulse(10)
      )
    )

    tst <- suppressWarnings(
      irf_generator(
        intercept = c(-10, 10),
        ar_params = 0.75,
        x_params = c(2, 1),
        x = impulse(10)
      )$irf
    )
    expect_true(all(tst$irf_intercept < 0))

    tst <- suppressWarnings(
      irf_generator(
        intercept = c(10, -10),
        ar_params = 0.75,
        x_params = c(2, 1),
        x = impulse(10)
      )$irf
    )
    expect_true(all(tst$irf_intercept > 0))
  }
)

# Test expected warnings 4
test_that(
  "Testing expected warning: Nonstationarity",
  {
    expect_warning(
      irf_generator(
        intercept = 0,
        ar_params = c(0.8, 0.2),
        x_params = 1,
        x = impulse(10)
      )
    )

    expect_warning(
      irf_generator(
        intercept = 0,
        ar_params = c(0.8, 0.3),
        x_params = 1,
        x = impulse(10)
      )
    )

    expect_no_warning(
      irf_generator(
        intercept = 0,
        ar_params = c(0.8, 0.1),
        x_params = 1,
        x = impulse(10)
      )
    )
  }
)

# Test properties of the output.
test_that(
  "Testing properties of output",
  {
    # Based on x
    vals <- c(1, rep(0, 9))
    tst <- irf_generator(
      intercept = parameters[[50]]$intercept,
      ar_params = parameters[[50]]$eps,
      x_params = parameters[[50]]$x,
      x = vals,
      innovations = rep(0, 10),
      burnin = TRUE
    )$irf

    expect_true(is.data.frame(tst))
    expect_equal(
      colnames(tst),
      c("time", "irf", "irf_intercept", "irf_x", "irf_v", "x", "innovations")
    )
    expect_equal(nrow(tst), 10)
    expect_equal(tst$x, vals)
    expect_equal(tst$innovations, rep(0, 10))

    # Based on eps
    vals <- rep(c(1, -1), times = 5)
    tst <- irf_generator(
      intercept = parameters[[50]]$intercept,
      ar_params = parameters[[50]]$eps,
      x_params = parameters[[50]]$x,
      x = rep(0, 10),
      innovations = vals,
      burnin = TRUE
    )$irf

    expect_equal(tst$x, rep(0, 10))
    expect_equal(tst$innovations, vals)

    # Based on both
    vals_x <- c(1, rep(0, 9))
    vals_eps <- rep(c(1, -1), times = 5)
    tst <- irf_generator(
      intercept = parameters[[50]]$intercept,
      ar_params = parameters[[50]]$eps,
      x_params = parameters[[50]]$x,
      x = vals_x,
      innovations = vals_eps,
      burnin = TRUE
    )$irf

    expect_equal(tst$x, vals_x)
    expect_equal(tst$innovations, vals_eps)
  }
)

# Test for some implicit handling of variables, based on the defaults of the
# code
test_that(
  "Testing defaults of x and innovations",
  {
    # Defaults for innovations
    tst <- irf_generator(
      intercept = parameters[[50]]$intercept,
      ar_params = parameters[[50]]$eps,
      x_params = parameters[[50]]$x,
      x = rep(1, 10)
    )$irf

    expect_equal(tst$innovations, rep(0, 10))

    # Defaults for x
    tst <- irf_generator(
      intercept = parameters[[50]]$intercept,
      ar_params = parameters[[50]]$eps,
      x_params = parameters[[50]]$x,
      innovations = rep(1, 10)
    )$irf

    expect_equal(tst$x, rep(0, 10))
  }
)

test_that(
  "Testing effect of burnin",
  {
    # Create burned in and none-burned in data.
    burned <- irf_generator(
      intercept = 1,
      ar_params = c(0.5, 0.25),
      x_params = 1,
      x = rep(1, 10),
      burnin = TRUE
    )$irf$irf_intercept

    not_burned <- irf_generator(
      intercept = 1,
      ar_params = c(0.5, 0.25),
      x_params = 1,
      x = rep(1, 10),
      burnin = FALSE
    )$irf$irf_intercept

    # Compare the values of `irf_intercept` based on analytic values of the
    # non-zero intercept
    expect_equal(burned, rep(1 / 0.25, 10))
    expect_equal(
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
test_that(
  "Testing variations in x",
  {
    #############################
    # Single impulse in beginning
    vals <- c(1, rep(0, 9))
    tst <- lapply(
      parameters,
      \(x) irf_generator(
        intercept = x$intercept,
        ar_params = x$eps,
        x_params = x$x,
        x = vals,
        innovations = rep(0, 10),
        burnin = TRUE
      )$irf[, c("time", "irf")] |>
        as.matrix() |>
        `rownames<-`(NULL) |>
        `colnames<-`(NULL)
    )

    ref <- readRDS(
      test_path("ref", "ref_x_single.Rds")
    )

    expect_equal(tst, ref)



    #############################
    # Two impulses in beginning
    vals <- c(1, -1, rep(0, 8))
    tst <- lapply(
      parameters,
      \(x) irf_generator(
        intercept = x$intercept,
        ar_params = x$eps,
        x_params = x$x,
        x = vals,
        innovations = rep(0, 10),
        burnin = TRUE
      )$irf[, c("time", "irf")] |>
        as.matrix() |>
        `rownames<-`(NULL) |>
        `colnames<-`(NULL)
    )

    ref <- readRDS(
      test_path("ref", "ref_x_double.Rds")
    )

    expect_equal(tst, ref)



    #############################
    # Multiple impulses
    vals <- rep(c(1, -1), times = 5)
    tst <- lapply(
      parameters,
      \(x) irf_generator(
        intercept = x$intercept,
        ar_params = x$eps,
        x_params = x$x,
        x = vals,
        innovations = rep(0, 10),
        burnin = TRUE
      )$irf[, c("time", "irf")] |>
        as.matrix() |>
        `rownames<-`(NULL) |>
        `colnames<-`(NULL)
    )

    ref <- readRDS(
      test_path("ref", "ref_x_multiple.Rds")
    )

    expect_equal(tst, ref)
  }
)

test_that(
  "Testing variations in epsilon",
  {
    #############################
    # Single impulse in beginning
    vals <- c(1, rep(0, 9))
    tst <- lapply(
      parameters,
      \(x) irf_generator(
        intercept = x$intercept,
        ar_params = x$eps,
        x_params = x$x,
        x = rep(0, 10),
        innovations = vals,
        burnin = TRUE
      )$irf[, c("time", "irf")] |>
        as.matrix() |>
        `rownames<-`(NULL) |>
        `colnames<-`(NULL)
    )

    ref <- readRDS(
      test_path("ref", "ref_eps_single.Rds")
    )

    expect_equal(tst, ref)


    #############################
    # Multiple impulses
    vals <- rep(c(1, -1), times = 5)
    tst <- lapply(
      parameters,
      \(x) irf_generator(
        intercept = x$intercept,
        ar_params = x$eps,
        x_params = x$x,
        x = rep(0, 10),
        innovations = vals,
        burnin = TRUE
      )$irf[, c("time", "irf")] |>
        as.matrix() |>
        `rownames<-`(NULL) |>
        `colnames<-`(NULL)
    )

    ref <- readRDS(
      test_path("ref", "ref_eps_multiple.Rds")
    )

    expect_equal(tst, ref)
  }
)

test_that(
  "Testing cumulative responses through data",
  {
    tst <- sapply(
      parameters,
      function(x) {
        # Generate responses
        data <- irf_generator(
          intercept = x$intercept,
          ar_params = x$eps,
          x_params = x$x,
          x = rnorm(100),
          innovations = rnorm(100)
        )$irf

        # Use these data with the same set of parameters
        output <- irf_generator(
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
    expect_true(all(tst))
  }
)


# TEST MISSING VALUES: LISTWISE VS PARTIAL AND GETTING BACK THE VALUES NEEDED
test_that(
  "Computing system responses for data with NA works: Listwise deletion",
  {
    # Create a dataset for which to fix the inputs
    set.seed(1)
    innovations <- rnorm(10)

    data <- irf_generator(
      intercept = 0, 
      ar_params = c(0.25, 0.1),
      x_params = c(2, 1, 0.5),
      innovations = innovations, 
      x = rnorm(10)
    )
    data <- data$irf[, c("irf", "x")] |>
      `colnames<-` (c("y", "x"))

    # Impose an NA in the middle of the dataset
    data[6, ] <- NA

    # Generate an irf for these data using the innovations and x that are 
    # computed here. If performed well, then the irf should be exactly equal
    # to the observed data.
    #
    # Note that checking the computed innovations directly against the simulated 
    # innovations (like in the previous test) wouldn't work, as the model will
    # compensate the loss of information.
    tst <- irf_generator(
      intercept = 0, 
      ar_params = c(0.25, 0.1),
      x_params = c(2, 1, 0.5),
      data = data,
      na_action = "listwise"
    ) |>
      suppressWarnings()

    # Check the output
    expect_equal(
      tst$irf$irf, 
      data$y[!is.na(data$y)]
    )

    # Impose an additional NA value right next to the previous one and one a bit
    # before
    data[c(4, 7), ] <- NA

    # Generate an irf for these data using the innovations and x that are 
    # computed here. If performed well, then the irf should be exactly equal
    # to the observed data.
    #
    # Note that checking the computed innovations directly against the simulated 
    # innovations (like in the previous test) wouldn't work, as the model will
    # compensate the loss of information.
    tst <- irf_generator(
      intercept = 0, 
      ar_params = c(0.25, 0.1),
      x_params = c(2, 1, 0.5),
      data = data,
      na_action = "listwise"
    ) |>
      suppressWarnings()

    # Check the output
    expect_equal(
      tst$irf$irf, 
      data$y[!is.na(data$y)]
    )
  }
)

test_that(
  "Computing system responses for data with NA works: Partial deletion",
  {
    # Create a dataset for which to fix the inputs
    set.seed(1)
    innovations <- rnorm(10)

    data <- irf_generator(
      intercept = 0, 
      ar_params = c(0.25, 0.1),
      x_params = c(2, 1, 0.5),
      innovations = innovations, 
      x = rnorm(10)
    )
    data <- data$irf[, c("irf", "x")] |>
      `colnames<-` (c("y", "x"))

    # Impose an NA in the middle of the dataset
    data[6, ] <- NA

    # Generate an irf for these data using the innovations and x that are 
    # computed here. If performed well, then the irf should be exactly equal
    # to the observed data.
    #
    # Note that checking the computed innovations directly against the simulated 
    # innovations (like in the previous test) wouldn't work, as the model will
    # compensate the loss of information.
    tst <- irf_generator(
      intercept = 0, 
      ar_params = c(0.25, 0.1),
      x_params = c(2, 1, 0.5),
      data = data,
      na_action = "partial"
    )

    # Check the output
    expect_equal(
      tst$irf$irf, 
      data$y[!is.na(data$y)]
    )

    # Impose an additional NA value right next to the previous one and one a bit
    # before
    data[c(4, 7), ] <- NA

    # Generate an irf for these data using the innovations and x that are 
    # computed here. If performed well, then the irf should be exactly equal
    # to the observed data.
    #
    # Note that checking the computed innovations directly against the simulated 
    # innovations (like in the previous test) wouldn't work, as the model will
    # compensate the loss of information.
    tst <- irf_generator(
      intercept = 0, 
      ar_params = c(0.25, 0.1),
      x_params = c(2, 1, 0.5),
      data = data,
      na_action = "partial"
    )

    # Check the output
    expect_equal(
      tst$irf$irf, 
      data$y[!is.na(data$y)]
    )
  }
)
