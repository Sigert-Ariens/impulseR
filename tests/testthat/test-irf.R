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

# Test expected warnings
testthat::test_that(
  "Testing expected warning: Deviations in length of Xvals and Epsvals",
  {
    # Create a function that only takes in Xvals and Epsvals. Makes the test 
    # easier to read
    fx <- function(x, residuals) {
      return(
        irf::irf(
          parameters[[1]]$intercept,
          parameters[[1]]$eps,
          parameters[[1]]$x,
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
    tst <- irf::irf(
      parameters[[1]]$intercept,
      parameters[[1]]$eps,
      parameters[[1]]$x, 
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
    tst <- irf::irf(
      parameters[[1]]$intercept,
      parameters[[1]]$eps,
      parameters[[1]]$x, 
      rep(0, 10),
      vals,
      TRUE
    )

    testthat::expect_equal(tst$x, rep(0, 10))
    testthat::expect_equal(tst$residuals, vals)

    # Based on both
    vals_x <- c(1, rep(0, 9))
    vals_eps <- rep(c(1, -1), times = 5)
    tst <- irf::irf(
      parameters[[1]]$intercept,
      parameters[[1]]$eps,
      parameters[[1]]$x, 
      vals_x,
      vals_eps,
      TRUE
    )

    testthat::expect_equal(tst$x, vals_x)
    testthat::expect_equal(tst$residuals, vals_eps)
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
      \(x) irf::irf(
        x$intercept, 
        x$eps,
        x$x, 
        vals, 
        rep(0, 10),
        TRUE
      ) %>% 
        dplyr::select(time, irf) %>% 
        as.matrix() %>% 
        `rownames<-` (NULL) %>% 
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
      \(x) irf::irf(
        x$intercept, 
        x$eps,
        x$x, 
        vals, 
        rep(0, 10),
        TRUE
      ) %>% 
        dplyr::select(time, irf) %>% 
        as.matrix() %>% 
        `rownames<-` (NULL) %>% 
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
      \(x) irf::irf(
        x$intercept, 
        x$eps,
        x$x, 
        vals, 
        rep(0, 10),
        TRUE
      ) %>% 
        dplyr::select(time, irf) %>% 
        as.matrix() %>% 
        `rownames<-` (NULL) %>% 
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
      \(x) irf::irf(
        x$intercept, 
        x$eps,
        x$x, 
        rep(0, 10), 
        vals,
        TRUE
      ) %>% 
        dplyr::select(time, irf) %>% 
        as.matrix() %>% 
        `rownames<-` (NULL) %>% 
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
      \(x) irf::irf(
        x$intercept, 
        x$eps,
        x$x, 
        rep(0, 10), 
        vals,
        TRUE
      ) %>% 
        dplyr::select(time, irf) %>% 
        as.matrix() %>% 
        `rownames<-` (NULL) %>% 
        `colnames<-` (NULL)
    )

    ref <- readRDS(
      file.path("ref", "ref_eps_multiple.Rds")
    )

    testthat::expect_equal(tst, ref)
  }
)
