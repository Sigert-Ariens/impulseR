test_that(
  "Testing known errors",
  {
    # Wrong columns in estimate
    expect_error(
      estimate(
        data.frame(Y = numeric(100), X = numeric(100)),
        cols = c("y", "x"),
        x_lags = 2,
        y_lags = 2
      )
    )

    # Wrong columns in compute_innovations
    expect_error(
      compute_innovations(
        data.frame(Y = numeric(100), X = numeric(100)),
        cols = c("y", "x")
      )
    )

    # Wrong columns in prepare_data
    expect_error(
      prepare_data(
        data.frame(Y = numeric(100), X = numeric(100)),
        cols = c("y", "x")
      )
    )

    # Single column specified, but only if x_lags specified in estimate
    expect_error(
      estimate(
        data.frame(Y = numeric(100)),
        cols = c("y", "x"),
        x_lags = 2,
        y_lags = 2
      )
    )

    expect_no_error(
      estimate(
        data.frame(Y = numeric(100)),
        cols = c("y", "x"),
        y_lags = 2
      ) |>
        suppressWarnings()
    )

    # Single column specified, but only if x_lags specified in estimate
    expect_error(
      compute_innovations(
        data.frame(Y = numeric(100)),
        cols = c("y", "x"),
        intercept = 2,
        ar_params = c(0.75, 0.5),
        x_params = c(2, 1)
      )
    )

    expect_no_error(
      compute_innovations(
        data.frame(Y = numeric(100)),
        cols = c("y", "x"),
        intercept = 2,
        ar_params = c(0.75, 0.5)
      )
    )

    # Single column specified, but only if x_lags in estimate
    expect_error(
      prepare_data(
        data.frame(Y = numeric(100)),
        cols = c("y", "x"),
        x_lags = 2,
        y_lags = 2
      )
    )

    expect_no_error(
      prepare_data(
        data.frame(Y = numeric(100)),
        cols = c("y", "x"),
        y_lags = 2
      )
    )
  }
)

test_that(
  "Testing known warnings: No lags specified",
  {
    data <- data.frame(y = rnorm(100), x = rnorm(100))

    # No lags specified. Also testing output
    expect_warning(prepare_data(data))

    tst <- suppressWarnings(prepare_data(data))
    expect_equal(tst, data)
  }
)

test_that(
  "Testing known warnings: Lags wrongly specified",
  {
    data <- data.frame(y = rnorm(100), x = rnorm(100))

    # Lags wrongly specified in `prepare_data`. Also testing output (but only
    # for single option)
    #
    # Multiple warnings are sometimes expect, which leads to the nested calls
    expect_warning(
      expect_warning(
        expect_warning(
          prepare_data(
            data,
            x_lags = -2,
            y_lags = -2
          )
        )
      )
    )

    expect_warning(
      prepare_data(
        data,
        x_lags = -2,
        y_lags = 1
      )
    )

    expect_warning(
      prepare_data(
        data,
        x_lags = 1,
        y_lags = -2
      )
    )

    tst <- suppressWarnings(
      prepare_data(
        data,
        x_lags = -2,
        y_lags = -2
      )
    )
    expect_equal(tst, data)

    # Lags wrongly specified in `estimate`. Also testing output
    #
    # Multiple warnings are sometimes expect, which leads to the nested calls
    expect_warning(
      expect_warning(
        estimate(
          data,
          x_lags = -2,
          y_lags = -2
        )
      )
    )

    expect_warning(
      estimate(
        data,
        x_lags = -2,
        y_lags = 1
      )
    )

    expect_warning(
      estimate(
        data,
        x_lags = 1,
        y_lags = -2
      )
    )

    tst <- suppressWarnings(
      estimate(
        data,
        x_lags = -2,
        y_lags = -2
      )
    )
    expect_equal(tst$x_params, 0)
    expect_equal(tst$ar_params, 0)

    tst <- suppressWarnings(
      estimate(
        data,
        x_lags = 1,
        y_lags = -2
      )
    )
    expect_equal(length(tst$x_params), 2)
    expect_equal(tst$ar_params, 0)

    tst <- suppressWarnings(
      estimate(
        data,
        x_lags = -2,
        y_lags = 2
      )
    )
    expect_equal(tst$x_params, 0)
    expect_equal(length(tst$ar_params), 2)
  }
)

test_that(
  "Testing known warnings: NAs in the data",
  {
    # Create data to be used in this test
    data <- data.frame(y = rnorm(100), x = rnorm(100))
    data$y[1:3] <- NA
    data$x[4:6] <- NA

    # NAs in data in `estimate`. Also testing output: By default no warning here
    expect_no_warning(estimate(data))

    tst <- estimate(data)
    expect_equal(tst$x, data$x)
    expect_equal(
      tst$innovations + tst$intercept,
      data$y
    )

    # NAs in data in `compute_innovations`. Also testing output
    expect_warning(
      compute_innovations(
        data,
        ar_params = c(0.5, 0.25),
        x_params = c(2, 2)
      )
    )

    tst <- suppressWarnings(
      compute_innovations(
        data,
        ar_params = c(0.5, 0.25),
        x_params = c(2, 2)
      )
    )
    expect_equal(length(tst), 100)

    # NAs in data in `prepare_data`. Also testing output
    expect_warning(
      prepare_data(
        data,
        x_lags = 2,
        y_lags = 2
      )
    )

    tst <- suppressWarnings(
      prepare_data(
        data,
        x_lags = 2,
        y_lags = 2
      )
    )
    expect_equal(names(tst), c("y", "X"))
    expect_equal(nrow(tst$X), 92)
    expect_equal(length(tst$y), 92)
  }
)

test_that(
  "Testing the recovery of estimation",
  {
    tst <- logical(length(parameters))

    # Loop over each of the parameters
    set.seed(1)
    for (i in seq_along(parameters)) {
      # Generate data
      y <- suppressWarnings(
        irf_generator(
          intercept = parameters[[i]]$intercept,
          x_params = parameters[[i]]$x,
          ar_params = parameters[[i]]$eps,
          x = rnorm(1000),
          innovations = rnorm(1000)
        )$irf
      )

      # Estimate the parameters
      results <- estimate(
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

      tst[i] <- all(abs(params) <= 10^(-1))
    }

    # Do the actual check
    expect_true(all(tst))
  }
)

test_that(
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
    # data `y` if we correctly compute the innovations in `estimate`. innovations
    # and other impulse response functions may deviate, however, due to nonexact
    # recovery of the parameters.
    #
    # To make this point even clearer, This analysis is done for different types
    # of models that do not necessarily correspond to the original generating
    # model
    for (i in seq_along(parameters)) {
      # Generate data
      y <- suppressWarnings(
        irf_generator(
          intercept = parameters[[i]]$intercept,
          x_params = parameters[[i]]$x,
          ar_params = parameters[[i]]$eps,
          x = rnorm(100),
          innovations = rnorm(100)
        )$irf
      )

      # Loop over all possibilities of the lags
      for (j in seq_len(nrow(lags))) {
        # Estimate the parameters and retrieve the results
        results <- estimate(
          y,
          cols = c("irf", "x"),
          y_lags = lags$y_lags[j],
          x_lags = ifelse(is.na(lags$x_lags[j]), NA, lags$x_lags[j] - 1)
        )

        # Create the impulse response functions
        results <- suppressWarnings(
          irf_generator(
            intercept = results$intercept,
            x_params = results$x_params,
            ar_params = results$ar_params,
            x = results$x,
            innovations = results$innovations,
            burnin = FALSE
          )$irf
        )

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
    # data `y` if we correctly compute the innovations in `estimate`. innovations
    # and other impulse response functions may deviate, however, due to nonexact
    # recovery of the parameters.
    #
    # To make this point even clearer, This analysis is done for different types
    # of models that do not necessarily correspond to the original generating
    # model
    for (i in seq_along(parameters)) {
      # Generate data
      y <- suppressWarnings(
        irf_generator(
          intercept = parameters[[i]]$intercept,
          x_params = parameters[[i]]$x,
          ar_params = parameters[[i]]$eps,
          x = rnorm(100),
          innovations = rnorm(100)
        )$irf
      )

      # Loop over all possibilities of the lags
      for (j in seq_len(nrow(lags))) {
        # Estimate the parameters and retrieve the results
        results <- estimate(
          y,
          cols = c("irf", "x"),
          y_lags = lags$y_lags[j],
          x_lags = ifelse(is.na(lags$x_lags[j]), NA, lags$x_lags[j] - 1)
        ) |>
          suppressWarnings()

        # Create the impulse response functions
        innovations <- compute_innovations(
          y,
          cols = c("irf", "x"),
          intercept = results$intercept,
          x_params = results$x_params,
          ar_params = results$ar_params,
          innovations = NULL
        ) |>
          suppressWarnings()

        # Check whether the computed innovations are the same in both cases
        tst[i, j] <- all(round(innovations, 4) == round(results$innovations, 4))
      }
    }

    # Do the actual check
    expect_true(all(tst))
  }
)

test_that(
  "Estimation inside and outside `estimate` works",
  {
    # Generate data
    data <- irf_generator(
      intercept = 0,
      ar_params = c(0.75, 0.1),
      x_params = c(2, 1, -0.25),
      x = rnorm(100),
      innovations = rnorm(100)
    )$irf

    # Impose NA values in the data
    set.seed(1)
    idx <- sample(1:100, 10, replace = FALSE)
    data[idx, ] <- NA

    # ADL(2, 2)
    ref <- lm(
      data = data,
      irf ~ dplyr::lag(irf, 1) + x + dplyr::lag(x, 1)
    )

    tst <- estimate(
      data, 
      cols = c("irf", "x"),
      y_lags = 1, 
      x_lags = 1, 
      na_action = "listwise"
    ) |>
      suppressWarnings()

    # Actual test
    expect_equal(
      ref$residuals, 
      tst$fit$residuals
    )
  }
)
