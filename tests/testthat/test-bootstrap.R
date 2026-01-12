test_that(
  "Testing known errors",
  {
    params <- c(1, 0.1, 0.1, 0.1, 0.1)

    # Covariances are not numeric matrix
    sigma <- matrix("test", nrow = 5, ncol = 5)
    expect_error(bootstrap(params, sigma, x = impulse(10)))

    sigma <- matrix("0", nrow = 5, ncol = 5)
    expect_error(bootstrap(params, sigma, x = impulse(10)))

    sigma <- rep(1, 5)
    expect_error(bootstrap(params, sigma, x = impulse(10)))

    # Covariances are not positive definite

    # Covariances do not correspond to parameter vector
    sigma <- matrix(0, nrow = 5, ncol = 2)
    expect_error(bootstrap(params, sigma, x = impulse(10)))

    sigma <- matrix(0, nrow = 2, ncol = 5)
    expect_error(bootstrap(params, sigma, x = impulse(10)))

    # Lags do not correspond to parameter vector
    sigma <- diag(5) * 0.05^2
    expect_error(
      bootstrap(
        params, 
        sigma, 
        y_lags = 2, 
        x_lags = 3, 
        x = impulse(10)
      )
    )
    expect_error(
      bootstrap(
        params, 
        sigma, 
        y_lags = 3, 
        x_lags = 2, 
        x = impulse(10)
      )
    )
    expect_error(
      bootstrap(
        params, 
        sigma, 
        y_lags = 1, 
        x_lags = 1, 
        x = impulse(10)
      )
    )
    expect_error(
      bootstrap(
        params, 
        sigma, 
        y_lags = 2, 
        x_lags = 0, 
        x = impulse(10)
      )
    )

    set.seed(1)
    expect_no_error(
      bootstrap(
        params, 
        sigma, 
        y_lags = 2, 
        x_lags = 1, 
        x = impulse(10),
        N = 10
      )
    )
    expect_no_error(
      bootstrap(
        params, 
        sigma, 
        y_lags = 1, 
        x_lags = 2, 
        x = impulse(10),
        N = 10
      )
    )
    expect_no_error(
      bootstrap(
        params, 
        sigma, 
        y_lags = 3, 
        x_lags = 0, 
        x = impulse(10),
        N = 10
      )
    )
    expect_no_error(
      bootstrap(
        params, 
        sigma, 
        y_lags = 4, 
        x_lags = NA,
        x = impulse(10),
        N = 10
      )
    )
    expect_no_error(
      bootstrap(
        params, 
        sigma, 
        y_lags = NA,
        x_lags = 3,
        x = impulse(10),
        N = 10
      )
    )

    # Parameter names do not correspond to parameter vector
    expect_error(
      bootstrap(
        params, 
        sigma, 
        parameter_names = rep("test", 4), 
        x = impulse(10)
      )
    )
    expect_error(
      bootstrap(
        params, 
        sigma, 
        parameter_names = rep("test", 6), 
        x = impulse(10)
      )
    )

    expect_error(
      bootstrap(
        params, 
        sigma, 
        parameter_names = c("intercept", rep("test", 4)), 
        x = impulse(10)
      ) |>
        suppressWarnings()
    )
  }
)

test_that(
  "Testing known warnings",
  {
    params <- c(1, 0.1, 0.1, 0.1, 0.1)
    sigma <- diag(5) * 0.05^2

    # Impossible lags are specified
    expect_warning(
      bootstrap(
        params, 
        sigma, 
        y_lags = 0, 
        x_lags = 3,
        x = impulse(10)
      )
    )
    expect_warning(
      bootstrap(
        params, 
        sigma, 
        y_lags = 4, 
        x_lags = -1,
        x = impulse(10)
      )
    )

    # Weird parameters are provided
    expect_warning(
      bootstrap(
        params,
        sigma,
        parameter_names = c(
          "intercept", 
          "y_1", 
          "y_2", 
          "x_1", 
          "x_2",
          "test"
        ),
        x = impulse(10)
      )
    )
  }
)

test_that(
  "Check the output: Parameters",
  {
    intercept <- 5
    ar_params <- c(0.75, 0.25, 0.1, -0.1)
    x_params <- c(5, 3, -2, -1)

    params <- c(intercept, ar_params, x_params)
    sigma <- diag(length(params))

    # Generate bootstrap samples for according to the specified parameters. 
    # Note that the process is nonstationary, which is why I included a 
    # suppressWarnings here.
    set.seed(1)
    samples <- bootstrap(
      params,
      sigma, 
      y_lags = length(ar_params),
      x_lags = length(x_params) - 1,
      x = impulse(10),
      innovations = impulse(10),
      burnin = TRUE,
      N = 1000
    ) |>
      suppressWarnings()

    # Extract the generated parameters, ready for the different tests
    parameters <- samples$parameters

    # Check the column names
    expect_equal(
      colnames(parameters),
      c(
        "intercept", 
        paste("y_", 1:length(ar_params), sep = ""), 
        paste("x_", 1:length(x_params) - 1, sep = "")
      )
    )

    # Check number of parameters generated
    expect_equal(nrow(parameters), 1000)

    # Check mean values of the parameters generated
    expect_equal(
      colMeans(parameters) |>
        `names<-`(NULL),
      params,
      tolerance = 1e-1
    )

    # Check variation around the values of the parameters generated
    expect_equal(
      sapply(parameters, var) |>
        as.numeric() |>
        `names<-`(NULL),
      diag(sigma),
      tolerance = 1e-1
    )

    # Check the correlations between the parameters that are generated
    S <- diag(length(params))
    S <- S * 0.5

    C <- diag(length(params))
    C[lower.tri(C)] <- 0.25
    C[upper.tri(C)] <- 0.25

    sigma <- S %*% C %*% t(S)

    set.seed(1)
    samples <- bootstrap(
      params,
      sigma, 
      y_lags = length(ar_params),
      x_lags = length(x_params) - 1,
      x = impulse(10),
      innovations = impulse(10),
      burnin = TRUE,
      N = 1000
    ) |>
      suppressWarnings()

    expect_equal(
      cor(samples$parameters) |>
        as.matrix() |>
        `dimnames<-`(NULL),
      C,
      tolerance = 1e-1
    )
  }
)

test_that(
  "Check the output: Parameters when specified by the user",
  {
    ar_params <- c(0.1, -0.1)
    x_params <- c(-2, -1)
    parameter_names <- c("y_3", "y_4", "x_2", "x_4")

    params <- c(ar_params, x_params)
    sigma <- diag(length(params))

    # Generate bootstrap samples for according to the specified parameters. 
    # Note that the process is nonstationary, which is why I included a 
    # suppressWarnings here.
    set.seed(1)
    samples <- bootstrap(
      params,
      sigma,
      parameter_names = parameter_names,
      x = impulse(10),
      innovations = impulse(10),
      burnin = TRUE,
      N = 1000
    ) |>
      suppressWarnings()

    # Extract the generated parameters, ready for the different tests
    parameters <- samples$parameters

    # Check the column names
    expect_equal(
      colnames(parameters),
      c(
        "intercept", 
        paste("y_", 1:4, sep = ""), 
        paste("x_", 0:4, sep = "")
      )
    )

    # Check number of parameters generated
    expect_equal(nrow(parameters), 1000)

    # Check whether only those parameters that are actually provided have zero
    # values
    expect_equal(
      sapply(
        colnames(parameters)[!(colnames(parameters) %in% parameter_names)],
        function(x) mean(parameters[, x])
      ) |>
        `names<-`(NULL),
      rep(0, 6)
    )

    expect_equal(
      sapply(
        colnames(parameters)[!(colnames(parameters) %in% parameter_names)],
        function(x) sd(parameters[, x])
      ) |>
        `names<-`(NULL),
      rep(0, 6)
    )

    expect_equal(
      sapply(
        parameter_names, 
        function(x) mean(parameters[, x])
      ) |>
        `names<-`(NULL),
      params,
      tolerance = 1e-1
    )

    expect_equal(
      sapply(
        parameter_names, 
        function(x) sd(parameters[, x])
      ) |>
        `names<-`(NULL),
      diag(sigma),
      tolerance = 1e-1
    )

    # Check the correlations between the parameters that are generated
    S <- diag(length(params))
    S <- S * 0.5

    C <- diag(length(params))
    C[lower.tri(C)] <- 0.25
    C[upper.tri(C)] <- 0.25

    sigma <- S %*% C %*% t(S)

    set.seed(1)
    samples <- bootstrap(
      params,
      sigma, 
      parameter_names = parameter_names,
      x = impulse(10),
      innovations = impulse(10),
      burnin = TRUE,
      N = 1000
    ) |>
      suppressWarnings()

    expect_equal(
      cor(samples$parameters[, parameter_names]) |>
        as.matrix() |>
        `dimnames<-`(NULL),
      C,
      tolerance = 1e-1
    )
  }
)

test_that(
  "Check the output: System responses",
  {
    intercept <- 1
    ar_params <- c(0.5, 0.1)
    x_params <- c(5, 3)

    params <- c(intercept, ar_params, x_params)
    sigma <- diag(length(params)) * c(1, 0.1, 0.1, 1, 1)

    # Generate bootstrap samples for according to the specified parameters. 
    # Note that the process is nonstationary, which is why I included a 
    # suppressWarnings here.
    set.seed(1)
    samples <- bootstrap(
      params,
      sigma,
      x_lags = 1,
      y_lags = 2,
      x = impulse(10),
      innovations = impulse(10),
      burnin = TRUE,
      N = 1000
    ) |>
      suppressWarnings()

    # Extract the samples
    samples <- samples$samples

    # Check length of the output
    expect_equal(nrow(samples), 1000 * 10)

    # Check number of samples drawn
    expect_equal(unique(samples$sample), 1:1000)

    # Check mean system response and whether it corresponds to the actual system
    # response
    ref <- irf_generator(
      intercept = intercept,
      ar_params = ar_params,
      x_params = x_params,
      x = impulse(10),
      innovations = impulse(10),
      burnin = TRUE
    )$irf

    expect_equal(
      sapply(
        0:9,
        function(i) median(samples$irf[samples$time == i])
      ),
      ref$irf, 
      tolerance = 1
    )

    expect_equal(
      sapply(
        0:9,
        function(i) median(samples$irf_intercept[samples$time == i])
      ),
      ref$irf_intercept, 
      tolerance = 1
    )

    expect_equal(
      sapply(
        0:9,
        function(i) median(samples$irf_x[samples$time == i])
      ),
      ref$irf_x, 
      tolerance = 1
    )

    expect_equal(
      sapply(
        0:9,
        function(i) median(samples$irf_v[samples$time == i])
      ),
      ref$irf_v, 
      tolerance = 1
    )
  }
)

test_that(
  "Check the output: Trivial case when no covariances are specified",
  {
    intercept <- 5
    ar_params <- c(0.75, 0.25, 0.1, -0.1)
    x_params <- c(5, 3, -2, -1)

    params <- c(intercept, ar_params, x_params)
    sigma <- matrix(0, nrow = length(params), ncol = length(params))

    # Generate bootstrap samples for according to the specified parameters. 
    # Note that the process is nonstationary, which is why I included a 
    # suppressWarnings here.
    set.seed(1)
    samples <- bootstrap(
      params,
      sigma, 
      y_lags = length(ar_params),
      x_lags = length(x_params) - 1,
      x = impulse(10),
      innovations = impulse(10),
      burnin = TRUE,
      N = 1000
    ) |>
      suppressWarnings()

    # Extract the samples and parameters
    parameters <- samples$parameters
    samples <- samples$samples

    # Check whether all parameters correspond to the ones provided
    expect_true(
      all(
        sapply(
          seq_along(params),
          function(i) all(parameters[, i] == params[i])
        )
      )
    )

    # Check whether all samples just correspond to the reference sample
    ref <- irf_generator(
      intercept = intercept,
      ar_params = ar_params,
      x_params = x_params,
      x = impulse(10),
      innovations = impulse(10),
      burnin = TRUE
    )$irf

    expect_true(
      all(
        sapply(
          1:1000,
          function(i) all(samples$irf[samples$sample == i] == ref$irf)
        )
      )
    )

    expect_true(
      all(
        sapply(
          1:1000,
          function(i) all(samples$irf_intercept[samples$sample == i] == ref$irf_intercept)
        )
      )
    )

    expect_true(
      all(
        sapply(
          1:1000,
          function(i) all(samples$irf_x[samples$sample == i] == ref$irf_x)
        )
      )
    )

    expect_true(
      all(
        sapply(
          1:1000,
          function(i) all(samples$irf_v[samples$sample == i] == ref$irf_v)
        )
      )
    )
  }
)

test_that(
  "Bootstrap puts confidence intervals around innovations, not observed data",
  {
    # Create data
    set.seed(1)
    data <- irf_generator(
      intercept = 0, 
      ar_params = c(0.75, 0.2),
      x_params = c(2, 1),
      x = rnorm(100),
      innovations = rnorm(100)
    )$irf

    # Use irf_empirical with confidence intervals
    results <- irf_empirical(
      data, 
      cols = c("irf", "x"),
      y_lags = 1, 
      x_lags = 1,
      confidence_interval = TRUE
    )$irf

    # Check whether there is no variation around the observed data: The observed
    # irf is the one thing we should be sure of
    expect_equal(
      mean(abs(results$irf_lower - results$irf_upper)),
      0
    )
    expect_equal(
      mean(abs(results$irf - results$irf_upper)),
      0
    )
    expect_equal(
      mean(abs(results$irf_lower - results$irf)),
      0
    )
  }
)

test_that(
  "Bootstrap puts confidence intervals around data when computing responses",
  {
    # Create data
    set.seed(1)
    data <- irf_generator(
      intercept = 0, 
      ar_params = c(0.75, 0.2),
      x_params = c(2, 1),
      x = rnorm(100),
      innovations = rnorm(100)
    )$irf

    # Use irf_empirical without burnin
    results <- irf_empirical(
      data, 
      cols = c("irf", "x"),
      y_lags = 1, 
      x_lags = 1,
      confidence_interval = TRUE,
      x = impulse(10),
      innovations = impulse(10),
      burnin = FALSE
    )$irf

    # Check whether there is variation around the observed data, should be higher
    # than 0
    expect_true(mean(abs(results$irf_lower - results$irf_upper)) > 0)
    expect_true(mean(abs(results$irf - results$irf_upper)) > 0)
    expect_true(mean(abs(results$irf_lower - results$irf)) > 0)

    # Use irf_empirical with burnin
    results <- irf_empirical(
      data, 
      cols = c("irf", "x"),
      y_lags = 1, 
      x_lags = 1,
      confidence_interval = TRUE,
      x = impulse(10),
      innovations = impulse(10),
      burnin = TRUE
    )$irf

    # Same test as before
    expect_true(mean(abs(results$irf_lower - results$irf_upper)) > 0)
    expect_true(mean(abs(results$irf - results$irf_upper)) > 0)
    expect_true(mean(abs(results$irf_lower - results$irf)) > 0)
  }
)

test_that(
  "Test of zero confidence intervals with data containing missing values",
  {
    # Create data
    set.seed(1)
    data <- irf_generator(
      intercept = 0, 
      ar_params = c(0.75, 0.2),
      x_params = c(2, 1),
      x = rnorm(100),
      innovations = rnorm(100)
    )$irf

    # Add random missing values
    idx <- sample(1:100, 10, replace = FALSE)
    data[idx, ] <- NA

    # Use irf_empirical with confidence intervals
    results <- irf_empirical(
      data, 
      cols = c("irf", "x"),
      y_lags = 1, 
      x_lags = 1,
      confidence_interval = TRUE
    )$irf |>
      suppressWarnings()

    # Check whether there is no variation around the observed data: The observed
    # irf is the one thing we should be sure of
    expect_equal(
      mean(abs(results$irf_lower - results$irf_upper)),
      0
    )
    expect_equal(
      mean(abs(results$irf - results$irf_upper)),
      0
    )
    expect_equal(
      mean(abs(results$irf_lower - results$irf)),
      0
    )
  }
)

test_that(
  "Test of nonzero confidence intervals with data containing missing values",
  {
    # Create data
    set.seed(1)
    data <- irf_generator(
      intercept = 0, 
      ar_params = c(0.75, 0.2),
      x_params = c(2, 1),
      x = rnorm(250),
      innovations = rnorm(250)
    )$irf

    # Estimate the system responses without NAs, for burnin being FALSE or TRUE
    ref_F <- irf_empirical(
      data, 
      cols = c("irf", "x"),
      y_lags = 1, 
      x_lags = 1,
      confidence_interval = TRUE,
      x = impulse(10),
      innovations = impulse(10),
      burnin = FALSE
    )$irf

    ref_T <- irf_empirical(
      data, 
      cols = c("irf", "x"),
      y_lags = 1, 
      x_lags = 1,
      confidence_interval = TRUE,
      x = impulse(10),
      innovations = impulse(10),
      burnin = TRUE
    )$irf

    # Add random missing values
    idx <- sample(1:nrow(data), 10, replace = FALSE)
    data[idx, ] <- NA

    # Use irf_empirical without burnin
    tst <- irf_empirical(
      data, 
      cols = c("irf", "x"),
      y_lags = 1, 
      x_lags = 1,
      confidence_interval = TRUE,
      x = impulse(10),
      innovations = impulse(10),
      burnin = FALSE
    )$irf |>
      suppressWarnings()

    # Check whether the confidence intervals are roughly the same
    expect_equal(
      ref_F$irf_lower, 
      tst$irf_lower,
      tolerance = 1e-1
    )
    expect_equal(
      ref_F$irf, 
      tst$irf,
      tolerance = 1e-1
    )
    expect_equal(
      ref_F$irf_upper, 
      tst$irf_upper,
      tolerance = 1e-1
    )
    
    # Use irf_empirical with burnin
    tst <- irf_empirical(
      data, 
      cols = c("irf", "x"),
      y_lags = 1, 
      x_lags = 1,
      confidence_interval = TRUE,
      x = impulse(10),
      innovations = impulse(10),
      burnin = TRUE
    )$irf |>
      suppressWarnings()

    # Same test as before
    expect_equal(
      ref_T$irf_lower, 
      tst$irf_lower,
      tolerance = 1e-1
    )
    expect_equal(
      ref_T$irf, 
      tst$irf,
      tolerance = 1e-1
    )
    expect_equal(
      ref_T$irf_upper, 
      tst$irf_upper,
      tolerance = 1e-1
    )
  }
)
