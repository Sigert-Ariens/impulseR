test_that(
  "Compute input works when no NAs are present",
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

    # Compute the innovations for this dataset
    tst <- compute_input(
      data, 
      cols = c("y", "x"),
      intercept = 0, 
      ar_params = c(0.25, 0.1),
      x_params = c(2, 1, 0.5)
    )

    # Check the output
    expect_equal(
      tst$x, 
      data$x
    )
    expect_equal(
      tst$innovations,
      innovations
    )
  }
)

test_that(
  "Compute input works when NAs are present",
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

    # Compute the innovations and x for this dataset
    input <- compute_input(
      data, 
      cols = c("y", "x"),
      intercept = 0, 
      ar_params = c(0.25, 0.1),
      x_params = c(2, 1, 0.5)
    )

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
      innovations = input$innovations, 
      x = input$x
    )

    # Check the output
    expect_equal(
      tst$irf$irf, 
      data$y[!is.na(data$y)]
    )
  }
)