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

# # Should be more general: How many innovations are there etc
# test_that(
#   "Compute input works when NAs are present",
#   {
    
#   }
# )