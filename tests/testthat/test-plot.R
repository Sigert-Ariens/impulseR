# We may need to look at vdiffr at some point if we wish to test our plotting 
# function

# testthat::test_that(
#   "Testing whether two plotting methods converge",
#   {
#     ref <- impulseR::irf_plot(
#       intercept = 0,
#       x_params = c(2, -1),
#       ar_params = c(0.75, 0.25),
#       x = impulseR::impulse(10, 1),
#       innovations = impulseR::scaled_impulse(10, 1, -1)
#     )

#     results <- impulseR::irf_generator(
#       intercept = 0,
#       x_params = c(2, -1),
#       ar_params = c(0.75, 0.25),
#       x = impulseR::impulse(10, 1),
#       innovations = impulseR::scaled_impulse(10, 1, -1)
#     )$irf
#     tst <- impulseR::irf_plot(results)

#     testthat::expect_equal(tst, ref)
#   }
# )

# Example case
# y <- irf_generator(
#     0, 
#     c(0.25, 0.1, 0.1),
#     c(3, 2, 1),
#     x = rnorm(20),
#     innovation = rnorm(20)
# )

# result <- irf_empirical(
#     y$irf,
#     cols = c("irf", "x"),
#     y_lags = 1, 
#     x_lags = 1,
#     confidence_interval = TRUE
# )

# irf_plot(result$irf)
# irf_plot(result$irf, cols = "irf")
