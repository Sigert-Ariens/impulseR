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
