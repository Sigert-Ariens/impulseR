testthat::test_that(
  "Testing the impulse function",
  {
    tst_1 <- impulseR::impulse(10, 1)
    tst_2 <- impulseR::impulse(5, 2)
    tst_3 <- impulseR::impulse(7, 7)

    testthat::expect_equal(tst_1, c(1, rep(0, 9)))
    testthat::expect_equal(tst_2, c(0, 1, 0, 0, 0))
    testthat::expect_equal(tst_3, c(rep(0, 6), 1))

    testthat::expect_error(impulseR::impulse(5, 7))
  }
)

testthat::test_that(
  "Testing the scaled_impulse function",
  {
    tst_1 <- impulseR::scaled_impulse(10, 1, -10)
    tst_2 <- impulseR::scaled_impulse(5, 2, 1)
    tst_3 <- impulseR::scaled_impulse(7, 7, 2)

    testthat::expect_equal(tst_1, c(-10, rep(0, 9)))
    testthat::expect_equal(tst_2, c(0, 1, 0, 0, 0))
    testthat::expect_equal(tst_3, c(rep(0, 6), 2))

    testthat::expect_error(impulseR::scaled_impulse(5, 7, 3))
  }
)
