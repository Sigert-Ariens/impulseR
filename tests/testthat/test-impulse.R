test_that(
  "Testing the impulse function",
  {
    tst_1 <- impulse(10, 1)
    tst_2 <- impulse(5, 2)
    tst_3 <- impulse(7, 7)

    expect_equal(tst_1, c(1, rep(0, 9)))
    expect_equal(tst_2, c(0, 1, 0, 0, 0))
    expect_equal(tst_3, c(rep(0, 6), 1))

    expect_error(impulse(5, 7))
  }
)

test_that(
  "Testing the scaled_impulse function",
  {
    tst_1 <- scaled_impulse(10, 1, -10)
    tst_2 <- scaled_impulse(5, 2, 1)
    tst_3 <- scaled_impulse(7, 7, 2)

    expect_equal(tst_1, c(-10, rep(0, 9)))
    expect_equal(tst_2, c(0, 1, 0, 0, 0))
    expect_equal(tst_3, c(rep(0, 6), 2))

    expect_error(scaled_impulse(5, 7, 3))
  }
)
