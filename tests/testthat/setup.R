# Create some parameter sets to be used in the test. Contains no lag, single
# lag, two lags for epsilon and/or x. Values for these lags can also be negative.
# Slopes for x can be positive, negative, or no effect.
eps_params <- list(
  0,
  0.5,
  c(0.5, 0.25),
  c(0.5, 0.25, -0.1),
  -0.5,
  c(-0.5, 0.25),
  c(-0.5, 0.25, -0.1)
)

x_params <- append(
  lapply(eps_params, \(x) 2 * x),
  append(
    lapply(eps_params, \(x) 0 * x),
    lapply(eps_params, \(x) -2 * x)
  )
)

parameters <- list()
for (i in eps_params) {
  for (j in x_params) {
    parameters <- append(
      parameters,
      list(list("intercept" = 0, "eps" = i, "x" = j))
    )
  }
}

# Define the number of lags for each of the parameter sets, allowing us to recover
# the parameters
est_eps_lags <- c(NA, 1, 2, 3, 1, 2, 3)
est_x_lags <- rep(est_eps_lags, times = 3)

est_lags <- cbind(
  rep(est_eps_lags, each = length(est_x_lags)),
  rep(est_x_lags, times = length(est_eps_lags))
)
