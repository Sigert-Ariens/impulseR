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