#' Create single unit impulse
#'
#' @param N Integer denoting the size of the impulse vector
#' @param index Integer denoting at which index in the vector to add the single
#' impulse. Defaults to `1`.
#'
#' @returns Numeric vector containing a singular impulse of value `1`.
#'
#' @examples
#' impulse(
#'   10,
#'   index = 1
#' )
#'
#' @export
impulse <- function(N,
                    index = 1) {
  # Check whether the index fits. If not, error is thrown
  if (index > N) {
    stop("Index does not fall within range of the data. Please adjust.")
  }

  # Actual creation of the vector
  x <- numeric(N)
  x[index] <- 1

  return(x)
}

#' Create single scaled impulse
#'
#' @param N Integer denoting the size of the impulse vector
#' @param index Integer denoting at which index in the vector to add the single
#' impulse. Defaults to `1`.
#' @param scale Numeric denoting the value the impulse should be given. Defaults
#' to `1`
#'
#' @returns Numeric vector containing a singular impulse of value `scale`.
#'
#' @examples
#' scaled_impulse(
#'   10,
#'   index = 1,
#'   scale = 2
#' )
#'
#' @export
scaled_impulse <- function(N,
                           index = 1,
                           scale = 1) {
  return(scale * impulse(N, index = index))
}
