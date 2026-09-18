#' Euclidean algorithm
#'
#' This function computes the greatest common divisor (GCD) of two integers using the Euclidean algorithm.
#'
#' @param a A numeric scalar representing the first integer.
#' @param b A numeric scalar representing the second integer.
#' @return A numeric scalar representing the greatest common divisor (GCD) of a and b.
#' @references \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#' @export
euclidean <- function(a, b) {
  # Validates inputs
  tol <- .Machine$double.eps ^ 0.5
  if (
    !is.numeric(a) || length(a) != 1 || abs(a - round(a)) > tol
    || !is.numeric(b) || length(b) != 1 || abs(b - round(b)) > tol) {
    stop("a and b must be scalar integers!")
  }
  
  # Converts the numbers to positive. Negative numbers have the same GCDs.
  a <- abs(round(a))
  b <- abs(round(b))
  
  # Executes the Euclidean algorithm.
  while (b != 0) {
    temp <- b
    b <- a %% b
    a <- temp
  }
  
  return(a)
}
