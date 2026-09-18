#' Euclidean algorithm for greatest common divisor
#'
#' Compute the greatest common divisor of two integers using Euclidean algorithm.
#' @param a numeric scalar, input integer
#' @param b numeric scalar, input integer
#' @return integer, greatest common divisor of a and b
#' @references \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#' @export
euclidean <- function(a, b) {
  # 作业要求断言：必须是数值标量
  if (!is.numeric(a) || length(a) != 1 || !is.numeric(b) || length(b) != 1) {
    stop("输入参数 a 和 b 必须是数值标量（长度为1的数值向量）")
  } 
  
  # 取绝对值，将输入转换为整数（防止传入小数）
  a <- abs(as.integer(a))
  b <- abs(as.integer(b))
  
  # 欧几里得算法核心逻辑
  while (b != 0) {
    temp <- b
    b <- a %% b
    a <- temp
  }
  
  return(a)
}