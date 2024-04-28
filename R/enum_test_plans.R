#' @title Generate all test plans for BRDT (binary relibility demonstration test)
#'
#' @description Generates a dataframe including all possible combinations of sample size (n),
#'              maximum allowable failures (c), and lower level reliability requirement (R).
#'
#' @param n Numeric vector, sample size values
#' @param c Numeric vector, maximum allowable failures values
#' @param R Numeric vector, lower level reliability requirement values
#'
#' @return A dataframe with all possible combinations of n, c, and R
#'
#' @examples
#' n <- 1:10
#' c <- 0:5
#' R <- 0.8
#' enum_test_plans(n, c, R)
#'
#' @export
enum_test_plans <- function(n, c, R) {
  # Create all possible combinations of n, c, and R
  data <- expand.grid(n = n, c = c, R = R)

  # test sample size need to be no fewer than failures
  data <- data[which(data$n >= data$c),]
  colnames(data) <- c('n', 'c', 'R')

  return(data)
}
