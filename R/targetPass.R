#' Target Distribution Given Passing the RDT
#'
#' @description This function calculates the target distribution given passing the RDT.
#'
#' @param x Failure probability (must be a value between 0 and 1).
#' @param n RDT testing sample size.
#' @param c The maximum number of allowable failures.
#' @param betaPriorParam1 Parameter 1 for the Beta distribution.
#' @param betaPriorParam2 Parameter 2 for the Beta distribution.
#'
#' @importFrom stats pbinom dbeta
#'
#' @return The target distribution value.
#'
#' @examples
#' samples <- targetPass(x = 0.2,
#' n = 100,
#' c = 10,
#' betaPriorParam1 = 2,
#' betaPriorParam2 = 5)
#'
#' samples
#'
#' @export

targetPass <- function(x, n, c, betaPriorParam1, betaPriorParam2){
  if(!(x>0 & x<1)) return(0)
  else {
    sum1 <- c()
    for (j in 0:c) {
      sum1[j+1] <- pbinom(c, n, x)*dbeta(x, betaPriorParam1, betaPriorParam2)
    }
    return(sum(sum1))
  }
}
