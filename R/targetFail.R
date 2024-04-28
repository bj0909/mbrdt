#' Target Distribution Given Failing the RDT
#'
#' @description This function calculates the target distribution given failing the RDT.
#'
#' @param x Failure probability (must be a value between 0 and 1).
#' @param n RDT testing sample size.
#' @param c The maximum number of allowable failures.
#' @param betaPriorParam1 Parameter 1 for the Beta distribution.
#' @param betaPriorParam2 Parameter 2 for the Beta distribution.
#'
#' @return The target distribution value.
#' @importFrom stats pbinom dbeta
#' @export
#'
#' @examples
#' target_fail <- targetFail(x = 0.2,
#' n = 100,
#' c = 10,
#' betaPriorParam1 = 2,
#' betaPriorParam2 = 5)
#'
#' target_fail
#'

targetFail <- function(x, n, c, betaPriorParam1, betaPriorParam2){
  if(!(x>0 & x<1)) return(0)
  else {
    sum1 <- c()
    for(j in (c+1):n){
      sum1[j-c] <- (pbinom(n,n,x)-pbinom(c,n,x))*dbeta(x, betaPriorParam1, betaPriorParam2)
    }
    return(sum(sum1))
  }
}
