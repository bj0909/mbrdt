#' Calculate Warranty Services Cost
#'
#' @description Define the cost function of warranty services (WS) after the decision of the test (for binomial RDT)
#'
#' @param cw Average cost per warranty claim
#' @param V Sales volume
#' @param n RDT sample size
#' @param c Maximum allowable failures
#' @param pi Failure probability
#' @param M Monte Carlo simulation size
#' @return The warranty services cost
#' @examples
#' pi = pi_sim_beta(M = 100, seed = 10, a = 1, b = 1)
#' cw = 200
#' V = 500
#' n = 100
#' c = 5
#' M = 1000
#'
#' warranty_cost = bcost_WS(cw, V, n, c, pi, M)
#' warranty_cost
#'
#' @importFrom stats pbinom
#' @export
#'
bcost_WS <- function(cw,V,n,c,pi,M){
  sum1 <- c()
  for (i in 1:M) {
    sum1[i] <- pi[i]*pbinom(c,n,pi[i])
  }
  return(cw*V*mean(sum1))
}



