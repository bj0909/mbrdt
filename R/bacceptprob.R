#' Acceptance Probability for Binomial RDT
#'
#' @description Define the acceptance probability function which gets the probability of passing the test (for binomial RDT).
#'
#' @param M Sample size of Monte Carlo simulation
#' @param n Testing sample size
#' @param c Maximum allowable failures
#' @param pi Failure probability
#' @return Acceptance probability
#' @examples
#' pi <- pi_sim_beta(M = 100, seed = 10, a = 1, b = 1)
#' bacceptprob(M = 100, n = 10, c = 2, pi = pi)
#'
#' @export
#' @importFrom stats pbinom
#'
bacceptprob<-function(M,n,c,pi){
  sum<-c()
  for(i in 1:M){
    sum[i]<-pbinom(c,n,pi[i])
  }
  return(mean(sum))
}
