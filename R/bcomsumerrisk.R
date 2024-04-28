#' @title Consumer's Risk for Multi-stage Binomial RDT
#'
#' @description Define the consumer's risk function which gets the probability of passing the test when the lower level reliability requirement is not satisfied (for binomial RDT).
#'
#' @param M Monte Carlo simulation size
#' @param n sample size in test plan.
#' @param c Maximum allowable failures in test plan.
#' @param pi Failure probability.
#' @param R Lower level reliability requirement.
#' @return Probability of consumer's risk
#' @examples
#'
#' @export
#' @importFrom stats pbinom

bconsumerrisk<-function(M,n,c,pi,R){
  sum1<-c()
  sum2<-c()
  for(i in 1:M){
    sum1[i]<-pbinom(c,n,pi[i])*bIndicator(pi[i],R)
    sum2[i]<-pbinom(c,n,pi[i])
  }
  return(1-sum(sum1)/sum(sum2))
}
