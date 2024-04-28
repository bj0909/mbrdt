#' @title Metropolis-Hastings Method for sampling the posterior distribution given Failing the RDT
#'
#' @description This function uses the Metropolis-Hastings algorithm to generate a sample of
#' failure probabilities given failinig the RDT.
#'
#' @param sampleSize The size of the sample to be generated.
#' @param startVal The initial value of the Markov Chain.
#' @param sd The standard deviation for generating the proposal distribution.
#' @param n RDT testing sample size.
#' @param c The maximum number of allowable failures.
#' @param betaPriorParam1 Parameter 1 for the Beta distribution.
#' @param betaPriorParam2 Parameter 2 for the Beta distribution.
#'
#' @return A vector containing the Markov Chain sample of failure probabilities for the posterior given failing the RDT.
#' @importFrom stats rnorm runif
#' @export
#'
#' @examples
#' samples_fail <- mh_sampling_fail(sampleSize = 1000,
#' startVal = 0.2,
#' sd = 0.1,
#' n = 100,
#' c = 10,
#' betaPriorParam1 = 2,
#' betaPriorParam2 = 5)
#'
#' samples_fail
#'

mh_sampling_fail <- function(sampleSize, startVal, sd, n, c,
                             betaPriorParam1, betaPriorParam2){
  x <- rep(NA,sampleSize)
  x[1] <- startVal
  for(i in 2:sampleSize){
    current <- x[i-1]
    proposed <- rnorm(1,mean = current,sd = sd)
    if(!(proposed>0 & proposed<1)){
      ratio <- 0
    } else if (targetFail(current, n, c, betaPriorParam1, betaPriorParam2)==0) {
      ratio <- 1
    } else {
      ratio <- min(1, exp(log(targetFail(proposed, n, c, betaPriorParam1, betaPriorParam2))-
                            log(targetFail(current, n, c, betaPriorParam1, betaPriorParam2))))
    }
    accept <- runif(1) < ratio
    x[i] <- ifelse(accept, proposed, current)
    current <- x[i]
  }
  return(x)
}
