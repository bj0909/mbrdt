#' @title Add Consumer Risks (CR) to data frame given failure probabilities
#'
#' @description This function calculates and adds the Consumer Risk (CR) values to the
#'              input data frame for a Binomial Randomized Diagnostic Trial (RDT).
#'
#' @param data The data frame containing all possible test plans.
#' @param M Monte Carlo simulation size.
#' @param pi A matrix containing the failure probability samples for different
#'        test plans (rows represent samples, columns represent test plans).
#' @return The data frame with an additional column for Consumer Risk (CR) values.
#'
#' @examples
#'


add_CR_b <- function(data,M,pi){
  n = data$n
  c = data$c
  R = data$R
  CR_b <- c()
  for(i in 1:dim(data)[1]){
    CR_b[i] = bconsumerrisk(M,n[i],c[i],pi[,i],R[i])
  }
  data$CR_b = CR_b
  return(data)
}
