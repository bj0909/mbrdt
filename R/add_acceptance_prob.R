#' Add Acceptance Probability (AP) to data frame
#'
#' @description This function calculates and adds the Acceptance Probability (AP) values to the input data frame.
#'
#' @param data The data frame containing all possible test plans.
#' @param M Monte Carlo simulation size.
#' @param pi A matrix containing the failure probability samples for different
#'        test plans (rows represent samples, columns represent test plans).
#'
#' @return The data frame with an additional column for Acceptance Probability (AP) values.
#'
#' @examples
#'
#' @export
#'

add_AP <- function(data,M,pi){
  n = data$n
  c = data$c
  AP = c()
  for(i in 1:dim(data)[1]){
    AP[i] = bacceptprob(M,n[i],c[i],pi[,i])
  }
  data$AP = AP
  return(data)
}
