#' Standard Gompertz Reliability Growth Function
#'
#' @description This function calculates the improved reliability at a given stage
#' using the Standard Gompertz reliability growth model.
#'
#' @param t The stage number (starting from 1, for stage2, t=1).
#' @param upperLimit The upper limit of reliability, where 0 < upperLimit <= 1.
#' @param baseLine The initial reliability at time=0, where 0 < baseLine < 1.
#' @param growthRate The growth pattern indicator, where 0 < growthRate < 1.
#' Smaller values of growthRate indicate rapid early reliability growth, while larger values indicate slow reliability growth.
#'
#' @return The reliability at the given stage based on the Standard Gompertz reliability growth model.
#'
#' @examples
#' reliability_at_stage_5 <- standardGompertz(t = 5,
#' upperLimit = 0.99,
#' baseLine = 0.2,
#' growthRate = 0.5)
#'
#' reliability_at_stage_5
#'
#' @export
#'

standardGompertz <- function(t,upperLimit,baseLine,growthRate){
  return(upperLimit*(baseLine/upperLimit)^(growthRate^t))
}
