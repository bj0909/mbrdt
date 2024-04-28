#' Add BRDT Testing Cost
#'
#' @description This function calculates and adds the Binomial RDT Testing Cost to the input data frame.
#'
#' @param data The data frame containing all possible test plans.
#' @param cv The average unit testing cost per testing sample.
#' @param cf The average fixed cost per RDT.
#'
#' @return The data frame with an additional column for Binomial RDT Testing Cost.
#'
#' @examples
#' @export
#'

add_testing_cost <- function(data,cv,cf){
  n = data$n
  testing_cost <- c()
  for (i in 1:dim(data)[1]) {
    testing_cost[i] <- cv*n[i]+cf
  }
  data$testing_cost <- testing_cost
  return(data)
}
