#' Add Expected Warranty Services Cost to Data Frame
#'
#' Calculate and add the expected warranty services (WS) cost based on the provided parameters
#' to the input data frame.
#'
#' @param data A data frame containing test plan and cost information.
#' @param M Monte Carlo simulation size.
#' @param pi A matrix containing the failure probability samples for different test plans
#'           (rows represent samples, columns represent test plans).
#' @param cw Average cost per warranty claim.
#' @param V Sales volume.
#' @param G Reliability growth cost.
#'
#' @return The data frame with an additional column for the expected warranty services cost (expW).
#'
#' @examples
#'
#' @export
#'
add_expected_w <- function(data, M, pi, cw, V, G){
  n = data$n
  c = data$c
  D = data$Cost_cnv
  W_cost = c()
  for(i in 1:dim(data)[1]){
    W_cost[i] = bcost_WS(cw,V,n[i],c[i],pi[,i],M)
  }
  data$expW = W_cost
  return(data)
}
