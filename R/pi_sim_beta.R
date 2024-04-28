#' @title Monte Carlo Simulation for failure probabilities pi given Beta prior
#'
#' @description Define the simulation function to generate failure probabilities
#' with Beta prior as conjugate prior to binomial distributions (for binomial RDT).
#'
#' @param M How many samples (pi values) you are going to simulate
#' @param seed Random seed for random sample
#' @param a Shape parameter 1 for beta distribution
#' @param b Shape parameter 2 for beta distribution
#' @return The vector of failure probabilities simulated
#'
#' @examples
#' pi <- pi_sim_beta(M = 100, seed = 10, a = 1, b = 1)
#' pi
#'
#' @export
#' @importFrom stats rbeta

pi_sim_beta <- function(M, seed, a, b){
  #requireNamespace("stats")
  set.seed(seed)
  return(rbeta(M, a, b))
}
