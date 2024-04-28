#' Multi-Stage Binomial Reliability Demonstration Test (MBRDT) Processor
#'
#' This function performs a multi-stage reliability demonstration test (RDT) analysis.
#' It iteratively updates the parameters of the prior distribution and estimates the posterior distribution
#' of failure probabilities based on the test results at each stage.
#'
#'@param maxStageNumber Planning horizon, total number of stages
#'@param n Numeric vector, sample size values
#'@param c Numeric vector, maximum allowable failures values
#'@param R Numeric vector, lower level reliability requirement values
#'@param seed Random seed
#'@param betaPriorParam1 Parameter alpha of the prior beta distribution
#'@param betaPriorParam2 Parameter beta of the prior beta distribution
#'@param sampleSizePass MH sampling size given passing the RDT
#'@param sampleSizeFail MH sampling size given failing the RDT
#'@param startValForMH Starting value for MH sampling
#'@param sdForMH Standard deviation of MH sampling
#'@param M  Simulation Sample Size
#'@param cv variable cost for test
#'@param cf fixed cost for test
#'@param cw warranty cost for each unit
#'@param V  sales volume
#'@param G  Reliability growth cost
#'@param burnInNum Burn-in number of MH sampling
#'@param upperLimit upper limit of reliability after reliability growth
#'@param growthRate parameter included in the RG model, larger value corresponds to slower growth
#'
#' @examples
#' @importFrom stats ks.test rbeta var
#' @importFrom utils write.csv
#' @export
#'
multiStageRdt <- function(maxStageNumber = 2,
                          n = 1:20,
                          c = 0:5,
                          R = 0.85,
                          seed = 123,
                          M = 500,
                          betaPriorParam1 = 1,
                          betaPriorParam2 = 1,
                          sampleSizePass = 200, sampleSizeFail = 200,
                          startValForMH = 0.1, sdForMH = 1,
                          cv = 25, cf = 0,
                          cw = 6, V = 1000, G = 100000,
                          burnInNum = 40,
                          upperLimit = 1, growthRate = 0.7){
  # data includes all possible test plans
  data <- enum_test_plans(n, c, R)

  # Initiate parameters w.r.t prior knowledge of failure probability ~ Beta(betaPriorParam1, betaPriorParam2)
  params <- data.frame(matrix(NA, nrow = dim(data)[1], ncol = 6))
  colnames(params) <- c("alpha_fail", "beta_fail", "alpha_pass", "beta_pass", "ksTestD", "ksTestP")
  params$alpha_fail <- betaPriorParam1
  params$beta_fail <- betaPriorParam2
  params$alpha_pass <- betaPriorParam1
  params$beta_pass <- betaPriorParam2

  # Iterate for all stages
  seed = seed
  M = M
  for(k in 1:maxStageNumber) {
    # Generate M Pi(failure probability) samples for each test plan
    piDataFrame <- data.frame(matrix(NA, nrow = M, ncol = dim(data)[1]))
    for(i in 1:dim(data)[1]){
      piDataFrame[,i] <- pi_sim_beta(M, seed, params$alpha_fail[i], params$beta_fail[i])
      colnames(piDataFrame)[i] <- paste("PI_plan_",i,sep = "")
    }
    # write.csv(piDataFrame, file = paste("PI_stage_",k,".csv",sep = ""),row.names = F)

    # Add consumer risks to data
    data <- add_CR_b(data, M, piDataFrame)

    # Generate samples of the posterior distribution given passing the RDT
    samples_pass<- data.frame(matrix(NA, nrow = sampleSizePass, ncol = dim(data)[1]))
    colnames(samples_pass)<- seq(1:dim(data)[1])
    for(i in 1:dim(data)[1]){
      samples_pass[,i] <- mh_sampling_pass(sampleSizePass, startValForMH, sdForMH, data$n[i], data$c[i],
                                           params$alpha_fail[i], params$beta_fail[i])
    }
    # write.csv(samples_pass, file = paste("samples_pass_", k, ".csv",sep=""), row.names = F)

    # Generate samples of the posterior distribution given failing the RDT
    samples_fail<- data.frame(matrix(NA, nrow = sampleSizeFail, ncol = dim(data)[1]))
    colnames(samples_fail)<- seq(1:dim(data)[1])
    for(i in 1:dim(data)[1]){
      samples_fail[,i] <- mh_sampling_fail(sampleSizeFail, startValForMH, sdForMH, data$n[i], data$c[i],
                                           params$alpha_fail[i], params$beta_fail[i])
    }
    # write.csv(samples_fail, file = paste("samples_fail_", k, ".csv",sep=""), row.names = F)

    # Add results to the data frame
    data <- add_AP(data, M, piDataFrame)
    data <- add_testing_cost(data, cv, cf)
    data <- add_expected_w(data, M, pi, cw, V, G)

    # Output the results for the current iteration
    write.csv(data, file = paste("data_", k, ".csv", sep = ""), row.names = F)

    # Burn-in for MH samples
    samples_fail <- samples_fail[(burnInNum+1):sampleSizeFail,]

    # Reliability Growth
    samples_fail <- 1-standardGompertz(k,upperLimit,(1-samples_fail),growthRate)

    # Update the beta distribution parameters
    muVec_fail <- sapply(samples_fail, mean, na.rm = T)
    muVec_fail <- unname(muVec_fail)
    sigmaSqrVec_fail <- sapply(samples_fail, var, na.rm = T)
    sigmaSqrVec_fail <- unname(sigmaSqrVec_fail)
    for(i in 1:dim(params)[1]){
      params$alpha_fail[i] <- (((1-muVec_fail[i])/sigmaSqrVec_fail[i])-(1/muVec_fail[i]))*muVec_fail[i]^2
      params$beta_fail[i] <- params$alpha_fail[i]*((1/muVec_fail[i])-1)
      # Goodness-of-fit test for the estimated distribution
      res <- ks.test(unique(samples_fail[,i]), rbeta(40, params$alpha_fail[i], params$beta_fail[i]))
      params$ksTestD[i] <- res[[1]]
      params$ksTestP[i] <- res[[2]]
      if(params$ksTestP[i] > 0.05){
        print(paste(k,"Pass the ks test"))
      } else print(paste(k,"Fail the ks test"))
    }

    # Update the beta distribution parameters
    muVec_pass <- sapply(samples_pass, mean, na.rm = T)
    muVec_pass <- unname(muVec_pass)
    sigmaSqrVec_pass <- sapply(samples_pass, var, na.rm = T)
    sigmaSqrVec_pass <- unname(sigmaSqrVec_pass)
    for(i in 1:dim(params)[1]){
      params$alpha_pass[i] <- (((1-muVec_pass[i])/sigmaSqrVec_pass[i])-(1/muVec_pass[i]))*muVec_pass[i]^2
      params$beta_pass[i] <- params$alpha_pass[i]*((1/muVec_pass[i])-1)
    }

    write.csv(params, file = paste("params_stage_",k+1,".csv", sep = ""), row.names = F)
    # Reset data for the next iteration
    data <- enum_test_plans(n, c, R)
  }
  return("Done")
}

