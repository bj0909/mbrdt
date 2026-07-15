# Small, fast configuration for demonstrating the maintained workflow.
# These values are illustrative and are not intended to reproduce Paper 1 results.
example_parameters <- list(
  simulation = list(
    maxStageNumber = 2,
    n = 1:3,
    c = 0,
    R = 0.85,
    seed = 123,
    M = 50,
    betaPriorParam1 = 1,
    betaPriorParam2 = 1,
    sampleSizePass = 50,
    sampleSizeFail = 50,
    startValForMH = 0.1,
    sdForMH = 0.1,
    cv = 25,
    cf = 0,
    cw = 6,
    V = 1000,
    G = 100000,
    burnInNum = 10,
    upperLimit = 1,
    growthRate = 0.7
  ),
  postprocessing = list(
    continuation_costs = 100000,
    terminal_penalty = 200000,
    beta = 1
  )
)
