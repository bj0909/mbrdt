library(mbrdt)

assert_equal <- function(actual, expected, tolerance = 1e-10) {
  if (!isTRUE(all.equal(actual, expected, tolerance = tolerance))) {
    stop(sprintf("Expected %s but got %s", deparse(expected), deparse(actual)))
  }
}

assert_error <- function(expression, pattern) {
  message <- tryCatch({
    force(expression)
    NULL
  }, error = conditionMessage)
  if (is.null(message) || !grepl(pattern, message, fixed = TRUE)) {
    stop(sprintf("Expected error containing '%s'", pattern))
  }
}

stage <- function(ids = 1:2, ap = c(0.5, 0.8), risk = c(0.02, 0.03),
                  testing = c(10, 20), warranty = c(5, 4)) {
  data.frame(
    plan = ids,
    AP = ap,
    CR_b = risk,
    testing_cost = testing,
    expW = warranty
  )
}

# One stage: testing + expected warranty + rejection probability * penalty.
one <- combine_stage_outputs(list(stage(ids = 1, ap = 0.5, risk = 0.01,
                                        testing = 10, warranty = 5)),
                             id_cols = "plan")
one_cost <- calculate_recursive_costs(one, terminal_penalty = 100)
assert_equal(one_cost$plans$total_expected_cost, 65)

# Two stages: stage 2 = 10 + 5 + 0.5 * 100 = 65;
# stage 1 = 10 + 5 + 0.5 * (20 + 65) = 57.5.
two <- combine_stage_outputs(list(
  stage(ids = 1, ap = 0.5, risk = 0.01, testing = 10, warranty = 5),
  stage(ids = 1, ap = 0.5, risk = 0.02, testing = 10, warranty = 5)
), id_cols = "plan")
two_cost <- calculate_recursive_costs(two, continuation_costs = 20,
                                      terminal_penalty = 100)
assert_equal(two_cost$plans$total_expected_cost, 57.5)

# Three stages: C3 = 65; C2 = 10 + 5 + 0.5 * (20 + 65) = 57.5;
# C1 = 10 + 5 + 0.5 * (30 + 57.5) = 58.75.
three <- combine_stage_outputs(list(
  stage(ids = 1, ap = 0.5, risk = 0.01, testing = 10, warranty = 5),
  stage(ids = 1, ap = 0.5, risk = 0.02, testing = 10, warranty = 5),
  stage(ids = 1, ap = 0.5, risk = 0.03, testing = 10, warranty = 5)
), id_cols = "plan")
three_cost <- calculate_recursive_costs(three, continuation_costs = c(30, 20),
                                        terminal_penalty = 100)
assert_equal(three_cost$plans$total_expected_cost, 58.75)

# Every stage risk is enforced. Plan 1 is cheaper but infeasible at stage 2.
risk_stages <- combine_stage_outputs(list(
  stage(ids = 1:2, risk = c(0.01, 0.02), testing = c(1, 10), warranty = c(0, 0)),
  stage(ids = 1:2, risk = c(0.06, 0.02), testing = c(1, 10), warranty = c(0, 0)),
  stage(ids = 1:2, risk = c(0.01, 0.02), testing = c(1, 10), warranty = c(0, 0))
), id_cols = "plan")
risk_cost <- calculate_recursive_costs(risk_stages, continuation_costs = 0,
                                       terminal_penalty = 0)
selection <- select_minimum_cost_plans(risk_cost, beta = 0.05)
stopifnot(!selection$plans$feasible[selection$plans$plan == 1])
stopifnot(selection$plans$feasible[selection$plans$plan == 2])
stopifnot(identical(selection$selected_plans$plan, 2L))

# Clear validation errors.
missing_column <- stage()
missing_column$CR_b <- NULL
assert_error(combine_stage_outputs(list(missing_column), id_cols = "plan"),
             "missing required columns: CR_b")
assert_error(combine_stage_outputs(list(stage(ids = 1:2), stage(ids = 2:3)),
                                   id_cols = "plan"),
             "inconsistent test-plan identifiers")
assert_error(select_minimum_cost_plans(risk_cost, beta = -0.1),
             "beta must be a single finite value between 0 and 1")
assert_error(select_minimum_cost_plans(risk_cost, beta = 0),
             "No feasible test plans")

# Core calculations have no file-system side effects.
side_effect_dir <- tempfile("mbrdt-core-test-")
dir.create(side_effect_dir)
old_working_dir <- setwd(side_effect_dir)
before <- list.files(side_effect_dir, all.files = TRUE, no.. = TRUE)
invisible(select_minimum_cost_plans(
  calculate_recursive_costs(
    combine_stage_outputs(list(stage()), id_cols = "plan"),
    terminal_penalty = 100
  ),
  beta = 0.05
))
after <- list.files(side_effect_dir, all.files = TRUE, no.. = TRUE)
setwd(old_working_dir)
stopifnot(identical(before, after))
