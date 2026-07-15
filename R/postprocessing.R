#' Combine Stage-Level MBRDT Outputs
#'
#' Validate and align an ordered list of stage-level simulation outputs. Each
#' stage must contain the same uniquely identified test plans. Later stages are
#' reordered to match the first stage when necessary.
#'
#' @param stage_outputs A non-empty list of data frames ordered from the first
#'   to the final stage.
#' @param id_cols Character vector naming columns that uniquely identify a test
#'   plan. Defaults to `c("n", "c", "R")`.
#' @param acceptance_col Column containing acceptance probabilities.
#' @param risk_col Column containing consumer risks.
#' @param testing_cost_col Column containing stage testing costs.
#' @param warranty_cost_col Column containing expected warranty-cost
#'   contributions. These values must already include the probability of
#'   acceptance at the stage.
#'
#' @return An object of class `mbrdt_stage_outputs` containing aligned,
#'   standardized stage data and identifier metadata.
#' @export
combine_stage_outputs <- function(stage_outputs,
                                  id_cols = c("n", "c", "R"),
                                  acceptance_col = "AP",
                                  risk_col = "CR_b",
                                  testing_cost_col = "testing_cost",
                                  warranty_cost_col = "expW") {
  if (!is.list(stage_outputs) || length(stage_outputs) == 0L ||
      !all(vapply(stage_outputs, is.data.frame, logical(1)))) {
    stop("stage_outputs must be a non-empty list of data frames", call. = FALSE)
  }
  if (!is.character(id_cols) || length(id_cols) == 0L || anyNA(id_cols) ||
      anyDuplicated(id_cols)) {
    stop("id_cols must contain one or more unique column names", call. = FALSE)
  }

  value_cols <- c(acceptance_col, risk_col, testing_cost_col, warranty_cost_col)
  if (!all(vapply(value_cols, function(x) is.character(x) && length(x) == 1L && nzchar(x),
                  logical(1)))) {
    stop("Stage value column names must be non-empty character strings", call. = FALSE)
  }
  required_cols <- c(id_cols, value_cols)

  make_key <- function(data) {
    do.call(paste, c(lapply(data[id_cols], as.character), sep = "\034"))
  }

  standardized <- vector("list", length(stage_outputs))
  reference_key <- NULL

  for (stage in seq_along(stage_outputs)) {
    data <- stage_outputs[[stage]]
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0L) {
      stop(sprintf("Stage %d is missing required columns: %s", stage,
                   paste(missing_cols, collapse = ", ")), call. = FALSE)
    }
    if (nrow(data) == 0L) {
      stop(sprintf("Stage %d contains no test plans", stage), call. = FALSE)
    }
    if (anyNA(data[id_cols])) {
      stop(sprintf("Stage %d contains missing test-plan identifiers", stage),
           call. = FALSE)
    }

    key <- make_key(data)
    if (anyDuplicated(key)) {
      stop(sprintf("Stage %d contains duplicate test-plan identifiers", stage),
           call. = FALSE)
    }

    if (is.null(reference_key)) {
      reference_key <- key
    } else {
      if (length(key) != length(reference_key) || !setequal(key, reference_key)) {
        stop(sprintf("Stage %d has inconsistent test-plan identifiers", stage),
             call. = FALSE)
      }
      data <- data[match(reference_key, key), , drop = FALSE]
    }

    values <- data[value_cols]
    if (!all(vapply(values, is.numeric, logical(1)))) {
      stop(sprintf("Stage %d probability and cost columns must be numeric", stage),
           call. = FALSE)
    }
    if (any(!is.finite(as.matrix(values)))) {
      stop(sprintf("Stage %d probability and cost columns must be finite", stage),
           call. = FALSE)
    }
    if (any(data[[acceptance_col]] < 0 | data[[acceptance_col]] > 1) ||
        any(data[[risk_col]] < 0 | data[[risk_col]] > 1)) {
      stop(sprintf("Stage %d probabilities and consumer risks must be between 0 and 1", stage),
           call. = FALSE)
    }
    if (any(data[[testing_cost_col]] < 0 | data[[warranty_cost_col]] < 0)) {
      stop(sprintf("Stage %d costs must be non-negative", stage), call. = FALSE)
    }

    standardized[[stage]] <- data.frame(
      data[id_cols],
      acceptance_probability = data[[acceptance_col]],
      consumer_risk = data[[risk_col]],
      testing_cost = data[[testing_cost_col]],
      expected_warranty_cost = data[[warranty_cost_col]],
      check.names = FALSE
    )
  }

  names(standardized) <- paste0("stage_", seq_along(standardized))
  structure(
    list(stages = standardized, id_cols = id_cols, stage_count = length(standardized)),
    class = "mbrdt_stage_outputs"
  )
}

#' Calculate Recursive Expected Costs Across MBRDT Stages
#'
#' Starting at the final stage, calculate the expected cost of each test plan.
#' Expected warranty cost is treated as an already probability-weighted stage
#' contribution. At a nonterminal stage, rejection incurs the reliability-growth
#' continuation cost and the expected cost of the next stage. At the final stage,
#' rejection incurs the terminal penalty. Plan identifiers remain fixed across
#' stages: recursion follows the same aligned plan after rejection and does not
#' dynamically switch to a different plan.
#'
#' @param stages An object returned by [combine_stage_outputs()].
#' @param continuation_costs Non-negative reliability-growth costs following
#'   rejection at nonterminal stages. Supply one value for all transitions or one
#'   value for each of the `K - 1` transitions.
#' @param terminal_penalty A non-negative scalar terminal rejection penalty, or a
#'   vector with one value per test plan.
#'
#' @return An object of class `mbrdt_cost_result` with `plans` and
#'   `stage_costs` data frames.
#' @export
calculate_recursive_costs <- function(stages, continuation_costs = 0,
                                      terminal_penalty = 0) {
  if (!inherits(stages, "mbrdt_stage_outputs")) {
    stop("stages must be created by combine_stage_outputs()", call. = FALSE)
  }

  stage_count <- stages$stage_count
  plan_count <- nrow(stages$stages[[1]])

  if (!is.numeric(continuation_costs) || anyNA(continuation_costs) ||
      any(!is.finite(continuation_costs)) || any(continuation_costs < 0)) {
    stop("continuation_costs must contain finite non-negative values", call. = FALSE)
  }
  needed_transitions <- max(stage_count - 1L, 0L)
  if (needed_transitions == 0L) {
    if (length(continuation_costs) > 1L) {
      stop("A one-stage analysis accepts at most one continuation cost", call. = FALSE)
    }
    continuation_costs <- numeric(0)
  } else if (length(continuation_costs) == 1L) {
    continuation_costs <- rep(continuation_costs, needed_transitions)
  } else if (length(continuation_costs) != needed_transitions) {
    stop(sprintf("continuation_costs must have length 1 or %d", needed_transitions),
         call. = FALSE)
  }

  if (!is.numeric(terminal_penalty) || anyNA(terminal_penalty) ||
      any(!is.finite(terminal_penalty)) || any(terminal_penalty < 0) ||
      !length(terminal_penalty) %in% c(1L, plan_count)) {
    stop("terminal_penalty must be a finite non-negative scalar or one value per plan",
         call. = FALSE)
  }
  terminal_penalty <- rep(terminal_penalty, length.out = plan_count)

  recursive_cost <- matrix(NA_real_, nrow = plan_count, ncol = stage_count)
  stage_results <- vector("list", stage_count)

  for (stage in rev(seq_len(stage_count))) {
    data <- stages$stages[[stage]]
    rejection_probability <- 1 - data$acceptance_probability
    growth_component <- numeric(plan_count)
    downstream_component <- numeric(plan_count)
    penalty_component <- numeric(plan_count)

    if (stage == stage_count) {
      penalty_component <- rejection_probability * terminal_penalty
    } else {
      growth_component <- rejection_probability * continuation_costs[[stage]]
      downstream_component <- rejection_probability * recursive_cost[, stage + 1L]
    }

    recursive_cost[, stage] <- data$testing_cost + data$expected_warranty_cost +
      growth_component + downstream_component + penalty_component

    stage_results[[stage]] <- data.frame(
      data[stages$id_cols],
      stage = stage,
      acceptance_probability = data$acceptance_probability,
      consumer_risk = data$consumer_risk,
      testing_cost = data$testing_cost,
      expected_warranty_cost = data$expected_warranty_cost,
      reliability_growth_cost = growth_component,
      downstream_expected_cost = downstream_component,
      terminal_penalty_cost = penalty_component,
      recursive_expected_cost = recursive_cost[, stage],
      check.names = FALSE
    )
  }

  plans <- data.frame(
    stages$stages[[1]][stages$id_cols],
    total_expected_cost = recursive_cost[, 1L],
    check.names = FALSE
  )

  structure(
    list(
      plans = plans,
      stage_costs = do.call(rbind, stage_results),
      id_cols = stages$id_cols,
      stage_count = stage_count,
      continuation_costs = continuation_costs,
      terminal_penalty = terminal_penalty
    ),
    class = "mbrdt_cost_result"
  )
}

#' Select Minimum-Cost Feasible MBRDT Plans
#'
#' A plan is feasible only when its consumer risk is no greater than `beta` at
#' every stage. Among feasible plans, select all plans tied for minimum stage-1
#' recursive expected cost, optionally within groups.
#'
#' @param cost_result An object returned by [calculate_recursive_costs()].
#' @param beta Consumer-risk threshold in `[0, 1]`.
#' @param group_cols Optional identifier columns within which minimum-cost plans
#'   are selected. By default, one global minimum is selected.
#'
#' @return An object of class `mbrdt_selection_result` containing all plans with
#'   feasibility status, selected plans, and stage-specific costs.
#' @export
select_minimum_cost_plans <- function(cost_result, beta, group_cols = NULL) {
  if (!inherits(cost_result, "mbrdt_cost_result")) {
    stop("cost_result must be created by calculate_recursive_costs()", call. = FALSE)
  }
  if (!is.numeric(beta) || length(beta) != 1L || is.na(beta) ||
      !is.finite(beta) || beta < 0 || beta > 1) {
    stop("beta must be a single finite value between 0 and 1", call. = FALSE)
  }
  if (!is.null(group_cols) &&
      (!is.character(group_cols) || any(!group_cols %in% cost_result$id_cols))) {
    stop("group_cols must be NULL or identifier columns in the cost result",
         call. = FALSE)
  }

  risk_by_plan <- split(cost_result$stage_costs$consumer_risk,
                        rep(seq_len(nrow(cost_result$plans)),
                            times = cost_result$stage_count))
  feasible <- vapply(risk_by_plan, function(x) all(x <= beta), logical(1))
  all_plans <- cost_result$plans
  all_plans$feasible <- feasible
  feasible_plans <- all_plans[all_plans$feasible, , drop = FALSE]

  if (nrow(feasible_plans) == 0L) {
    stop("No feasible test plans satisfy the consumer-risk threshold at every stage",
         call. = FALSE)
  }

  if (is.null(group_cols) || length(group_cols) == 0L) {
    selected <- feasible_plans[
      feasible_plans$total_expected_cost == min(feasible_plans$total_expected_cost),
      , drop = FALSE
    ]
  } else {
    group_key <- do.call(paste, c(lapply(feasible_plans[group_cols], as.character),
                                  sep = "\034"))
    group_min <- stats::ave(feasible_plans$total_expected_cost, group_key, FUN = min)
    selected <- feasible_plans[
      feasible_plans$total_expected_cost == group_min, , drop = FALSE
    ]
  }

  stage_costs <- cost_result$stage_costs
  stage_costs$feasible <- rep(feasible, times = cost_result$stage_count)

  structure(
    list(
      plans = all_plans,
      selected_plans = selected,
      stage_costs = stage_costs,
      beta = beta
    ),
    class = "mbrdt_selection_result"
  )
}

#' Read Stage-Level MBRDT Output Files
#'
#' @param paths Character vector of CSV paths ordered by stage.
#' @param ... Additional arguments passed to [combine_stage_outputs()].
#'
#' @return An object of class `mbrdt_stage_outputs`.
#' @export
read_stage_outputs <- function(paths, ...) {
  if (!is.character(paths) || length(paths) == 0L || anyNA(paths)) {
    stop("paths must be a non-empty character vector", call. = FALSE)
  }
  missing_paths <- paths[!file.exists(paths)]
  if (length(missing_paths) > 0L) {
    stop(sprintf("Stage output files do not exist: %s",
                 paste(missing_paths, collapse = ", ")), call. = FALSE)
  }
  combine_stage_outputs(lapply(paths, utils::read.csv), ...)
}

#' Write MBRDT Post-Processing Results
#'
#' Write plan, stage-cost, and selected-plan CSV files. Existing files are not
#' replaced unless `overwrite = TRUE`.
#'
#' @param result An object returned by [select_minimum_cost_plans()].
#' @param output_dir Existing output directory.
#' @param prefix Non-empty filename prefix.
#' @param overwrite Whether existing files may be overwritten.
#'
#' @return Invisibly, the paths written.
#' @export
write_postprocess_results <- function(result, output_dir, prefix = "mbrdt",
                                      overwrite = FALSE) {
  if (!inherits(result, "mbrdt_selection_result")) {
    stop("result must be created by select_minimum_cost_plans()", call. = FALSE)
  }
  if (!is.character(output_dir) || length(output_dir) != 1L ||
      !dir.exists(output_dir)) {
    stop("output_dir must be an existing directory", call. = FALSE)
  }
  if (!is.character(prefix) || length(prefix) != 1L || !nzchar(prefix) ||
      grepl("[\\/]", prefix)) {
    stop("prefix must be a non-empty filename prefix without path separators",
         call. = FALSE)
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    stop("overwrite must be TRUE or FALSE", call. = FALSE)
  }

  paths <- file.path(
    output_dir,
    paste0(prefix, c("_plans.csv", "_stage_costs.csv", "_selected_plans.csv"))
  )
  if (!overwrite && any(file.exists(paths))) {
    stop(sprintf("Refusing to overwrite existing files: %s",
                 paste(paths[file.exists(paths)], collapse = ", ")), call. = FALSE)
  }

  utils::write.csv(result$plans, paths[[1]], row.names = FALSE)
  utils::write.csv(result$stage_costs, paths[[2]], row.names = FALSE)
  utils::write.csv(result$selected_plans, paths[[3]], row.names = FALSE)
  invisible(paths)
}
