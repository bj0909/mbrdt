main <- function() {
  script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(script_argument) == 1L) {
    script_path <- sub("^--file=", "", script_argument)
    example_dir <- dirname(normalizePath(script_path, mustWork = TRUE))
  } else {
    example_dir <- file.path(getwd(), "inst", "examples")
  }

  source(file.path(example_dir, "example_parameters.R"), local = TRUE)
  library(mbrdt)

  original_working_dir <- getwd()
  on.exit(setwd(original_working_dir), add = TRUE)

  output_dir <- tempfile("mbrdt-end-to-end-")
  dir.create(output_dir, recursive = TRUE)
  setwd(output_dir)

  simulation_result <- do.call(multiStageRdt, example_parameters$simulation)
  if (!identical(simulation_result, "Done")) {
    stop("multiStageRdt() did not complete successfully")
  }

  stage_paths <- file.path(
    output_dir,
    paste0("data_", seq_len(example_parameters$simulation$maxStageNumber), ".csv")
  )
  stages <- read_stage_outputs(stage_paths)
  costs <- calculate_recursive_costs(
    stages,
    continuation_costs = example_parameters$postprocessing$continuation_costs,
    terminal_penalty = example_parameters$postprocessing$terminal_penalty
  )
  selection <- select_minimum_cost_plans(
    costs,
    beta = example_parameters$postprocessing$beta
  )
  report_paths <- write_postprocess_results(
    selection,
    output_dir = output_dir,
    prefix = "minimal_example"
  )

  cat("Example completed successfully.\n")
  cat("Output directory:", output_dir, "\n")
  cat("Stage outputs:\n", paste(stage_paths, collapse = "\n"), "\n")
  cat("Post-processing reports:\n", paste(report_paths, collapse = "\n"), "\n")
  cat("Selected minimum-cost feasible plan(s):\n")
  print(selection$selected_plans)
}

main()
