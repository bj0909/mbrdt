main <- function() {
  package_root <- getwd()
  original_working_dir <- getwd()
  on.exit(setwd(original_working_dir), add = TRUE)

  output_dir <- tempfile("mbrdt-smoke-test-")
  dir.create(output_dir, recursive = TRUE)

  r_files <- sort(list.files(file.path(package_root, "R"), pattern = "[.]R$", full.names = TRUE))
  invisible(lapply(r_files, source))

  setwd(output_dir)
  result <- multiStageRdt(
    maxStageNumber = 1,
    n = 1:2,
    c = 0,
    R = 0.85,
    seed = 123,
    M = 20,
    betaPriorParam1 = 1,
    betaPriorParam2 = 1,
    sampleSizePass = 20,
    sampleSizeFail = 20,
    startValForMH = 0.1,
    sdForMH = 0.1,
    cv = 25,
    cf = 0,
    cw = 6,
    V = 1000,
    G = 100000,
    burnInNum = 5,
    upperLimit = 1,
    growthRate = 0.7
  )

  expected_files <- c("data_1.csv", "params_stage_2.csv")
  if (!identical(result, "Done")) {
    stop("multiStageRdt() did not return \"Done\"")
  }
  if (!all(file.exists(file.path(output_dir, expected_files)))) {
    stop("Smoke test did not create all expected output files")
  }

  cat("result:", result, "\n")
  cat("output_dir:", output_dir, "\n")
  cat("generated_files:\n")
  cat(paste(sort(list.files(output_dir)), collapse = "\n"), "\n")
}

main()
