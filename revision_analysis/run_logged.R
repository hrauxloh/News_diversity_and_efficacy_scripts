# run_logged.R ---------------------------------------------------------------
# Runs a script and saves EVERYTHING it prints (the commands, the output, messages and warnings)
# in a text file, so that the file can be shared instead of copying the console.
#
#   source("revision_analysis/run_logged.R")
#   run_logged("revision_analysis/01_prepare_data.R")
#   run_logged("revision_analysis/03_main_models.R")
#   run_logged("revision_analysis/04_wave_and_robustness.R")
#   # other folders: say where the log should go
#   run_logged("revision_analysis_time/02_compare_models.R", log_dir = "revision_analysis_time/output")
#
# Logs are written to  <log_dir>/log_<script name>.txt  (default log_dir: revision_analysis/output).
# The tables the scripts save as csv files are in the same output folder. Please check the files
# for anything you do not want to share before you send them. The scripts do not print ids or
# individual-level data, and no data set is saved.

run_logged <- function(path, log_dir = "revision_analysis/output") {
  stopifnot(file.exists(path))
  dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
  log_file <- file.path(log_dir, paste0("log_", sub("\\.R$", "", basename(path)), ".txt"))
  con <- file(log_file, open = "wt", encoding = "UTF-8")
  sink(con, split = TRUE)                       # normal output: console AND file
  on.exit({ sink(); close(con) }, add = TRUE)
  cat("Script:", path, "\nRun at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      "\nR:", R.version.string, "\n")
  for (pk in c("SYNCSA", "dplyr", "sandwich", "lmtest", "splines"))
    if (requireNamespace(pk, quietly = TRUE)) cat(pk, as.character(utils::packageVersion(pk)), "\n")
  cat("----------------------------------------------------------------\n")
  withCallingHandlers(
    source(path, echo = TRUE, max.deparse.length = Inf, local = FALSE),
    message = function(m) cat(conditionMessage(m), file = con),            # messages also go to the file
    warning = function(w) cat("WARNING:", conditionMessage(w), "\n", file = con))
  message("Log saved: ", log_file)
  invisible(log_file)
}
