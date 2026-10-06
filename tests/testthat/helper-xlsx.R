# Helpers for the deprecated xlsx ARS input path (#179) -------------------------

# Evaluate `expr`, muffling ONLY siera's xlsx deprecation warning (class
# `siera_deprecated_xlsx`) so any other warning still reaches the test.
.quiet_xlsx_deprecation <- function(expr) {
  withCallingHandlers(
    expr,
    siera_deprecated_xlsx = function(w) invokeRestart("muffleWarning")
  )
}

# Generate ARD scripts from an ARS file and return each script's normalised
# lines keyed by file name: the timestamp line is dropped and carriage returns
# and blank lines are removed, so two generations can be compared exactly.
.gen_scripts <- function(ars_path, adam_path, code_style = "wrapped") {
  d <- withr::local_tempdir()
  suppressMessages(.quiet_xlsx_deprecation(
    readARS(ars_path, output_path = d, adam_path = adam_path,
            code_style = code_style)
  ))
  fs <- sort(list.files(d, pattern = "\\.R$", full.names = TRUE))
  stats::setNames(lapply(fs, function(f) {
    ls <- readLines(f, warn = FALSE)
    ls <- ls[!grepl("^# Date created:", ls)]
    ls <- gsub("\r", "", ls)
    ls[nzchar(trimws(ls))]
  }), basename(fs))
}
