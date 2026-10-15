# Example ADaM data for tests: CDISC Pilot 01 from {clinTrialData} (#205). ----
#
# siera ships no ADaM data files. Tests that source generated ARD scripts
# read the CDISC Pilot 01 ADaM bundled with {clinTrialData} and write it to a
# temp folder as upper-case CSVs (ADSL.csv, ...), the layout readARS() expects.
#
# Some example ARS files reference datasets/variables the pilot data does not
# carry yet (e.g. ADSL.COUNTRY, ADEXSUM). Those are requested upstream rather
# than derived in siera; .skip_if_pilot_lacks() skips the dependent tests until
# clinTrialData provides them, so they re-enable automatically.

.pilot_cache <- new.env(parent = emptyenv())

# Read one pilot ADaM dataset (lower-case name, e.g. "adsl"); NULL if absent.
.pilot_dataset <- function(dataset) {
  testthat::skip_if_not_installed("clinTrialData")
  testthat::skip_if_not_installed("arrow")
  dataset <- tolower(dataset)
  if (!exists(dataset, envir = .pilot_cache, inherits = FALSE)) {
    # {connector} narrates every connection/read; keep test output clean
    quiet <- function(expr) {
      utils::capture.output(res <- suppressMessages(expr))
      res
    }
    db <- quiet(clinTrialData::connect_clinical_data("cdisc_pilot"))
    file <- paste0(dataset, ".parquet")
    assign(dataset,
           if (file %in% quiet(db$adam$list_content_cnt())) {
             as.data.frame(quiet(db$adam$read_cnt(file)))
           },
           envir = .pilot_cache)
  }
  get(dataset, envir = .pilot_cache, inherits = FALSE)
}

# Write pilot datasets as upper-case CSVs to a temp dir cleaned up with `env`.
.pilot_adam_dir <- function(datasets = c("adsl", "adae", "advs"),
                            env = parent.frame()) {
  testthat::skip_if_not_installed("readr")
  dir <- withr::local_tempdir(.local_envir = env)
  for (ds in datasets) {
    df <- .pilot_dataset(ds)
    if (is.null(df)) {
      testthat::skip(paste0("clinTrialData cdisc_pilot has no ", toupper(ds)))
    }
    readr::write_csv(df, file.path(dir, paste0(toupper(ds), ".csv")), na = "")
  }
  dir
}

# Skip unless the pilot data carries every dataset and `dataset.variable`
# needed (e.g. c("ADSL.COUNTRY", "ADEXSUM")). Gaps are tracked in siera #205.
.skip_if_pilot_lacks <- function(needs) {
  missing <- character(0)
  for (need in needs) {
    parts <- strsplit(need, ".", fixed = TRUE)[[1]]
    df <- .pilot_dataset(parts[1])
    if (is.null(df) || (length(parts) > 1 && !parts[2] %in% names(df))) {
      missing <- c(missing, need)
    }
  }
  if (length(missing)) {
    testthat::skip(paste0(
      "clinTrialData cdisc_pilot lacks ", paste(missing, collapse = ", "),
      " (requested upstream, see siera #205)"
    ))
  }
  invisible(TRUE)
}
