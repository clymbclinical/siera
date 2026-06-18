# Shared helper for eTFL Portal regression tests.
# Extracts a named ZIP; returns ARS JSON path and an empty adam/ directory.
# readARS() embeds adam_path as a string without checking file existence,
# so no dataset conversion is needed for metadata-parsing-only tests.
.extract_etfl_zip <- function(zip_name, tmp_dir) {
  zip_path <- testthat::test_path("testdata", "etfl", zip_name)
  if (!file.exists(zip_path)) {
    testthat::skip(paste("eTFL fixture not found:", zip_name))
  }

  utils::unzip(zip_path, exdir = tmp_dir)

  ars_json <- list.files(tmp_dir, pattern = "-ars\\.json$",
                        full.names = TRUE, recursive = FALSE)[1]

  adam_dir <- file.path(tmp_dir, "adam")
  dir.create(adam_dir)

  list(ars_json = ars_json, adam_dir = adam_dir)
}

# Full-pipeline helper: converts XPT → CSV, runs readARS with a -siera.json
# enrichment (which carries codeTemplate entries), sources the generated script,
# and returns the ARD data frame.
.run_etfl_siera_pipeline <- function(zip_name, siera_json_name, tmp_dir) {
  if (!requireNamespace("haven",  quietly = TRUE)) testthat::skip("haven not installed")
  if (!requireNamespace("readr",  quietly = TRUE)) testthat::skip("readr not installed")

  zip_path   <- testthat::test_path("testdata", "etfl",       zip_name)
  siera_json <- testthat::test_path("testdata", "etfl-siera", siera_json_name)

  if (!file.exists(zip_path))   testthat::skip(paste("eTFL ZIP not found:",     zip_name))
  if (!file.exists(siera_json)) testthat::skip(paste("siera JSON not found:", siera_json_name))

  adam_dir <- file.path(tmp_dir, "adam")
  dir.create(adam_dir)

  flist <- utils::unzip(zip_path, list = TRUE)$Name
  xpt_files <- flist[grepl("\\.xpt$", flist, ignore.case = TRUE)]
  for (f in xpt_files) {
    utils::unzip(zip_path, files = f, exdir = tmp_dir)
    ds <- haven::read_xpt(file.path(tmp_dir, f))
    readr::write_csv(
      ds,
      file.path(adam_dir, paste0(toupper(tools::file_path_sans_ext(basename(f))), ".csv"))
    )
  }

  script_dir <- file.path(tmp_dir, "scripts")
  dir.create(script_dir)
  suppressWarnings(readARS(ARS_path = siera_json, output_path = script_dir, adam_path = adam_dir))

  scripts <- list.files(script_dir, pattern = "^ARD_.*\\.R$", full.names = TRUE)
  if (length(scripts) == 0L) stop("No ARD script generated")

  env <- new.env(parent = baseenv())
  source(scripts[[1L]], local = env)
  env$ARD
}

# Extract a single numeric stat from the ARD for a given analysis + operationid.
.ard_stat <- function(ard, analysis_id, opid) {
  rows <- ard[ard$AnalysisId == analysis_id & ard$operationid == opid, ]
  vapply(rows$stat, function(v) if (is.null(v) || is.na(v)) NA_real_ else as.numeric(v[[1L]]), numeric(1L))
}
