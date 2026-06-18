# Comparison script: ars-lb-t01
# Generates ARD via siera, sources it against real ADaM data, and compares
# numeric results against the CDISC eTFL reference ARD.
#
# Prerequisites:
#   - haven, cards, cardx, dplyr, jsonlite, readr installed
#   - siera package installed or loaded with devtools::load_all()
#   - ars-lb-t01_20241022.zip in tests/testthat/testdata/etfl/
#   - ars-lb-t01-siera.json in tests/testthat/testdata/etfl-siera/

library(dplyr)
library(haven)
library(jsonlite)
library(readr)

# ---- 1. Paths ---------------------------------------------------------------
repo_root <- "c:/Users/mbosm/Projects/siera"
zip_path  <- file.path(repo_root, "tests/testthat/testdata/etfl/ars-lb-t01_20241022.zip")
siera_json <- file.path(repo_root, "tests/testthat/testdata/etfl-siera/ars-lb-t01-siera.json")

# ---- 2. Extract ZIP and convert XPT to CSV ----------------------------------
tmp     <- tempfile("lb_t01_")
dir.create(tmp)
adam_dir <- file.path(tmp, "adam")
dir.create(adam_dir)

unzip(zip_path, files = c("adsl.xpt", "adlb.xpt"), exdir = tmp)

adsl <- haven::read_xpt(file.path(tmp, "adsl.xpt"))
adlb <- haven::read_xpt(file.path(tmp, "adlb.xpt"))
readr::write_csv(adsl, file.path(adam_dir, "ADSL.csv"))
readr::write_csv(adlb, file.path(adam_dir, "ADLB.csv"))
message("ADaM CSVs written to: ", adam_dir)

# ---- 3. Generate siera script -----------------------------------------------
devtools::load_all(repo_root, quiet = TRUE)
script_dir <- file.path(tmp, "scripts")
dir.create(script_dir)

readARS(
  ARS_path    = siera_json,
  output_path = script_dir,
  adam_path   = adam_dir
)

script_path <- file.path(script_dir, "ARD_Out_10.R")
message("Generated script: ", script_path)

# ---- 4. Source generated script to produce ARD ------------------------------
env <- new.env(parent = baseenv())
source(script_path, local = env)
siera_ard <- env$ARD
message("siera ARD rows: ", nrow(siera_ard))

# ---- 5. Load reference ARD --------------------------------------------------
unzip(zip_path, files = "ars-lb-t01-ard.json", exdir = tmp)
ref_raw <- jsonlite::fromJSON(
  file.path(tmp, "ars-lb-t01-ard.json"),
  simplifyDataFrame = FALSE,
  simplifyVector    = FALSE
)
ref_cols <- vapply(ref_raw$columns, `[[`, character(1), "name")
ref_ard  <- as.data.frame(
  do.call(rbind, lapply(ref_raw$rows, function(r) {
    row_vals <- lapply(r, function(v) if (is.null(v)) NA else v)
    as.data.frame(setNames(row_vals, ref_cols), stringsAsFactors = FALSE)
  })),
  stringsAsFactors = FALSE
)
ref_ard$res <- as.numeric(ref_ard$res)
message("Reference ARD rows: ", nrow(ref_ard))

# ---- 6. Compare An_82 (bigN) ------------------------------------------------
message("\n=== An_82 (bigN per treatment) ===")
siera_82 <- siera_ard |>
  dplyr::filter(AnalysisId == "An_82") |>
  dplyr::select(group1_level, stat, operationid) |>
  dplyr::arrange(group1_level)

ref_82 <- ref_ard |>
  dplyr::filter(analysisId == "An_82") |>
  dplyr::select(Group1, res, operationId) |>
  dplyr::arrange(res)

message("siera An_82:")
print(as.data.frame(siera_82))
message("reference An_82:")
print(ref_82)

# ---- 7. Compare An_85 n values (baseline, by TRT01AN + PARAM) ---------------
message("\n=== An_85 n values (first 10 parameters, TRT arm 1) ===")
siera_85_n <- siera_ard |>
  dplyr::filter(AnalysisId == "An_85", stat_name == "N") |>
  dplyr::select(group1_level, group2_level, stat) |>
  dplyr::filter(group1_level == 1) |>
  dplyr::arrange(group2_level) |>
  head(10)

ref_85_n <- ref_ard |>
  dplyr::filter(analysisId == "An_85", operationId == "Mth_06_01_n") |>
  dplyr::select(Group1, Group2, res) |>
  dplyr::filter(grepl("Low Dose", Group1)) |>
  dplyr::arrange(Group2) |>
  head(10)

message("siera An_85 n (arm 1, first 10 params):")
print(as.data.frame(siera_85_n))
message("reference An_85 n (arm 1, first 10 params):")
print(ref_85_n)

# ---- 8. Numeric match summary -----------------------------------------------
message("\n=== Numeric match check: An_85 all n values ===")
siera_85_wide <- siera_ard |>
  dplyr::filter(AnalysisId == "An_85", stat_name == "N") |>
  dplyr::mutate(
    trt = as.integer(group1_level),
    param = as.character(group2_level),
    siera_n = as.numeric(stat)
  ) |>
  dplyr::select(trt, param, siera_n)

ref_85_wide <- ref_ard |>
  dplyr::filter(analysisId == "An_85", operationId == "Mth_06_01_n") |>
  dplyr::mutate(
    trt = as.integer(groupId1 |>
      dplyr::case_match(
        "AnlsGrouping_47_Trt01An_01" ~ 1L,
        "AnlsGrouping_47_Trt01An_02" ~ 2L,
        "AnlsGrouping_47_Trt01An_03" ~ 3L
      )),
    param = as.character(Group2),
    ref_n = res
  ) |>
  dplyr::select(trt, param, ref_n)

comparison <- dplyr::inner_join(siera_85_wide, ref_85_wide, by = c("trt", "param"))
comparison$match <- comparison$siera_n == comparison$ref_n

message("An_85 n values matched: ", sum(comparison$match), " / ", nrow(comparison))
if (any(!comparison$match)) {
  message("Mismatches:")
  print(comparison[!comparison$match, ])
} else {
  message("All n values match!")
}

message("\nDone. Script completed successfully.")
