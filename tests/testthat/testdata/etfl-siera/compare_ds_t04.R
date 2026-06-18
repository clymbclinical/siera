# Comparison script: fda-ds-t04 (disposition table)
# Generates ARD via siera and compares key results against the CDISC eTFL reference ARD.

library(dplyr)
library(haven)
library(jsonlite)
library(readr)

# ---- 1. Paths ---------------------------------------------------------------
repo_root  <- "c:/Users/mbosm/Projects/siera"
zip_path   <- file.path(repo_root, "tests/testthat/testdata/etfl/fda-ds-t04_20241022.zip")
siera_json <- file.path(
  repo_root, "tests/testthat/testdata/etfl-siera/fda-ds-t04-siera.json"
)

# ---- 2. Extract ZIP and convert XPT to CSV ----------------------------------
tmp      <- tempfile("ds_t04_")
dir.create(tmp)
adam_dir <- file.path(tmp, "adam")
dir.create(adam_dir)

unzip(zip_path, files = "adsl.xpt", exdir = tmp)
adsl <- haven::read_xpt(file.path(tmp, "adsl.xpt"))

# ADSL does not have DISCONFL; derive from DCTREAS NE ''
adsl <- adsl |>
  dplyr::mutate(
    DISCONFL = dplyr::if_else(
      !is.na(DCTREAS) & nchar(trimws(DCTREAS)) > 0, "Y", ""
    )
  )

readr::write_csv(adsl, file.path(adam_dir, "ADSL.csv"))
message("ADSL rows: ", nrow(adsl), " (with derived DISCONFL)")

# ---- 3. Generate siera script -----------------------------------------------
devtools::load_all(repo_root, quiet = TRUE)
script_dir <- file.path(tmp, "scripts")
dir.create(script_dir)

suppressWarnings(readARS(
  ARS_path    = siera_json,
  output_path = script_dir,
  adam_path   = adam_dir
))

script_path <- file.path(script_dir, "ARD_Out_02.R")
message("Generated: ", script_path)

# ---- 4. Source generated script ---------------------------------------------
env <- new.env(parent = baseenv())
source(script_path, local = env)
siera_ard <- env$ARD
message("siera ARD rows: ", nrow(siera_ard))

# ---- 5. Load reference ARD --------------------------------------------------
unzip(zip_path, files = "fda-ds-t04-ard.json", exdir = tmp)
ref_raw <- jsonlite::fromJSON(
  file.path(tmp, "fda-ds-t04-ard.json"),
  simplifyDataFrame = FALSE, simplifyVector = FALSE
)
ref_cols <- vapply(ref_raw$columns, `[[`, character(1), "name")
ref_ard <- as.data.frame(
  do.call(rbind, lapply(ref_raw$rows, function(r) {
    as.data.frame(
      setNames(lapply(r, function(v) if (is.null(v)) NA else v), ref_cols),
      stringsAsFactors = FALSE
    )
  })),
  stringsAsFactors = FALSE
)
ref_ard$res <- as.numeric(ref_ard$res)
message("Reference ARD rows: ", nrow(ref_ard))

# ---- 6. An_12 bigN ----------------------------------------------------------
message("\n=== An_12 bigN (sorted values) ===")
siera_12 <- sort(as.numeric(siera_ard$stat[siera_ard$AnalysisId == "An_12"]))
ref_12   <- sort(ref_ard$res[ref_ard$analysisId == "An_12"])
message("siera: ", paste(siera_12, collapse = ", "))
message("ref:   ", paste(ref_12,   collapse = ", "))
match_12 <- all(abs(siera_12 - ref_12) < 0.01)
message("An_12 match: ", match_12)

# ---- 7. An_21 n and % -------------------------------------------------------
message("\n=== An_21 n and % ===")
siera_21 <- siera_ard |>
  filter(AnalysisId == "An_21") |>
  mutate(gid = group1_groupId, val = as.numeric(stat)) |>
  select(gid, stat_name, val) |>
  arrange(gid, stat_name)
ref_21 <- ref_ard |>
  filter(analysisId == "An_21") |>
  mutate(stat_name = dplyr::case_when(
    grepl("_n$", operationId) ~ "n",
    grepl("_%$", operationId) ~ "p",
    TRUE ~ operationId
  )) |>
  select(gid = groupId1, stat_name, ref_val = res) |>
  arrange(gid, stat_name)
comp_21 <- inner_join(siera_21, ref_21, by = c("gid", "stat_name")) |>
  mutate(
    siera_disp = ifelse(stat_name == "p", round(val * 100, 4), val),
    match = abs(siera_disp - ref_val) < 0.01
  )
message("An_21 n/% matched: ", sum(comp_21$match), " / ", nrow(comp_21))
print(as.data.frame(comp_21))

# ---- 8. An_22 RD ------------------------------------------------------------
message("\n=== An_22 Risk Difference ===")
op_map <- c(
  "estimate"  = "Mth_03_1_01_Risk_Difference_%",
  "conf.low"  = "Mth_03_1_02_95%_CI_Low",
  "conf.high" = "Mth_03_1_03_95%_CI_High"
)
siera_22 <- siera_ard |>
  filter(AnalysisId == "An_22") |>
  mutate(val = as.numeric(stat)) |>
  select(stat_name, val)
ref_22 <- ref_ard |>
  filter(analysisId == "An_22") |>
  select(operationId, res)
comp_22 <- data.frame(
  stat_name = names(op_map),
  ref       = ref_22$res[match(op_map, ref_22$operationId)],
  siera     = vapply(names(op_map), function(s) {
    v <- siera_22$val[siera_22$stat_name == s]
    if (length(v) == 0) NA_real_ else v[[1]]
  }, numeric(1))
)
comp_22$match <- abs(comp_22$siera - comp_22$ref) < 0.05
message("An_22 match (tol=0.05): ", all(comp_22$match, na.rm = TRUE))
print(comp_22)

# ---- 9. An_22_1 RD ----------------------------------------------------------
message("\n=== An_22_1 Risk Difference ===")
siera_221 <- siera_ard |>
  filter(AnalysisId == "An_22_1") |>
  mutate(val = as.numeric(stat)) |>
  select(stat_name, val)
ref_221 <- ref_ard |>
  filter(analysisId == "An_22_1") |>
  select(operationId, res)
comp_221 <- data.frame(
  stat_name = names(op_map),
  ref       = ref_221$res[match(op_map, ref_221$operationId)],
  siera     = vapply(names(op_map), function(s) {
    v <- siera_221$val[siera_221$stat_name == s]
    if (length(v) == 0) NA_real_ else v[[1]]
  }, numeric(1))
)
comp_221$match <- abs(comp_221$siera - comp_221$ref) < 0.05
message("An_22_1 match (tol=0.05): ", all(comp_221$match, na.rm = TRUE))
print(comp_221)

# ---- 10. An_21_1 n by arm x reason ------------------------------------------
message("\n=== An_21_1 n by arm x DCTREAS (first 12) ===")
siera_211 <- siera_ard |>
  filter(AnalysisId == "An_21_1", stat_name == "n") |>
  mutate(arm = group1_groupId, reason = as.character(group2_level)) |>
  select(arm, reason, siera_n = stat) |>
  filter(!is.na(arm)) |>
  arrange(arm, reason) |>
  head(12)
ref_211 <- ref_ard |>
  filter(analysisId == "An_21_1", operationId == "Mth_03_01_n") |>
  select(groupId1, Group2, res) |>
  arrange(groupId1, Group2) |>
  head(12)
message("siera An_21_1 n (first 12):"); print(as.data.frame(siera_211))
message("reference An_21_1 n (first 12):"); print(ref_211)

message("\nDone.")
