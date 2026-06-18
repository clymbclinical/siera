# Comparison script: fda-dm-t02
# Generates ARD via siera, sources it against real ADSL data, and compares
# key numeric results against the CDISC eTFL reference ARD.

library(dplyr)
library(haven)
library(jsonlite)
library(readr)

# ---- 1. Paths ---------------------------------------------------------------
repo_root  <- "c:/Users/mbosm/Projects/siera"
zip_path   <- file.path(repo_root, "tests/testthat/testdata/etfl/fda-dm-t02_20241022.zip")
siera_json <- file.path(repo_root, "tests/testthat/testdata/etfl-siera/fda-dm-t02-siera.json")

# ---- 2. Extract ZIP and convert XPT to CSV ----------------------------------
tmp      <- tempfile("dm_t02_")
dir.create(tmp)
adam_dir <- file.path(tmp, "adam")
dir.create(adam_dir)

unzip(zip_path, files = "adsl.xpt", exdir = tmp)
adsl <- haven::read_xpt(file.path(tmp, "adsl.xpt"))
readr::write_csv(adsl, file.path(adam_dir, "ADSL.csv"))
message("ADSL rows: ", nrow(adsl))

# ---- 3. Generate siera script -----------------------------------------------
devtools::load_all(repo_root, quiet = TRUE)
script_dir <- file.path(tmp, "scripts")
dir.create(script_dir)

suppressWarnings(readARS(
  ARS_path    = siera_json,
  output_path = script_dir,
  adam_path   = adam_dir
))

script_path <- file.path(script_dir, "ARD_Out_01.R")
message("Generated: ", script_path)

# ---- 4. Source generated script ---------------------------------------------
env <- new.env(parent = baseenv())
source(script_path, local = env)
siera_ard <- env$ARD
message("siera ARD rows: ", nrow(siera_ard))
message("siera analysis IDs: ", paste(sort(unique(siera_ard$AnalysisId)), collapse=", "))

# ---- 5. Load reference ARD --------------------------------------------------
unzip(zip_path, files = "fda-dm-t02-ard.json", exdir = tmp)
ref_raw <- jsonlite::fromJSON(
  file.path(tmp, "fda-dm-t02-ard.json"),
  simplifyDataFrame = FALSE, simplifyVector = FALSE
)
ref_cols <- vapply(ref_raw$columns, `[[`, character(1), "name")
ref_ard  <- as.data.frame(
  do.call(rbind, lapply(ref_raw$rows, function(r) {
    as.data.frame(setNames(
      lapply(r, function(v) if (is.null(v)) NA else v),
      ref_cols
    ), stringsAsFactors = FALSE)
  })),
  stringsAsFactors = FALSE
)
ref_ard$res <- as.numeric(ref_ard$res)
message("Reference ARD rows: ", nrow(ref_ard))

# ---- 6. An_01 bigN ----------------------------------------------------------
message("\n=== An_01 bigN (per treatment arm) ===")
siera_01 <- siera_ard |>
  filter(AnalysisId == "An_01") |>
  select(group1_level, stat, operationid) |>
  arrange(group1_level)
ref_01 <- ref_ard |>
  filter(analysisId == "An_01", nzchar(groupId1)) |>
  select(groupId1, Group1, res) |>
  arrange(groupId1)
message("siera:"); print(as.data.frame(siera_01))
message("reference:"); print(ref_01)

# ---- 7. An_02 n values (sex by treatment arm) --------------------------------
message("\n=== An_02 sex n values (arm 1) ===")
siera_02_n <- siera_ard |>
  filter(AnalysisId == "An_02", stat_name == "n", group1_level == 1) |>
  mutate(sex = as.character(variable_level), n = as.numeric(stat)) |>
  select(sex, n) |>
  arrange(sex)
ref_02_n <- ref_ard |>
  filter(analysisId == "An_02", operationId == "Mth_03_01_n",
         groupId1 == "AnlsGrouping_01_Trt01An_01") |>
  select(Group2, res) |>
  arrange(Group2)
message("siera An_02 n arm1:"); print(as.data.frame(siera_02_n))
message("reference An_02 n arm1:"); print(ref_02_n)

# ---- 8. An_02 % match (all arms × all sex categories) -----------------------
message("\n=== An_02 % match (all arms) ===")
siera_02_p <- siera_ard |>
  filter(AnalysisId == "An_02", stat_name == "p") |>
  mutate(
    trt  = as.integer(group1_level),
    sex  = as.character(variable_level),
    pct  = round(as.numeric(stat) * 100, 4)
  ) |>
  select(trt, sex, pct)

ref_02_p <- ref_ard |>
  filter(analysisId == "An_02", operationId == "Mth_03_02_%") |>
  mutate(
    trt = dplyr::case_match(groupId1,
      "AnlsGrouping_01_Trt01An_01" ~ 1L,
      "AnlsGrouping_01_Trt01An_02" ~ 2L,
      "AnlsGrouping_01_Trt01An_03" ~ 3L
    ),
    sex = Group2,
    pct = round(res, 4)
  ) |>
  select(trt, sex, pct)

comp <- inner_join(siera_02_p, ref_02_p, by = c("trt", "sex"), suffix = c("_siera", "_ref"))
comp$match <- abs(comp$pct_siera - comp$pct_ref) < 0.01
message("An_02 % rows matched: ", sum(comp$match), " / ", nrow(comp))
if (any(!comp$match)) { message("Mismatches:"); print(comp[!comp$match, ]) }

# ---- 9. An_03 continuous summary (n per arm) --------------------------------
message("\n=== An_03 age n values per treatment arm ===")
siera_03_n <- siera_ard |>
  filter(AnalysisId == "An_03", stat_name == "N") |>
  mutate(trt = as.integer(group1_level), n = as.numeric(stat)) |>
  select(trt, n) |>
  arrange(trt)
ref_03_n <- ref_ard |>
  filter(analysisId == "An_03", operationId == "Mth_06_01_n") |>
  mutate(trt = dplyr::case_match(groupId1,
    "AnlsGrouping_01_Trt01An_01" ~ 1L,
    "AnlsGrouping_01_Trt01An_02" ~ 2L,
    "AnlsGrouping_01_Trt01An_03" ~ 3L)) |>
  select(trt, res) |> arrange(trt)
message("siera:"); print(as.data.frame(siera_03_n))
message("reference:"); print(ref_03_n)

# ---- 10. An_03 mean match ---------------------------------------------------
message("\n=== An_03 age mean per treatment arm ===")
siera_03_mean <- siera_ard |>
  filter(AnalysisId == "An_03", stat_name == "mean") |>
  mutate(trt = as.integer(group1_level), mean_age = round(as.numeric(stat), 4)) |>
  select(trt, mean_age) |> arrange(trt)
ref_03_mean <- ref_ard |>
  filter(analysisId == "An_03", operationId == "Mth_06_02_Mean") |>
  mutate(trt = dplyr::case_match(groupId1,
    "AnlsGrouping_01_Trt01An_01" ~ 1L,
    "AnlsGrouping_01_Trt01An_02" ~ 2L,
    "AnlsGrouping_01_Trt01An_03" ~ 3L),
    ref_mean = round(res, 4)) |>
  select(trt, ref_mean) |> arrange(trt)
comp_mean <- inner_join(siera_03_mean, ref_03_mean, by = "trt")
comp_mean$match <- abs(comp_mean$mean_age - comp_mean$ref_mean) < 0.001
message("An_03 mean match: ", sum(comp_mean$match), " / ", nrow(comp_mean))
print(comp_mean)

# ---- 11. An_03_Total age summary (no grouping) ------------------------------
message("\n=== An_03_Total (overall age summary) ===")
siera_tot <- siera_ard |>
  filter(AnalysisId == "An_03_Total") |>
  select(stat_name, stat, operationid)
ref_tot <- ref_ard |>
  filter(analysisId == "An_03_Total") |>
  select(operationId, res)
message("siera An_03_Total:"); print(as.data.frame(siera_tot))
message("reference An_03_Total:"); print(ref_tot)

message("\nDone.")
