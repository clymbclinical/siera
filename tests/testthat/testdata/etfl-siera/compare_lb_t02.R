# Comparison script: ars-lb-t02 (lab table — continuous summary)
# Generates ARD via siera and compares key analyses against the CDISC eTFL reference ARD.

library(dplyr)
library(haven)
library(jsonlite)
library(readr)

# ---- 1. Paths ---------------------------------------------------------------
repo_root  <- "c:/Users/mbosm/Projects/siera"
zip_path   <- file.path(repo_root, "tests/testthat/testdata/etfl/ars-lb-t02_20241022.zip")
siera_json <- file.path(
  repo_root, "tests/testthat/testdata/etfl-siera/ars-lb-t02-siera.json"
)

# ---- 2. Extract ZIP and convert XPT to CSV ----------------------------------
tmp      <- tempfile("lb_t02_")
dir.create(tmp)
adam_dir <- file.path(tmp, "adam")
dir.create(adam_dir)

for (f in c("adsl.xpt", "adlb.xpt")) {
  unzip(zip_path, files = f, exdir = tmp)
  ds <- haven::read_xpt(file.path(tmp, f))
  write_csv(ds, file.path(adam_dir, paste0(toupper(tools::file_path_sans_ext(f)), ".csv")))
}

# ---- 3. Generate siera script -----------------------------------------------
devtools::load_all(repo_root, quiet = TRUE)
script_dir <- file.path(tmp, "scripts")
dir.create(script_dir)
suppressWarnings(readARS(ARS_path = siera_json, output_path = script_dir, adam_path = adam_dir))
script_path <- file.path(script_dir, "ARD_Out_11.R")

# ---- 4. Source generated script ---------------------------------------------
env <- new.env(parent = baseenv())
source(script_path, local = env)
siera_ard <- env$ARD
message("siera ARD rows: ", nrow(siera_ard))

# ---- 5. Load reference ARD --------------------------------------------------
unzip(zip_path, files = "ars-lb-t02-ard.json", exdir = tmp)
ref_raw <- jsonlite::fromJSON(
  file.path(tmp, "ars-lb-t02-ard.json"),
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

# ---- 6. An_90 bigN ----------------------------------------------------------
message("\n=== An_90 bigN ===")
siera_90 <- sort(as.numeric(siera_ard$stat[siera_ard$AnalysisId == "An_90"]))
ref_90   <- sort(ref_ard$res[ref_ard$analysisId == "An_90"])
message("siera: ", paste(siera_90, collapse = ", "))
message("ref:   ", paste(ref_90,   collapse = ", "))
match_90 <- length(siera_90) == length(ref_90) && all(abs(siera_90 - ref_90) < 0.01)
message("An_90 match: ", match_90)

# ---- 7. An_93 N and Mean ----------------------------------------------------
message("\n=== An_93 N (arm 1, first 5 params) ===")
arm1_id <- unique(ref_ard$groupId1[ref_ard$analysisId == "An_93"])[1]
s_n <- siera_ard |>
  filter(AnalysisId == "An_93", operationid == "Mth_06_01_n",
         group1_groupId == arm1_id) |>
  mutate(param = as.character(group2_level), v = as.numeric(stat)) |>
  select(param, v) |> arrange(param) |> head(5)
r_n <- ref_ard |>
  filter(analysisId == "An_93", operationId == "Mth_06_01_n",
         groupId1 == arm1_id) |>
  select(Group2, res) |> arrange(Group2) |> head(5)
message("siera:"); print(as.data.frame(s_n))
message("ref:");   print(r_n)

message("\n=== An_93 Mean (arm 1, first 5, 3dp) ===")
s_m <- siera_ard |>
  filter(AnalysisId == "An_93", operationid == "Mth_06_02_Mean",
         group1_groupId == arm1_id) |>
  mutate(param = as.character(group2_level), v = round(as.numeric(stat), 3)) |>
  select(param, v) |> arrange(param) |> head(5)
r_m <- ref_ard |>
  filter(analysisId == "An_93", operationId == "Mth_06_02_Mean",
         groupId1 == arm1_id) |>
  mutate(v = round(res, 3)) |> select(Group2, v) |> arrange(Group2) |> head(5)
message("siera:"); print(as.data.frame(s_m))
message("ref:");   print(r_m)

# ---- 8. Spot check An_93 N match across all arms ----------------------------
message("\n=== An_93 N full match (all arms × 5 params) ===")
ref_n93 <- ref_ard |>
  filter(analysisId == "An_93", operationId == "Mth_06_01_n") |>
  select(groupId1, Group2, ref_val = res) |>
  arrange(groupId1, Group2) |>
  head(15)
siera_n93 <- siera_ard |>
  filter(AnalysisId == "An_93", operationid == "Mth_06_01_n") |>
  mutate(groupId1 = group1_groupId, Group2 = as.character(group2_level),
         siera_val = as.numeric(stat)) |>
  select(groupId1, Group2, siera_val) |>
  arrange(groupId1, Group2) |>
  head(15)
comp_n93 <- inner_join(ref_n93, siera_n93, by = c("groupId1", "Group2")) |>
  mutate(match = abs(ref_val - siera_val) < 0.01)
message("N matches (15 spot): ", sum(comp_n93$match), " / ", nrow(comp_n93))
print(as.data.frame(comp_n93))

message("\nDone.")
