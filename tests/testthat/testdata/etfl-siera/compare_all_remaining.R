# Comprehensive numeric comparison: all remaining eTFLs vs CDISC reference ARDs.
# Checks bigN values and key n/%, RD, continuous stats across 8 eTFLs.

library(dplyr)
library(haven)
library(jsonlite)
library(readr)

repo_root <- "c:/Users/mbosm/Projects/siera"
etfl_dir  <- file.path(repo_root, "tests/testthat/testdata/etfl")
siera_dir <- file.path(repo_root, "tests/testthat/testdata/etfl-siera")
devtools::load_all(repo_root, quiet = TRUE)

# ---- Helpers ----------------------------------------------------------------
load_ref <- function(tmp, zip_path) {
  flist <- unzip(zip_path, list = TRUE)$Name
  ard_f <- flist[grepl("-ard\\.json$", flist)]
  unzip(zip_path, files = ard_f, exdir = tmp)
  ref <- fromJSON(file.path(tmp, ard_f), simplifyDataFrame = FALSE, simplifyVector = FALSE)
  cols <- vapply(ref$columns, `[[`, character(1), "name")
  df <- as.data.frame(
    do.call(rbind, lapply(ref$rows, function(r)
      as.data.frame(setNames(lapply(r, function(v) if (is.null(v)) NA else v), cols),
        stringsAsFactors = FALSE))),
    stringsAsFactors = FALSE)
  df$res <- suppressWarnings(as.numeric(df$res))
  df
}

run_siera <- function(zip_path, siera_json) {
  tmp <- tempfile()
  dir.create(tmp)
  adam_dir <- file.path(tmp, "adam"); dir.create(adam_dir)
  flist <- unzip(zip_path, list = TRUE)$Name
  for (f in flist[grepl("\\.xpt$", flist, ignore.case = TRUE)]) {
    unzip(zip_path, files = f, exdir = tmp)
    ds <- haven::read_xpt(file.path(tmp, f))
    write_csv(ds, file.path(adam_dir, paste0(toupper(tools::file_path_sans_ext(f)), ".csv")))
  }
  script_dir <- file.path(tmp, "scripts"); dir.create(script_dir)
  suppressWarnings(readARS(ARS_path = siera_json, output_path = script_dir, adam_path = adam_dir))
  scr <- list.files(script_dir, pattern = "^ARD_.*\\.R$", full.names = TRUE)
  env <- new.env(parent = baseenv())
  source(scr[[1]], local = env)
  list(ard = env$ARD, tmp = tmp, zip_path = zip_path)
}

compare_bigN <- function(siera_ard, ref_ard, bigN_ana) {
  s <- sort(as.numeric(siera_ard$stat[siera_ard$AnalysisId == bigN_ana]))
  r <- sort(ref_ard$res[ref_ard$analysisId == bigN_ana])
  ok <- length(s) == length(r) && length(s) > 0 && all(abs(s - r) < 0.01)
  list(match = ok, siera = s, ref = r)
}

safe_stat <- function(x) vapply(x, function(v) {
  if (is.null(v) || (length(v) == 1 && is.na(v))) NA_real_ else as.numeric(v[[1]])
}, numeric(1))

compare_n_pct <- function(siera_ard, ref_ard, ana_id, n_opid, pct_opid) {
  s <- siera_ard |>
    filter(AnalysisId == ana_id) |>
    mutate(val = safe_stat(stat),
           siera_disp = ifelse(stat_name == "p", round(val * 100, 4), val)) |>
    select(grp = group1_groupId, stat_name, siera_disp)
  r <- ref_ard |>
    filter(analysisId == ana_id) |>
    mutate(stat_name = case_when(
      operationId == n_opid   ~ "n",
      operationId == pct_opid ~ "p",
      TRUE ~ NA_character_)) |>
    filter(!is.na(stat_name)) |>
    select(grp = groupId1, stat_name, ref = res)
  comp <- inner_join(s, r, by = c("grp", "stat_name"),
                     relationship = "many-to-many") |>
    distinct() |>
    mutate(match = abs(siera_disp - ref) < 0.01)
  list(n_matched = sum(comp$match), n_total = nrow(comp), data = comp)
}

compare_n_pct_2level <- function(siera_ard, ref_ard, ana_id, n_opid, pct_opid) {
  s <- siera_ard |>
    filter(AnalysisId == ana_id, stat_name %in% c("n", "p")) |>
    mutate(val = safe_stat(stat),
           siera_disp = ifelse(stat_name == "p", round(val * 100, 4), val),
           grp2 = as.character(group2_level)) |>
    select(grp1 = group1_groupId, grp2, stat_name, siera_disp)
  r <- ref_ard |>
    filter(analysisId == ana_id) |>
    mutate(stat_name = case_when(
      operationId == n_opid   ~ "n",
      operationId == pct_opid ~ "p",
      TRUE ~ NA_character_)) |>
    filter(!is.na(stat_name)) |>
    select(grp1 = groupId1, grp2 = Group2, stat_name, ref = res)
  comp <- inner_join(s, r, by = c("grp1", "grp2", "stat_name")) |>
    mutate(match = abs(siera_disp - ref) < 0.01)
  list(n_matched = sum(comp$match), n_total = nrow(comp), data = comp)
}

compare_continuous <- function(siera_ard, ref_ard, ana_id) {
  ops <- c("Mth_06_01_n","Mth_06_02_Mean","Mth_06_03_SD",
           "Mth_06_04_Median","Mth_06_07_Min","Mth_06_08_Max")
  s_raw <- siera_ard |> filter(AnalysisId == ana_id) |> mutate(val = safe_stat(stat))
  s <- if ("group2_level" %in% names(s_raw)) {
    s_raw |> mutate(grp2 = as.character(group2_level)) |>
      select(grp1 = group1_groupId, grp2, opid = operationid, val)
  } else {
    s_raw |> mutate(grp2 = NA_character_) |>
      select(grp1 = group1_groupId, grp2, opid = operationid, val)
  }
  r_raw <- ref_ard |> filter(analysisId == ana_id, operationId %in% ops)
  has_grp2_ref <- "Group2" %in% names(r_raw)
  r <- if (has_grp2_ref) {
    r_raw |> select(grp1 = groupId1, grp2 = Group2, opid = operationId, ref = res)
  } else {
    r_raw |> mutate(grp2 = NA_character_) |>
      select(grp1 = groupId1, grp2, opid = operationId, ref = res)
  }
  # grp2 may be NA/empty for 1-grouping analyses — join on grp1+grp2 only when both have values
  use_grp2 <- any(!is.na(r$grp2) & nchar(r$grp2) > 0, na.rm = TRUE)
  if (use_grp2) {
    r <- r |> mutate(grp2 = ifelse(is.na(grp2), "", grp2))
    s <- s |> mutate(grp2 = ifelse(is.na(grp2), "", grp2))
    comp <- inner_join(s, r, by = c("grp1", "grp2", "opid"))
  } else {
    comp <- inner_join(s, r, by = c("grp1", "opid"))
  }
  comp <- comp |> mutate(match = abs(val - ref) < 0.01)
  list(n_matched = sum(comp$match, na.rm = TRUE), n_total = nrow(comp), data = comp)
}

compare_rd <- function(siera_ard, ref_ard, ana_id) {
  op_map <- c(
    "estimate"  = "Mth_03_1_01_Risk_Difference_%",
    "conf.low"  = "Mth_03_1_02_95%_CI_Low",
    "conf.high" = "Mth_03_1_03_95%_CI_High"
  )
  s <- siera_ard |> filter(AnalysisId == ana_id) |>
    mutate(val = safe_stat(stat)) |> select(stat_name, val)
  r <- ref_ard |> filter(analysisId == ana_id) |> select(operationId, res)
  comp <- data.frame(
    stat_name = names(op_map),
    ref = r$res[match(op_map, r$operationId)],
    siera = vapply(names(op_map), function(s2) {
      v <- s$val[s$stat_name == s2]; if (length(v) == 0) NA_real_ else v[[1]]
    }, numeric(1))
  )
  comp$match <- abs(comp$siera - comp$ref) < 0.05
  list(n_matched = sum(comp$match, na.rm = TRUE), n_total = nrow(comp), data = comp)
}

# ---- ars-vs-t01 (vital signs — continuous) ----------------------------------
message("\n====== ars-vs-t01 ======")
bn <- "ars-vs-t01"
out <- run_siera(file.path(etfl_dir, paste0(bn, "_20241022.zip")),
                 file.path(siera_dir, paste0(bn, "-siera.json")))
ref <- load_ref(out$tmp, out$zip_path)
bN <- compare_bigN(out$ard, ref, "An_98")
message("An_98 bigN match: ", bN$match, " | siera:", paste(bN$siera, collapse=","),
        " ref:", paste(bN$ref, collapse=","))
# An_101: continuous summary
c101 <- compare_continuous(out$ard, ref, "An_101")
message("An_101 continuous match: ", c101$n_matched, "/", c101$n_total)

# ---- fda-ae-t06 (AE summary) ------------------------------------------------
message("\n====== fda-ae-t06 ======")
bn <- "fda-ae-t06"
out <- run_siera(file.path(etfl_dir, paste0(bn, "_20241022.zip")),
                 file.path(siera_dir, paste0(bn, "-siera.json")))
ref <- load_ref(out$tmp, out$zip_path)
bN <- compare_bigN(out$ard, ref, "An_30")
message("An_30 bigN match: ", bN$match, " | siera:", paste(bN$siera, collapse=","),
        " ref:", paste(bN$ref, collapse=","))
c31 <- compare_n_pct(out$ard, ref, "An_31", "Mth_03_01_n", "Mth_03_02_%")
message("An_31 n% match: ", c31$n_matched, "/", c31$n_total)
# An_34 = SAE deaths RD: 0 events → siera returns NA (nrow guard skips), ref returns 0
# Known limitation: when df2 is empty, siera skips computation; ref produces RD=0
s34 <- out$ard |> filter(AnalysisId=="An_34")
message("An_34 siera rows: ", nrow(s34), " (0 events → NA expected; ref has RD=0)")

# ---- fda-ae-t07 (AE subjects by cause) --------------------------------------
message("\n====== fda-ae-t07 ======")
bn <- "fda-ae-t07"
out <- run_siera(file.path(etfl_dir, paste0(bn, "_20241022.zip")),
                 file.path(siera_dir, paste0(bn, "-siera.json")))
ref <- load_ref(out$tmp, out$zip_path)
bN <- compare_bigN(out$ard, ref, "An_72")
message("An_72 bigN match: ", bN$match, " | siera:", paste(bN$siera, collapse=","),
        " ref:", paste(bN$ref, collapse=","))
c73 <- compare_n_pct(out$ard, ref, "An_73", "Mth_03_01_n", "Mth_03_02_%")
message("An_73 n% match: ", c73$n_matched, "/", c73$n_total)
c75 <- compare_n_pct(out$ard, ref, "An_75", "Mth_03_01_n", "Mth_03_02_%")
message("An_75 n% match: ", c75$n_matched, "/", c75$n_total)
# An_74 is Mth_03a (2-level: arm x cause) — opids same as Mth_03 in ref
c74 <- compare_n_pct_2level(out$ard, ref, "An_74", "Mth_03_01_n", "Mth_03_02_%")
message("An_74 n% 2-level match: ", c74$n_matched, "/", c74$n_total)

# ---- fda-ae-t09 (AE SAE/fatal table) ----------------------------------------
message("\n====== fda-ae-t09 ======")
bn <- "fda-ae-t09"
out <- run_siera(file.path(etfl_dir, paste0(bn, "_20241022.zip")),
                 file.path(siera_dir, paste0(bn, "-siera.json")))
ref <- load_ref(out$tmp, out$zip_path)
bN <- compare_bigN(out$ard, ref, "An_65")
message("An_65 bigN match: ", bN$match, " | siera:", paste(bN$siera, collapse=","),
        " ref:", paste(bN$ref, collapse=","))
c66 <- compare_n_pct(out$ard, ref, "An_66", "Mth_03_01_n", "Mth_03_02_%")
message("An_66 n% match: ", c66$n_matched, "/", c66$n_total)
# An_68 is Mth_03a (2-level: arm x cause of death)
c68 <- compare_n_pct_2level(out$ard, ref, "An_68", "Mth_03_01_n", "Mth_03_02_%")
message("An_68 n% 2-level match: ", c68$n_matched, "/", c68$n_total)
# An_67 is Mth_03_1 (risk difference)
c67 <- compare_rd(out$ard, ref, "An_67")
message("An_67 RD match: ", c67$n_matched, "/", c67$n_total, " | ",
        paste(round(c67$data$siera, 2), "vs", round(c67$data$ref, 2), collapse="; "))

# ---- fda-ae-t12 (TEAE table) ------------------------------------------------
message("\n====== fda-ae-t12 ======")
bn <- "fda-ae-t12"
out <- run_siera(file.path(etfl_dir, paste0(bn, "_20241022.zip")),
                 file.path(siera_dir, paste0(bn, "-siera.json")))
ref <- load_ref(out$tmp, out$zip_path)
bN <- compare_bigN(out$ard, ref, "An_58")
message("An_58 bigN match: ", bN$match, " | siera:", paste(bN$siera, collapse=","),
        " ref:", paste(bN$ref, collapse=","))
c59 <- compare_n_pct(out$ard, ref, "An_59", "Mth_03_01_n", "Mth_03_02_%")
message("An_59 n% match: ", c59$n_matched, "/", c59$n_total)
c60 <- compare_rd(out$ard, ref, "An_60")
message("An_60 RD match: ", c60$n_matched, "/", c60$n_total)
# An_61 is Mth_03a (2-level)
c61 <- compare_n_pct_2level(out$ard, ref, "An_61", "Mth_03_01_n", "Mth_03_02_%")
message("An_61 n% 2-level match: ", c61$n_matched, "/", c61$n_total)

# ---- fda-ae-t13 (AE + PT summary) -------------------------------------------
message("\n====== fda-ae-t13 ======")
bn <- "fda-ae-t13"
out <- run_siera(file.path(etfl_dir, paste0(bn, "_20241022.zip")),
                 file.path(siera_dir, paste0(bn, "-siera.json")))
ref <- load_ref(out$tmp, out$zip_path)
bN <- compare_bigN(out$ard, ref, "An_79")
message("An_79 bigN match: ", bN$match, " | siera:", paste(bN$siera, collapse=","),
        " ref:", paste(bN$ref, collapse=","))
# An_80 is Mth_03a (2-level: arm x PT) — ref is top AE terms only; some off-by-1 in counts
c80 <- compare_n_pct_2level(out$ard, ref, "An_80", "Mth_03_01_n", "Mth_03_02_%")
# Use tolerance of 1.01 for n (subject counts can differ by ±1 due to deduplication methods)
c80_loose <- sum(abs(c80$data$siera_disp[c80$data$stat_name=="n"] -
                     c80$data$ref[c80$data$stat_name=="n"]) <= 1.01, na.rm=TRUE)
message("An_80 n% 2-level: exact=", c80$n_matched, "/", c80$n_total,
        " | n-count within-1=", c80_loose, "/",
        sum(c80$data$stat_name=="n"))
# An_81 is Mth_03_1 (per-PT RD, many rows) — just report
rd81 <- out$ard |> filter(AnalysisId == "An_81")
message("An_81 RD rows in siera ARD: ", nrow(rd81), " (ref has ",
        sum(ref$analysisId == "An_81"), " rows)")

# ---- fda-ae-t36 (AE severity) -----------------------------------------------
message("\n====== fda-ae-t36 ======")
bn <- "fda-ae-t36"
out <- run_siera(file.path(etfl_dir, paste0(bn, "_20241022.zip")),
                 file.path(siera_dir, paste0(bn, "-siera.json")))
ref <- load_ref(out$tmp, out$zip_path)
bN <- compare_bigN(out$ard, ref, "An_51")
message("An_51 bigN match: ", bN$match, " | siera:", paste(bN$siera, collapse=","),
        " ref:", paste(bN$ref, collapse=","))
c52 <- compare_n_pct(out$ard, ref, "An_52", "Mth_03_01_n", "Mth_03_02_%")
message("An_52 n% match: ", c52$n_matched, "/", c52$n_total)
# An_54 is Mth_03a (2-level: arm x severity)
c54 <- compare_n_pct_2level(out$ard, ref, "An_54", "Mth_03_01_n", "Mth_03_02_%")
message("An_54 n% 2-level match: ", c54$n_matched, "/", c54$n_total)
# An_55 is Mth_03_1 (per-severity RD, many rows) — just report
rd55 <- out$ard |> filter(AnalysisId == "An_55")
message("An_55 RD rows in siera ARD: ", nrow(rd55), " (ref has ",
        sum(ref$analysisId == "An_55"), " rows)")

# ---- fda-ex-t05 (exposure) --------------------------------------------------
message("\n====== fda-ex-t05 ======")
bn <- "fda-ex-t05"
out <- run_siera(file.path(etfl_dir, paste0(bn, "_20241022.zip")),
                 file.path(siera_dir, paste0(bn, "-siera.json")))
ref <- load_ref(out$tmp, out$zip_path)
bN <- compare_bigN(out$ard, ref, "An_25")
message("An_25 bigN match: ", bN$match, " | siera:", paste(bN$siera, collapse=","),
        " ref:", paste(bN$ref, collapse=","))
# An_26 is Mth_06 (continuous summary)
c26 <- compare_continuous(out$ard, ref, "An_26")
message("An_26 continuous match: ", c26$n_matched, "/", c26$n_total)
# An_29 is Mth_03_1 (RD)
c29 <- compare_rd(out$ard, ref, "An_29")
message("An_29 RD match: ", c29$n_matched, "/", c29$n_total)

message("\n====== DONE ======")
