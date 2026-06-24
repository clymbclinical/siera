# Shared helpers for the eTFL Portal full-pipeline regression tests.
#
# Each test runs the full siera pipeline for one CDISC eTFL Portal table:
#   1. read the table's ADaM XPT data (tests/testthat/testdata/etfl/adam/<table>/),
#   2. run readARS() on the enriched -siera.json metadata
#      (tests/testthat/testdata/etfl/metadata/), which carries the codeTemplate
#      entries siera needs to emit a runnable script,
#   3. source the generated script to produce the siera ARD,
#   4. compare computed statistics against the CDISC reference ARD
#      (tests/testthat/testdata/etfl/reference/<table>-ard.json).
#
# The reference ARDs are the published eTFL Portal results, so these tests pin
# siera's numeric output to an external CDISC source of truth.

# Resolve the testdata paths for one eTFL table (e.g. "fda-ae-t12"). ----------
.etfl_paths <- function(table) {
  base <- testthat::test_path("testdata", "etfl")
  list(
    metadata  = file.path(base, "metadata",  paste0(table, "-siera.json")),
    adam_dir  = file.path(base, "adam",      table),
    reference = file.path(base, "reference", paste0(table, "-ard.json"))
  )
}

# Run the full siera pipeline for one table and return the combined ARD. -------
# adsl_transform: optional function(adsl) -> adsl applied before ADSL.csv is
#   written (fda-ds-t04 uses it to derive DISCONFL, which is absent from ADSL).
.run_etfl_pipeline <- function(table, tmp_dir, adsl_transform = NULL) {
  # The generated script reads XPT (haven), writes/reads CSV (readr) and loads
  # cards/cardx/broom/parameters at the top, so all must be present to source it.
  for (pkg in c("haven", "readr", "cards", "cardx", "broom", "parameters")) {
    if (!requireNamespace(pkg, quietly = TRUE)) testthat::skip(paste(pkg, "not installed"))
  }

  paths <- .etfl_paths(table)
  if (!file.exists(paths$metadata)) testthat::skip(paste("metadata not found:", table))
  if (!dir.exists(paths$adam_dir))  testthat::skip(paste("adam data not found:", table))

  adam_dir <- file.path(tmp_dir, "adam")
  dir.create(adam_dir)
  for (xpt in list.files(paths$adam_dir, pattern = "\\.xpt$", full.names = TRUE)) {
    ds <- haven::read_xpt(xpt)
    nm <- toupper(tools::file_path_sans_ext(basename(xpt)))
    if (!is.null(adsl_transform) && nm == "ADSL") ds <- adsl_transform(ds)
    readr::write_csv(ds, file.path(adam_dir, paste0(nm, ".csv")))
  }

  script_dir <- file.path(tmp_dir, "scripts")
  dir.create(script_dir)
  suppressWarnings(
    readARS(ARS_path = paths$metadata, output_path = script_dir, adam_path = adam_dir)
  )

  scripts <- list.files(script_dir, pattern = "^ARD_.*\\.R$", full.names = TRUE)
  if (length(scripts) == 0L) stop("No ARD script generated for ", table)

  ards <- lapply(scripts, function(s) {
    env <- new.env(parent = baseenv())
    source(s, local = env)
    env$ARD
  })
  dplyr::bind_rows(ards)
}

# Parse a CDISC eTFL reference ARD (-ard.json) into a flat data frame. ---------
# Columns include analysisId, operationId, groupId1, Group1, Group2 and res
# (the numeric result, coerced to numeric).
.load_etfl_reference <- function(table) {
  paths <- .etfl_paths(table)
  if (!file.exists(paths$reference)) testthat::skip(paste("reference ARD not found:", table))

  ref <- jsonlite::fromJSON(paths$reference,
                            simplifyDataFrame = FALSE, simplifyVector = FALSE)
  cols <- vapply(ref$columns, `[[`, character(1), "name")
  df <- as.data.frame(
    do.call(rbind, lapply(ref$rows, function(r)
      as.data.frame(setNames(lapply(r, function(v) if (is.null(v)) NA else v), cols),
                    stringsAsFactors = FALSE))),
    stringsAsFactors = FALSE)
  df$res <- suppressWarnings(as.numeric(df$res))
  df
}

# Extract a numeric vector from the siera ARD's list-column `stat`. ------------
.etfl_safe_stat <- function(x) vapply(x, function(v) {
  if (is.null(v) || (length(v) == 1 && is.na(v))) NA_real_ else as.numeric(v[[1]])
}, numeric(1))

# Compare "big N" (population counts per group) — order-independent. -----------
# The reference often carries an extra overall-total row with an empty groupId1;
# siera emits per-group big Ns only, so we compare against the per-group rows.
.cmp_bigN <- function(siera_ard, ref_ard, ana_id) {
  s <- sort(as.numeric(siera_ard$stat[siera_ard$AnalysisId == ana_id]))
  r_rows <- ref_ard[ref_ard$analysisId == ana_id, ]
  if ("groupId1" %in% names(r_rows)) {
    keep <- !is.na(r_rows$groupId1) & nzchar(as.character(r_rows$groupId1))
    if (any(keep)) r_rows <- r_rows[keep, ]
  }
  r <- sort(r_rows$res)
  list(siera = s, ref = r,
       match = length(s) == length(r) && length(s) > 0 && all(abs(s - r) < 0.01))
}

# Compare n and % against the reference. --------------------------------------
# siera_cat / ref_cat name an optional inner category dimension to join on, in
# addition to the first grouping:
#   * NULL           — single value per group (e.g. subjects with any event)
#   * "variable_level" / "Group2" — the analysis variable's categories (e.g. SEX)
#   * "group2_level"  / "Group2"  — a second grouping (e.g. arm x term)
.cmp_n_pct <- function(siera_ard, ref_ard, ana_id,
                       n_opid = "Mth_03_01_n", pct_opid = "Mth_03_02_%",
                       siera_cat = NULL, ref_cat = NULL) {
  s <- siera_ard |>
    dplyr::filter(AnalysisId == ana_id, stat_name %in% c("n", "p")) |>
    dplyr::mutate(val = .etfl_safe_stat(stat),
                  siera_disp = ifelse(stat_name == "p", round(val * 100, 4), val),
                  grp = group1_groupId)
  r <- ref_ard |>
    dplyr::filter(analysisId == ana_id) |>
    dplyr::mutate(stat_name = dplyr::case_when(
      operationId == n_opid   ~ "n",
      operationId == pct_opid ~ "p",
      TRUE ~ NA_character_)) |>
    dplyr::filter(!is.na(stat_name)) |>
    dplyr::mutate(grp = groupId1)

  if (!is.null(siera_cat)) {
    s$cat <- as.character(s[[siera_cat]])
    r$cat <- as.character(r[[ref_cat]])
    s <- dplyr::select(s, grp, cat, stat_name, siera_disp)
    r <- dplyr::select(r, grp, cat, stat_name, ref = res)
    by <- c("grp", "cat", "stat_name")
  } else {
    s <- dplyr::select(s, grp, stat_name, siera_disp)
    r <- dplyr::select(r, grp, stat_name, ref = res)
    by <- c("grp", "stat_name")
  }
  comp <- dplyr::inner_join(s, r, by = by, relationship = "many-to-many") |>
    dplyr::distinct() |>
    dplyr::mutate(match = abs(siera_disp - ref) < 0.01)
  list(n_matched = sum(comp$match), n_total = nrow(comp), data = comp)
}

# Two-grouping (e.g. arm x term) n and % — thin wrapper over .cmp_n_pct(). -----
.cmp_n_pct_2level <- function(siera_ard, ref_ard, ana_id,
                              n_opid = "Mth_03_01_n", pct_opid = "Mth_03_02_%") {
  .cmp_n_pct(siera_ard, ref_ard, ana_id, n_opid, pct_opid,
             siera_cat = "group2_level", ref_cat = "Group2")
}

# Independent ground truth for an arm x PT distinct-subject AE count (#158). ----
# Computed straight from the raw ADaM XPT, NOT from the reference ARD: for
# fda-ae-t13 An_80 the published reference ARD omits the treatment-emergent
# (TRTEMFL == "Y") filter that the analysis's own ARS metadata mandates via
# Dss_04, so it over-counts subjects whose only occurrence of a PT is a
# non-treatment-emergent event and cannot serve as the oracle. This helper
# applies the spec (Safety population + Treatment Emergent) and counts distinct
# USUBJID per (arm, PT). The treatment grouping numbers map 1:1 to the trailing
# digits of the siera groupId (AnlsGrouping_45_Trt01An_0N -> TRT01AN == N), as
# defined by the grouping conditions in the metadata.
.etfl_ae_pt_te_truth <- function(table, arm_var = "TRT01AN") {
  paths <- .etfl_paths(table)
  adsl <- haven::read_xpt(file.path(paths$adam_dir, "adsl.xpt"))
  adae <- haven::read_xpt(file.path(paths$adam_dir, "adae.xpt"))
  pop  <- adsl |> dplyr::filter(SAFFL == "Y")
  armmap <- pop |> dplyr::transmute(USUBJID, armn = as.integer(.data[[arm_var]]))
  adae |>
    dplyr::filter(TRTEMFL == "Y", !is.na(AEDECOD), AEDECOD != "",
                  USUBJID %in% pop$USUBJID) |>
    dplyr::inner_join(armmap, by = "USUBJID") |>
    dplyr::distinct(armn, AEDECOD, USUBJID) |>
    dplyr::count(armn, AEDECOD, name = "truth")
}

# Compare siera's two-level (arm x PT) n against the spec-correct TE ground
# truth from .etfl_ae_pt_te_truth(). Asserts EVERY siera n cell, so it also
# guards against siera dropping or inventing arm x PT combinations.
.cmp_ae_pt_te <- function(siera_ard, table, ana_id) {
  truth <- .etfl_ae_pt_te_truth(table)
  s <- siera_ard |>
    dplyr::filter(AnalysisId == ana_id, stat_name == "n") |>
    dplyr::mutate(siera = .etfl_safe_stat(stat),
                  armn = as.integer(sub(".*_(\\d+)$", "\\1", group1_groupId)),
                  AEDECOD = as.character(group2_level)) |>
    dplyr::select(armn, AEDECOD, siera)
  comp <- dplyr::full_join(s, truth, by = c("armn", "AEDECOD")) |>
    dplyr::mutate(siera = ifelse(is.na(siera), 0, siera),
                  truth = ifelse(is.na(truth), 0, truth),
                  match = abs(siera - truth) < 0.01)
  list(n_matched = sum(comp$match), n_total = nrow(comp), data = comp)
}

# Compare a continuous summary (n, Mean, SD, Median, Min, Max). ----------------
.cmp_continuous <- function(siera_ard, ref_ard, ana_id) {
  ops <- c("Mth_06_01_n", "Mth_06_02_Mean", "Mth_06_03_SD",
           "Mth_06_04_Median", "Mth_06_07_Min", "Mth_06_08_Max")
  s_raw <- siera_ard |> dplyr::filter(AnalysisId == ana_id) |>
    dplyr::mutate(val = .etfl_safe_stat(stat))
  s <- if ("group2_level" %in% names(s_raw)) {
    s_raw |> dplyr::mutate(grp2 = as.character(group2_level)) |>
      dplyr::select(grp1 = group1_groupId, grp2, opid = operationid, val)
  } else {
    s_raw |> dplyr::mutate(grp2 = NA_character_) |>
      dplyr::select(grp1 = group1_groupId, grp2, opid = operationid, val)
  }
  r_raw <- ref_ard |> dplyr::filter(analysisId == ana_id, operationId %in% ops)
  r <- if ("Group2" %in% names(r_raw)) {
    r_raw |> dplyr::select(grp1 = groupId1, grp2 = Group2, opid = operationId, ref = res)
  } else {
    r_raw |> dplyr::mutate(grp2 = NA_character_) |>
      dplyr::select(grp1 = groupId1, grp2, opid = operationId, ref = res)
  }
  use_grp2 <- any(!is.na(r$grp2) & nchar(r$grp2) > 0, na.rm = TRUE)
  if (use_grp2) {
    r <- r |> dplyr::mutate(grp2 = ifelse(is.na(grp2), "", grp2))
    s <- s |> dplyr::mutate(grp2 = ifelse(is.na(grp2), "", grp2))
    comp <- dplyr::inner_join(s, r, by = c("grp1", "grp2", "opid"))
  } else {
    comp <- dplyr::inner_join(s, r, by = c("grp1", "opid"))
  }
  comp <- comp |> dplyr::mutate(match = abs(val - ref) < 0.01)
  list(n_matched = sum(comp$match, na.rm = TRUE), n_total = nrow(comp), data = comp)
}

# Compare a risk-difference estimate and its 95% CI (looser 0.05 tolerance). ---
.cmp_rd <- function(siera_ard, ref_ard, ana_id) {
  op_map <- c(
    "estimate"  = "Mth_03_1_01_Risk_Difference_%",
    "conf.low"  = "Mth_03_1_02_95%_CI_Low",
    "conf.high" = "Mth_03_1_03_95%_CI_High"
  )
  s <- siera_ard |> dplyr::filter(AnalysisId == ana_id) |>
    dplyr::mutate(val = .etfl_safe_stat(stat)) |> dplyr::select(stat_name, val)
  r <- ref_ard |> dplyr::filter(analysisId == ana_id) |> dplyr::select(operationId, res)
  comp <- data.frame(
    stat_name = names(op_map),
    ref = r$res[match(op_map, r$operationId)],
    siera = vapply(names(op_map), function(nm) {
      v <- s$val[s$stat_name == nm]; if (length(v) == 0) NA_real_ else v[[1]]
    }, numeric(1))
  )
  comp$match <- abs(comp$siera - comp$ref) < 0.05
  list(n_matched = sum(comp$match, na.rm = TRUE), n_total = nrow(comp), data = comp)
}

# testthat assertion: at least one statistic was compared and all of them matched.
.expect_all_match <- function(cmp, info = NULL) {
  testthat::expect_gt(cmp$n_total, 0)
  testthat::expect_equal(cmp$n_matched, cmp$n_total, info = info)
}
