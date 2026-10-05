# Full-pipeline regression tests against the CDISC eTFL Portal reference ARDs.
#
# For each of the 12 eTFL Portal tables we run the whole siera pipeline
# (readARS -> generated script -> ARD) on the published ADaM data and compare
# the computed statistics against the published reference ARD. This pins siera's
# numeric output to an external CDISC source of truth, not to siera's own output.
#
# Helpers (.run_etfl_pipeline, .load_etfl_reference, .cmp_* and .expect_all_match)
# live in helper-etfl.R. All tests are skip_on_cran() because they source
# generated scripts against multi-MB ADaM data and need cards/cardx installed.
#
# fda-ae-t13 An_80 (arm x PT) is asserted against an independent treatment-
# emergent ground truth, since the published reference ARD omits the TRTEMFL
# filter its own ARS metadata mandates (#158, reference defect).
# Per-category risk difference (#157/#171/#172) is asserted against an
# independent prop.test ground truth: data-driven inner groupings use Mth_03_1a
# (fda-ae-t13 An_81/An_81_1 per PT; fda-ae-t36 An_55/An_55_1 per SOC),
# PRE-DEFINED inner groupings use Mth_03_1p, which loops the metadata-defined
# groups (fda-ae-t06 An_48_2/An_48_3 per action taken, An_50_2/An_50_3 per
# severity), and the 3-grouping per-(SOC x PT) case uses Mth_03_1b, which loops
# the observed (group2, group3) combinations (fda-ae-t36 An_57/An_57_1).

# -- Demographics / disposition / exposure (ADSL-only, fast) -------------------

test_that("fda-dm-t02 demographics: bigN, sex n%, and age summary match reference", {
  skip_on_cran()
  tmp <- withr::local_tempdir()
  ard <- .run_etfl_pipeline("fda-dm-t02", tmp)
  ref <- .load_etfl_reference("fda-dm-t02")

  expect_s3_class(ard, "data.frame")
  expect_gt(nrow(ard), 0L)

  expect_true(.cmp_bigN(ard, ref, "An_01")$match)                       # 84/84/86
  .expect_all_match(.cmp_n_pct(ard, ref, "An_02",
                               siera_cat = "variable_level", ref_cat = "Group2"))
  .expect_all_match(.cmp_continuous(ard, ref, "An_03"))
})

test_that("fda-ds-t04 disposition: bigN, n%, and risk differences match reference", {
  skip_on_cran()
  tmp <- withr::local_tempdir()
  # ADSL has no DISCONFL; the disposition ARS derives it from DCTREAS.
  ard <- .run_etfl_pipeline(
    "fda-ds-t04", tmp,
    adsl_transform = function(adsl) {
      adsl$DISCONFL <- ifelse(!is.na(adsl$DCTREAS) & nchar(trimws(adsl$DCTREAS)) > 0,
                              "Y", "")
      adsl
    })
  ref <- .load_etfl_reference("fda-ds-t04")

  expect_true(.cmp_bigN(ard, ref, "An_12")$match)
  .expect_all_match(.cmp_n_pct(ard, ref, "An_21"))
  .expect_all_match(.cmp_rd(ard, ref, "An_22"))
  .expect_all_match(.cmp_rd(ard, ref, "An_22_1"))
})

test_that("fda-ex-t05 exposure: bigN, continuous summary, and risk difference match reference", {
  skip_on_cran()
  tmp <- withr::local_tempdir()
  ard <- .run_etfl_pipeline("fda-ex-t05", tmp)
  ref <- .load_etfl_reference("fda-ex-t05")

  expect_true(.cmp_bigN(ard, ref, "An_25")$match)
  .expect_all_match(.cmp_continuous(ard, ref, "An_26"))
  .expect_all_match(.cmp_rd(ard, ref, "An_29"))
})

# -- Adverse events (ADAE / ADSL) ----------------------------------------------

test_that("fda-ae-t06 AE summary: bigN, n%, and per-category RD match spec (#171)", {
  skip_on_cran()
  tmp <- withr::local_tempdir()
  ard <- .run_etfl_pipeline("fda-ae-t06", tmp)
  ref <- .load_etfl_reference("fda-ae-t06")

  expect_true(.cmp_bigN(ard, ref, "An_30")$match)
  .expect_all_match(.cmp_n_pct(ard, ref, "An_31"))
  # An_34 is a SAE-death risk difference with zero events; siera now emits
  # RD = 0 / CI = [0, 0] matching the reference (#156 fixed).
  .expect_all_match(.cmp_rd(ard, ref, "An_34"))

  # An_48_2/An_48_3 (RD per action taken) and An_50_2/An_50_3 (RD per severity)
  # use Mth_03_1p: their inner groupings are PRE-DEFINED (dataDriven: false), so
  # the method loops the groups DEFINED in the metadata (via the
  # AG_var2_group_values valueSource) rather than observed data values -- every
  # defined group is emitted (empty ones as RD = 0 / CI = [0, 0]) and data
  # values matching no defined group (DRUG WITHDRAWN, and the "NOT APPLICALE"
  # data typo that survives Dss_68/Dss_69's correctly-spelt NOTIN filter) are
  # excluded. Category SHAPE therefore matches the published reference ARD
  # exactly (asserted below via the row counts and the explicit cats= list).
  #
  # The RD VALUES of the non-empty groups are asserted against an INDEPENDENT
  # prop.test ground truth, not against the reference (#171): the reference's
  # per-category counts are reproduced exactly by collapsing ADAE to ONE row
  # per subject (the first row) BEFORE applying the data-subset and group
  # conditions (e.g. An_50_2 arm-1 severity partition 45 MILD / 30 MODERATE /
  # 2 SEVERE is the first-row severity split, summing to the 77 any-AE
  # subjects; likewise action-taken INT 26/15/15, RED 18/19/11 across arms),
  # whereas the ARS conditions are event-level -- a subject counts in EVERY
  # category with a qualifying event. Zero-event groups (DOSE DELAY, OTHER)
  # coincide with the reference: 0 / [0, 0] on both sides.
  aeacn_groups <- c("DRUG INTERRUPTED", "DOSE REDUCED", "DOSE DELAY", "OTHER")
  aesev_groups <- c("SEVERE", "MODERATE", "MILD")

  # Shape parity with the reference: one row per defined group per statistic.
  for (an in c("An_48_2", "An_48_3", "An_50_2", "An_50_3")) {
    expect_identical(sum(ard$AnalysisId == an), sum(ref$analysisId == an))
  }

  .expect_all_match(.cmp_pergroup_rd(
    ard, "fda-ae-t06", "An_48_2",
    subset_fun = function(d) dplyr::filter(d, !(AEACN %in% c("NOT APPLICABLE", "DOSE NOT CHANGED")),
                                           TRTAN %in% c(1, 3)),
    cat_var = "AEACN", arms = c(1, 3), cats = aeacn_groups))
  .expect_all_match(.cmp_pergroup_rd(
    ard, "fda-ae-t06", "An_48_3",
    subset_fun = function(d) dplyr::filter(d, !(AEACN %in% c("NOT APPLICABLE", "DOSE NOT CHANGED")),
                                           TRTAN %in% c(2, 3)),
    cat_var = "AEACN", arms = c(2, 3), cats = aeacn_groups))
  .expect_all_match(.cmp_pergroup_rd(
    ard, "fda-ae-t06", "An_50_2",
    subset_fun = function(d) dplyr::filter(d, !is.na(AESEV), AESEV != "", TRTAN %in% c(1, 3)),
    cat_var = "AESEV", arms = c(1, 3), cats = aesev_groups))
  .expect_all_match(.cmp_pergroup_rd(
    ard, "fda-ae-t06", "An_50_3",
    subset_fun = function(d) dplyr::filter(d, !is.na(AESEV), AESEV != "", TRTAN %in% c(2, 3)),
    cat_var = "AESEV", arms = c(2, 3), cats = aesev_groups))

  # An_47_1 (n% by arm x action taken) uses Mth_03p, the categorical
  # counterpart of Mth_03_1p (#187): it aggregates per DEFINED group condition
  # instead of tabulating raw data values and stamping group ids afterwards.
  # So all four defined groups appear for all three arms -- DOSE DELAY and
  # OTHER as n = 0 / % = 0.0 rather than being dropped -- while DRUG WITHDRAWN
  # and the "NOT APPLICALE" data typo (which survives Dss_67's correctly-spelt
  # NOTIN) no longer appear as rows with NA group ids. That is exactly the
  # reference's 4 x 3 x 2 shape, asserted below.
  expect_identical(sum(ard$AnalysisId == "An_47_1"),
                   sum(ref$analysisId == "An_47_1"))

  # Values, like the RD ones above, go against the independent ground truth:
  # the reference's An_47_1 counts are the same first-record-per-subject
  # artefact (DRUG INTERRUPTED 26/15/15, DOSE REDUCED 18/19/11).
  .expect_all_match(.cmp_predefined_npct(
    ard, "fda-ae-t06", "An_47_1",
    subset_fun = function(d) dplyr::filter(d, !(AEACN %in% c("NOT APPLICABLE", "DOSE NOT CHANGED"))),
    cat_var = "AEACN", arms = c(1, 2, 3), cats = aeacn_groups))

  # The defined-but-absent groups really are zero-filled, and every row carries
  # CDISC group metadata for BOTH groupings (n_group_cols = num_grp).
  an47_1 <- ard |> dplyr::filter(AnalysisId == "An_47_1")
  expect_equal(
    sum(.etfl_safe_stat(an47_1$stat)[an47_1$group2_level %in% c("DOSE DELAY", "OTHER")]),
    0
  )
  expect_true(all(an47_1$group2_groupingId == "AnlsGrouping_24_Aeacn"))
  expect_false(any(is.na(an47_1$group1_groupId) | is.na(an47_1$group2_groupId)))
})

test_that("fda-ae-t07 AE by cause: bigN and one- and two-level n% match reference", {
  skip_on_cran()
  tmp <- withr::local_tempdir()
  ard <- .run_etfl_pipeline("fda-ae-t07", tmp)
  ref <- .load_etfl_reference("fda-ae-t07")

  expect_true(.cmp_bigN(ard, ref, "An_72")$match)
  .expect_all_match(.cmp_n_pct(ard, ref, "An_73"))
  .expect_all_match(.cmp_n_pct(ard, ref, "An_75"))
  .expect_all_match(.cmp_n_pct_2level(ard, ref, "An_74"))
})

test_that("fda-ae-t09 AE SAE/fatal: bigN, n%, two-level n%, and risk difference match reference", {
  skip_on_cran()
  tmp <- withr::local_tempdir()
  ard <- .run_etfl_pipeline("fda-ae-t09", tmp)
  ref <- .load_etfl_reference("fda-ae-t09")

  expect_true(.cmp_bigN(ard, ref, "An_65")$match)
  .expect_all_match(.cmp_n_pct(ard, ref, "An_66"))
  .expect_all_match(.cmp_n_pct_2level(ard, ref, "An_68"))
  .expect_all_match(.cmp_rd(ard, ref, "An_67"))
})

test_that("fda-ae-t12 TEAE: bigN, n%, two-level n%, and risk difference match reference", {
  skip_on_cran()
  tmp <- withr::local_tempdir()
  ard <- .run_etfl_pipeline("fda-ae-t12", tmp)
  ref <- .load_etfl_reference("fda-ae-t12")

  expect_true(.cmp_bigN(ard, ref, "An_58")$match)
  .expect_all_match(.cmp_n_pct(ard, ref, "An_59"))
  .expect_all_match(.cmp_n_pct_2level(ard, ref, "An_61"))
  .expect_all_match(.cmp_rd(ard, ref, "An_60"))
})

test_that("fda-ae-t13 AE by PT: bigN and two-level arm x PT n match spec (#158)", {
  skip_on_cran()
  tmp <- withr::local_tempdir()
  ard <- .run_etfl_pipeline("fda-ae-t13", tmp)
  ref <- .load_etfl_reference("fda-ae-t13")

  expect_true(.cmp_bigN(ard, ref, "An_79")$match)

  # An_80 (arm x preferred term) is asserted against an INDEPENDENT ground truth
  # computed from the raw ADaM, not against the published reference ARD. The
  # reference ARD omits the treatment-emergent (TRTEMFL == "Y") filter that An_80's
  # own ARS metadata mandates via Dss_04, so its per-term subject counts are
  # inflated for the 9 cells whose extra subjects have only non-treatment-emergent
  # occurrences of that PT (#158). siera applies the spec correctly: its arm x PT n
  # matches the Safety + Treatment-Emergent distinct-subject count exactly. The
  # earlier "siera under-counts" reading was the reference over-counting.
  .expect_all_match(.cmp_ae_pt_te(ard, "fda-ae-t13", "An_80"))

  # An_81 / An_81_1 (per-PT risk difference, #157) are asserted against the same
  # INDEPENDENT treatment-emergent ground truth, for the same reason as An_80:
  # the published reference RD is computed without the TRTEMFL filter (it matches
  # the non-TE basis, e.g. DIARRHOEA Low-vs-Placebo = -4.5 vs the spec-correct
  # -5.7). siera's Mth_03_1a emits one RD + 95% CI per PT over arms 1 vs 3
  # (An_81, Low Dose) and 2 vs 3 (An_81_1, High Dose); every PT matches the
  # distinct-subject prop.test recomputation.
  .expect_all_match(.cmp_pergroup_rd(
    ard, "fda-ae-t13", "An_81",
    subset_fun = function(d) dplyr::filter(d, TRTEMFL == "Y", !is.na(AEDECOD),
                                           AEDECOD != "", TRTAN %in% c(1, 3)),
    cat_var = "AEDECOD", arms = c(1, 3)))
  .expect_all_match(.cmp_pergroup_rd(
    ard, "fda-ae-t13", "An_81_1",
    subset_fun = function(d) dplyr::filter(d, TRTEMFL == "Y", !is.na(AEDECOD),
                                           AEDECOD != "", TRTAN %in% c(2, 3)),
    cat_var = "AEDECOD", arms = c(2, 3)))
})

test_that("fda-ae-t36 AE by severity: bigN, n%, two-level n%, and per-(SOC x PT) RD match spec", {
  skip_on_cran()
  tmp <- withr::local_tempdir()
  ard <- .run_etfl_pipeline("fda-ae-t36", tmp)
  ref <- .load_etfl_reference("fda-ae-t36")

  expect_true(.cmp_bigN(ard, ref, "An_51")$match)
  .expect_all_match(.cmp_n_pct(ard, ref, "An_52"))
  .expect_all_match(.cmp_n_pct_2level(ard, ref, "An_54"))

  # An_55 / An_55_1 (per-SOC risk difference, #157): one RD + 95% CI per body
  # system, over arms 1 vs 3 (Low Dose) and 2 vs 3 (High Dose), on first-
  # occurrence-of-SOC subjects (Dss_75 / Dss_76). The t36 reference RD matches
  # this spec at display precision, but rounds component percentages before
  # differencing, so we assert against the full-precision independent ground
  # truth (every SOC matches exactly).
  .expect_all_match(.cmp_pergroup_rd(
    ard, "fda-ae-t36", "An_55",
    subset_fun = function(d) dplyr::filter(d, AOCCSFL == "Y", TRTAN %in% c(1, 3)),
    cat_var = "AEBODSYS", arms = c(1, 3)))
  .expect_all_match(.cmp_pergroup_rd(
    ard, "fda-ae-t36", "An_55_1",
    subset_fun = function(d) dplyr::filter(d, AOCCSFL == "Y", TRTAN %in% c(2, 3)),
    cat_var = "AEBODSYS", arms = c(2, 3)))

  # An_57 / An_57_1 (per-(SOC x PT) risk difference, #172, method Mth_03_1b):
  # one RD + 95% CI per (AEBODSYS, AEDECOD) combination observed in the
  # first-occurrence-of-PT subset (Dss_77 / Dss_78), asserted against the
  # independent prop.test ground truth over every observed combination
  # (180 pairs for arms 1 vs 3; 187 for arms 2 vs 3).
  #
  # The reference ARD is NOT used as the oracle here, on two grounds. (1) It
  # rounds component percentages before differencing (the same defect as
  # An_55). (2) Its row set is the table-wide union of all 230 combinations,
  # padding combinations absent from the comparison arms; those filler rows
  # are internally inconsistent -- An_57 pads with zeros (correct: no events
  # in either arm means RD = 0), but 43 of An_57_1's filler rows carry
  # An_57's Low-vs-Placebo estimates verbatim (e.g. 1.2 = 1/84, a value
  # impossible in a High-vs-Placebo comparison whose true RD is 0). siera
  # emits only observed combinations, so it omits nothing but all-zero or
  # corrupted filler rows.
  .expect_all_match(.cmp_perpair_rd(
    ard, "fda-ae-t36", "An_57",
    subset_fun = function(d) dplyr::filter(d, AOCCPFL == "Y", TRTAN %in% c(1, 3)),
    cat_var2 = "AEBODSYS", cat_var3 = "AEDECOD", arms = c(1, 3)))
  .expect_all_match(.cmp_perpair_rd(
    ard, "fda-ae-t36", "An_57_1",
    subset_fun = function(d) dplyr::filter(d, AOCCPFL == "Y", TRTAN %in% c(2, 3)),
    cat_var2 = "AEBODSYS", cat_var3 = "AEDECOD", arms = c(2, 3)))
})

# -- Vital signs / labs (large ADVS / ADLB, continuous) ------------------------

test_that("ars-vs-t01 vital signs: bigN and continuous summary match reference", {
  skip_on_cran()
  tmp <- withr::local_tempdir()
  ard <- .run_etfl_pipeline("ars-vs-t01", tmp)
  ref <- .load_etfl_reference("ars-vs-t01")

  expect_true(.cmp_bigN(ard, ref, "An_98")$match)
  .expect_all_match(.cmp_continuous(ard, ref, "An_101"))
})

test_that("ars-lb-t01 lab shift: bigN and continuous n match reference", {
  skip_on_cran()
  tmp <- withr::local_tempdir()
  ard <- .run_etfl_pipeline("ars-lb-t01", tmp)
  ref <- .load_etfl_reference("ars-lb-t01")

  expect_true(.cmp_bigN(ard, ref, "An_82")$match)
  .expect_all_match(.cmp_continuous(ard, ref, "An_85"))
})

# -- Direct XPT input (issue #161) --------------------------------------------
# readARS() reads the real eTFL Portal .xpt ADaM files directly, with no
# intermediate CSV conversion. The .xpt files are committed lower-case
# (adsl.xpt, adlb.xpt) while the metadata names datasets in upper case, so this
# also exercises the case-insensitive file lookup. Results must match the same
# CDISC reference ARD as the CSV-driven test above.
test_that("ars-lb-t01: readARS reads .xpt ADaM directly and matches reference", {
  skip_on_cran()
  for (pkg in c("haven", "readr", "cards", "cardx", "broom", "parameters")) {
    if (!requireNamespace(pkg, quietly = TRUE)) skip(paste(pkg, "not installed"))
  }

  paths <- .etfl_paths("ars-lb-t01")
  if (!file.exists(paths$metadata)) skip("metadata not found: ars-lb-t01")
  if (!dir.exists(paths$adam_dir))  skip("adam data not found: ars-lb-t01")

  script_dir <- withr::local_tempdir()
  suppressWarnings(
    readARS(ARS_path = paths$metadata, output_path = script_dir,
            adam_path = paths$adam_dir)
  )

  scripts <- list.files(script_dir, pattern = "^ARD_.*\\.R$", full.names = TRUE)
  expect_gt(length(scripts), 0L)

  # The generated scripts must load ADaM via haven::read_xpt, never readr::read_csv.
  all_code <- unlist(lapply(scripts, readLines))
  expect_true(any(grepl("haven::read_xpt", all_code, fixed = TRUE)))
  expect_false(any(grepl("read_csv", all_code, fixed = TRUE)))

  ards <- lapply(scripts, function(s) {
    env <- new.env(parent = baseenv())
    suppressWarnings(suppressPackageStartupMessages(source(s, local = env)))
    env$ARD
  })
  ard <- dplyr::bind_rows(ards)
  ref <- .load_etfl_reference("ars-lb-t01")

  expect_true(.cmp_bigN(ard, ref, "An_82")$match)
  .expect_all_match(.cmp_continuous(ard, ref, "An_85"))
})

test_that("ars-lb-t02 lab summary: bigN and continuous summary match reference", {
  skip_on_cran()
  tmp <- withr::local_tempdir()
  ard <- .run_etfl_pipeline("ars-lb-t02", tmp)
  ref <- .load_etfl_reference("ars-lb-t02")

  expect_true(.cmp_bigN(ard, ref, "An_90")$match)
  # A few sparse param x arm cells have no observations, so siera yields NA where
  # the reference carries a value; every non-NA siera statistic matches exactly.
  cont <- .cmp_continuous(ard, ref, "An_93")
  expect_gt(cont$n_total, 0L)
  expect_true(all(cont$data$match[!is.na(cont$data$val)]))
})
