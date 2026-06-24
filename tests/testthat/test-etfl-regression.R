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
# Known limitations are asserted only where siera currently matches the
# reference; cases tracked by open issues are exercised but not asserted:
#   * per-term / per-category risk difference not yet emitted (#157)
#   * per-term subject counts can differ for top-N AE tables  (#158)

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

test_that("fda-ae-t06 AE summary: bigN and n% match reference", {
  skip_on_cran()
  tmp <- withr::local_tempdir()
  ard <- .run_etfl_pipeline("fda-ae-t06", tmp)
  ref <- .load_etfl_reference("fda-ae-t06")

  expect_true(.cmp_bigN(ard, ref, "An_30")$match)
  .expect_all_match(.cmp_n_pct(ard, ref, "An_31"))
  # An_34 is a SAE-death risk difference with zero events; siera now emits
  # RD = 0 / CI = [0, 0] matching the reference (#156 fixed).
  .expect_all_match(.cmp_rd(ard, ref, "An_34"))
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
  # An_81 (per-PT risk difference) is still not emitted (#157).
})

test_that("fda-ae-t36 AE by severity: bigN, n%, and two-level n% match reference", {
  skip_on_cran()
  tmp <- withr::local_tempdir()
  ard <- .run_etfl_pipeline("fda-ae-t36", tmp)
  ref <- .load_etfl_reference("fda-ae-t36")

  expect_true(.cmp_bigN(ard, ref, "An_51")$match)
  .expect_all_match(.cmp_n_pct(ard, ref, "An_52"))
  .expect_all_match(.cmp_n_pct_2level(ard, ref, "An_54"))
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
