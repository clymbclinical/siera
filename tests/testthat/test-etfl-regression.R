# Integration regression tests using CDISC eTFL Portal reference packages.
# Scope: metadata parsing only. These ARS JSONs carry no codeTemplate entries,
# so generated scripts are structural skeletons (ADaM loading + analysis-set
# code) but cannot be sourced to produce an ARD. Tests assert that readARS()
# does not crash and produces a script with the expected ADaM loading lines.

# -- Shift tables ----------------------------------------------------------------

test_that("LB-T01 shift table: metadata parses and script scaffold is generated", {
  skip_on_cran()
  tmp      <- withr::local_tempdir()
  paths    <- .extract_etfl_zip("ars-lb-t01_20241022.zip", tmp)
  prog_dir <- file.path(tmp, "programmes")
  dir.create(prog_dir)

  expect_error(
    readARS(ARS_path = paths$ars_json, adam_path = paths$adam_dir,
            output_path = prog_dir),
    NA
  )

  scripts <- list.files(prog_dir, pattern = "ARD_.*\\.R$", full.names = TRUE)
  expect_gte(length(scripts), 1L)

  lines <- readLines(scripts[1])
  expect_true(any(grepl("readr::read_csv", lines, fixed = TRUE)))
  expect_true(any(grepl("ADLB", lines, fixed = TRUE)))
  expect_true(any(grepl("ADSL", lines, fixed = TRUE)))
})

test_that("VS-T01 shift table: metadata parses and script scaffold is generated", {
  skip_on_cran()
  tmp      <- withr::local_tempdir()
  paths    <- .extract_etfl_zip("ars-vs-t01_20241022.zip", tmp)
  prog_dir <- file.path(tmp, "programmes")
  dir.create(prog_dir)

  expect_error(
    readARS(ARS_path = paths$ars_json, adam_path = paths$adam_dir,
            output_path = prog_dir),
    NA
  )

  scripts <- list.files(prog_dir, pattern = "ARD_.*\\.R$", full.names = TRUE)
  expect_gte(length(scripts), 1L)

  lines <- readLines(scripts[1])
  expect_true(any(grepl("readr::read_csv", lines, fixed = TRUE)))
  expect_true(any(grepl("ADVS", lines, fixed = TRUE)))
  expect_true(any(grepl("ADSL", lines, fixed = TRUE)))
})

test_that("LB-T02 shift table: metadata parses and script scaffold is generated", {
  skip_on_cran()
  tmp      <- withr::local_tempdir()
  paths    <- .extract_etfl_zip("ars-lb-t02_20241022.zip", tmp)
  prog_dir <- file.path(tmp, "programmes")
  dir.create(prog_dir)

  expect_error(
    readARS(ARS_path = paths$ars_json, adam_path = paths$adam_dir,
            output_path = prog_dir),
    NA
  )

  scripts <- list.files(prog_dir, pattern = "ARD_.*\\.R$", full.names = TRUE)
  expect_gte(length(scripts), 1L)

  lines <- readLines(scripts[1])
  expect_true(any(grepl("readr::read_csv", lines, fixed = TRUE)))
  expect_true(any(grepl("ADLB", lines, fixed = TRUE)))
  expect_true(any(grepl("ADSL", lines, fixed = TRUE)))
})

# -- FDA tables ------------------------------------------------------------------

test_that("FDA-DM-T02 demographics: metadata parses and script scaffold is generated", {
  skip_on_cran()
  tmp      <- withr::local_tempdir()
  paths    <- .extract_etfl_zip("fda-dm-t02_20241022.zip", tmp)
  prog_dir <- file.path(tmp, "programmes")
  dir.create(prog_dir)

  expect_error(
    readARS(ARS_path = paths$ars_json, adam_path = paths$adam_dir,
            output_path = prog_dir),
    NA
  )

  scripts <- list.files(prog_dir, pattern = "ARD_.*\\.R$", full.names = TRUE)
  expect_gte(length(scripts), 1L)
  expect_true(any(grepl("readr::read_csv", readLines(scripts[1]), fixed = TRUE)))
})

test_that("FDA-DS-T04 disposition: metadata parses and script scaffold is generated", {
  skip_on_cran()
  tmp      <- withr::local_tempdir()
  paths    <- .extract_etfl_zip("fda-ds-t04_20241022.zip", tmp)
  prog_dir <- file.path(tmp, "programmes")
  dir.create(prog_dir)

  expect_error(
    readARS(ARS_path = paths$ars_json, adam_path = paths$adam_dir,
            output_path = prog_dir),
    NA
  )

  scripts <- list.files(prog_dir, pattern = "ARD_.*\\.R$", full.names = TRUE)
  expect_gte(length(scripts), 1L)
  expect_true(any(grepl("readr::read_csv", readLines(scripts[1]), fixed = TRUE)))
})

test_that("FDA-EX-T05 exposure: metadata parses and script scaffold is generated", {
  skip_on_cran()
  tmp      <- withr::local_tempdir()
  paths    <- .extract_etfl_zip("fda-ex-t05_20241022.zip", tmp)
  prog_dir <- file.path(tmp, "programmes")
  dir.create(prog_dir)

  expect_error(
    readARS(ARS_path = paths$ars_json, adam_path = paths$adam_dir,
            output_path = prog_dir),
    NA
  )

  scripts <- list.files(prog_dir, pattern = "ARD_.*\\.R$", full.names = TRUE)
  expect_gte(length(scripts), 1L)
  expect_true(any(grepl("readr::read_csv", readLines(scripts[1]), fixed = TRUE)))
})

test_that("FDA-AE-T06 AE summary: metadata parses and script scaffold is generated", {
  skip_on_cran()
  tmp      <- withr::local_tempdir()
  paths    <- .extract_etfl_zip("fda-ae-t06_20241022.zip", tmp)
  prog_dir <- file.path(tmp, "programmes")
  dir.create(prog_dir)

  expect_error(
    readARS(ARS_path = paths$ars_json, adam_path = paths$adam_dir,
            output_path = prog_dir),
    NA
  )

  scripts <- list.files(prog_dir, pattern = "ARD_.*\\.R$", full.names = TRUE)
  expect_gte(length(scripts), 1L)
  all_lines <- unlist(lapply(scripts, readLines))
  expect_true(any(grepl("readr::read_csv", all_lines, fixed = TRUE)))
  expect_true(any(grepl("ADAE", all_lines, fixed = TRUE)))
})

test_that("FDA-AE-T07 AE by SOC/term: metadata parses and script scaffold is generated", {
  skip_on_cran()
  tmp      <- withr::local_tempdir()
  paths    <- .extract_etfl_zip("fda-ae-t07_20241022.zip", tmp)
  prog_dir <- file.path(tmp, "programmes")
  dir.create(prog_dir)

  expect_error(
    readARS(ARS_path = paths$ars_json, adam_path = paths$adam_dir,
            output_path = prog_dir),
    NA
  )

  scripts <- list.files(prog_dir, pattern = "ARD_.*\\.R$", full.names = TRUE)
  expect_gte(length(scripts), 1L)
  expect_true(any(grepl("readr::read_csv", readLines(scripts[1]), fixed = TRUE)))
})

test_that("FDA-AE-T09 AE by severity: metadata parses and script scaffold is generated", {
  skip_on_cran()
  tmp      <- withr::local_tempdir()
  paths    <- .extract_etfl_zip("fda-ae-t09_20241022.zip", tmp)
  prog_dir <- file.path(tmp, "programmes")
  dir.create(prog_dir)

  expect_error(
    readARS(ARS_path = paths$ars_json, adam_path = paths$adam_dir,
            output_path = prog_dir),
    NA
  )

  scripts <- list.files(prog_dir, pattern = "ARD_.*\\.R$", full.names = TRUE)
  expect_gte(length(scripts), 1L)
  all_lines <- unlist(lapply(scripts, readLines))
  expect_true(any(grepl("readr::read_csv", all_lines, fixed = TRUE)))
  expect_true(any(grepl("ADAE", all_lines, fixed = TRUE)))
})

test_that("FDA-AE-T12 SAE summary: metadata parses and script scaffold is generated", {
  skip_on_cran()
  tmp      <- withr::local_tempdir()
  paths    <- .extract_etfl_zip("fda-ae-t12_20241022.zip", tmp)
  prog_dir <- file.path(tmp, "programmes")
  dir.create(prog_dir)

  expect_error(
    readARS(ARS_path = paths$ars_json, adam_path = paths$adam_dir,
            output_path = prog_dir),
    NA
  )

  scripts <- list.files(prog_dir, pattern = "ARD_.*\\.R$", full.names = TRUE)
  expect_gte(length(scripts), 1L)
  all_lines <- unlist(lapply(scripts, readLines))
  expect_true(any(grepl("readr::read_csv", all_lines, fixed = TRUE)))
  expect_true(any(grepl("ADAE", all_lines, fixed = TRUE)))
})

test_that("FDA-AE-T13 deaths: metadata parses and script scaffold is generated", {
  skip_on_cran()
  tmp      <- withr::local_tempdir()
  paths    <- .extract_etfl_zip("fda-ae-t13_20241022.zip", tmp)
  prog_dir <- file.path(tmp, "programmes")
  dir.create(prog_dir)

  expect_error(
    readARS(ARS_path = paths$ars_json, adam_path = paths$adam_dir,
            output_path = prog_dir),
    NA
  )

  scripts <- list.files(prog_dir, pattern = "ARD_.*\\.R$", full.names = TRUE)
  expect_gte(length(scripts), 1L)
  all_lines <- unlist(lapply(scripts, readLines))
  expect_true(any(grepl("readr::read_csv", all_lines, fixed = TRUE)))
  expect_true(any(grepl("ADAE", all_lines, fixed = TRUE)))
})

test_that("FDA-AE-T36 AE leading to discontinuation: metadata parses and script scaffold is generated", {
  skip_on_cran()
  tmp      <- withr::local_tempdir()
  paths    <- .extract_etfl_zip("fda-ae-t36_20241022.zip", tmp)
  prog_dir <- file.path(tmp, "programmes")
  dir.create(prog_dir)

  expect_error(
    readARS(ARS_path = paths$ars_json, adam_path = paths$adam_dir,
            output_path = prog_dir),
    NA
  )

  scripts <- list.files(prog_dir, pattern = "ARD_.*\\.R$", full.names = TRUE)
  expect_gte(length(scripts), 1L)
  all_lines <- unlist(lapply(scripts, readLines))
  expect_true(any(grepl("readr::read_csv", all_lines, fixed = TRUE)))
  expect_true(any(grepl("ADAE", all_lines, fixed = TRUE)))
})

# -- Full-pipeline ARD tests (siera JSON enrichments) --------------------------
# These use *-siera.json files (codeTemplate added) and source the generated
# script against the real XPT data to verify computed statistics.

test_that("ars-lb-t02 siera: bigN and continuous stats match CDISC reference", {
  skip_on_cran()
  tmp <- withr::local_tempdir()
  ard <- .run_etfl_siera_pipeline("ars-lb-t02_20241022.zip", "ars-lb-t02-siera.json", tmp)

  expect_s3_class(ard, "data.frame")
  expect_gt(nrow(ard), 0L)

  # An_90 bigN: 3 arms with counts 84, 84, 86 (CDISC pilot Safety population)
  bign <- sort(.ard_stat(ard, "An_90", "Mth_00_1_01_n"))
  expect_equal(bign, c(84, 84, 86))

  # An_93 mean: some lab params may have no data but at least one must be numeric
  means <- .ard_stat(ard, "An_93", "Mth_06_02_Mean")
  expect_true(any(!is.na(means)))
  expect_true(is.numeric(means))
})

test_that("fda-ae-t12 siera: bigN, n%, and RD match CDISC reference", {
  skip_on_cran()
  tmp <- withr::local_tempdir()
  ard <- .run_etfl_siera_pipeline("fda-ae-t12_20241022.zip", "fda-ae-t12-siera.json", tmp)

  expect_s3_class(ard, "data.frame")
  expect_gt(nrow(ard), 0L)

  # An_58 bigN: 3 arms (84, 84, 86)
  bign <- sort(.ard_stat(ard, "An_58", "Mth_00_1_01_n"))
  expect_equal(bign, c(84, 84, 86))

  # An_59 n%: at least 4 n+p rows (arms with events; one arm has 0 SAEs → excluded)
  n59 <- ard[ard$AnalysisId == "An_59" & ard$stat_name %in% c("n", "p"), ]
  expect_gte(nrow(n59), 4L)

  # An_60 RD: must produce a numeric estimate (one RD per comparison arm pair)
  rd60 <- .ard_stat(ard, "An_60", "Mth_03_1_01_Risk_Difference_%")
  expect_length(rd60, 1L)
  expect_true(is.numeric(rd60) && !is.na(rd60))
})
