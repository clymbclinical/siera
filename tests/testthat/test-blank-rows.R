# Tests for the blank_rows option (#100): categorical analyses whose grouping
# is a pre-defined (non-data-driven) set of categories should be able to
# include a row for every defined category, even when no subjects fall into
# it (n = 0), instead of silently dropping it.

test_that("readARS validates the blank_rows argument", {
  ARS_path <- ARS_example("exampleARS_5.json")
  output_dir <- withr::local_tempdir()

  expect_error(
    readARS(ARS_path, output_dir, tempdir(), blank_rows = "yes"),
    class = "rlang_error"
  )
  expect_error(
    readARS(ARS_path, output_dir, tempdir(), blank_rows = NA),
    class = "rlang_error"
  )
  expect_error(
    readARS(ARS_path, output_dir, tempdir(), blank_rows = c(TRUE, FALSE)),
    class = "rlang_error"
  )
})

test_that("blank_rows = FALSE (default) does not inject factor-completion code", {
  ARS_path <- ARS_example("exampleARS_5.json")
  output_dir <- withr::local_tempdir()
  readARS(ARS_path, output_dir, tempdir())

  lines <- readLines(file.path(output_dir, "ARD_Out_01.R"))
  expect_false(any(grepl("factor(AVALCAT1", lines, fixed = TRUE)))
})

test_that("blank_rows = TRUE factor-completes pre-defined categorical groupings", {
  ARS_path <- ARS_example("exampleARS_5.json")
  output_dir <- withr::local_tempdir()
  readARS(ARS_path, output_dir, tempdir(), blank_rows = TRUE)

  lines <- readLines(file.path(output_dir, "ARD_Out_01.R"))
  # AVALCAT1 (analysis value category, pre-defined, dataDriven: false)
  expect_true(any(grepl(
    "AVALCAT1 = factor(AVALCAT1, levels = c('< 6 months', '>= 6 months'))",
    lines, fixed = TRUE
  )))
  # TRT01A (treatment arm, pre-defined, dataDriven: false) also completed
  expect_true(any(grepl("TRT01A = factor(TRT01A, levels = c(", lines, fixed = TRUE)))
  # AEXTRT (data-driven) must NOT be factor-completed
  expect_false(any(grepl("AEXTRT = factor(", lines, fixed = TRUE)))
  # PARAM is pre-defined but is also the Dss_01 data-subset discriminating
  # variable for An_04 (fixed to "Duration on Therapy (days)"), so it must
  # NOT be factor-completed for that analysis (its other categories are
  # deliberately excluded, not just absent from the data).
  an04_start <- grep("^# Analysis An_04", lines)
  an04_end <- if (length(an04_start) > 0) {
    nxt <- grep("^# Analysis An_", lines)
    nxt <- nxt[nxt > an04_start[1]]
    if (length(nxt) > 0) min(nxt) - 1L else length(lines)
  } else length(lines)
  an04_block <- lines[an04_start[1]:an04_end]
  expect_false(any(grepl("PARAM = factor(", an04_block, fixed = TRUE)))
})

test_that(".generate_blank_rows_code returns \"\" when blank_rows is FALSE or no groupings", {
  expect_identical(
    siera:::`.generate_blank_rows_code`(
      analysis_id = "An_01", groupids = "AG_01", num_grp = 1L,
      AG_vars = "AGEGR1", AG_dataDriven = c(FALSE),
      analysis_groupings = tibble::tibble(
        id = "AG_01", group_id = "AG_01_01", group_order = 1,
        group_condition_comparator = "EQ", group_condition_value = "<18"
      ),
      blank_rows = FALSE
    ),
    ""
  )
  expect_identical(
    siera:::`.generate_blank_rows_code`(
      analysis_id = "An_01", groupids = character(0), num_grp = 0L,
      AG_vars = character(0), AG_dataDriven = character(0),
      analysis_groupings = tibble::tibble(
        id = character(0), group_id = character(0), group_order = numeric(0),
        group_condition_comparator = character(0), group_condition_value = character(0)
      ),
      blank_rows = TRUE
    ),
    ""
  )
})

test_that(".generate_blank_rows_code skips data-driven groupings and subset variables", {
  ag <- tibble::tibble(
    id = c("AG_01", "AG_02"),
    group_id = c("AG_01_01", "AG_02_01"),
    group_order = c(1, 1),
    group_condition_comparator = c("EQ", "EQ"),
    group_condition_value = c("<18", "DrugA")
  )

  # AG_01 is pre-defined and not subset-fixed -> gets factor-completed.
  # AG_02 is pre-defined but IS the subset variable -> skipped.
  code <- siera:::`.generate_blank_rows_code`(
    analysis_id = "An_01",
    groupids = c("AG_01", "AG_02"),
    num_grp = 2L,
    AG_vars = c("AGEGR1", "PARAM"),
    AG_dataDriven = c(FALSE, FALSE),
    analysis_groupings = ag,
    subset_vars = "PARAM",
    blank_rows = TRUE
  )

  expect_true(grepl("AGEGR1 = factor(AGEGR1, levels = c('<18'))", code, fixed = TRUE))
  expect_false(grepl("PARAM = factor(", code, fixed = TRUE))
})

test_that(".generate_blank_rows_code skips groupings with no usable EQ-defined groups", {
  # AG_01 is pre-defined and not subset-fixed, but its only group condition
  # uses a non-EQ comparator, so .ag_group_values() resolves it to "" (with
  # its own warning) and .generate_blank_rows_code() must skip it too rather
  # than emitting a `factor(..., levels = c())` with no levels.
  ag <- tibble::tibble(
    id = "AG_01", group_id = "AG_01_01", group_order = 1,
    group_condition_comparator = "GT", group_condition_value = "18"
  )

  code <- suppressWarnings(siera:::`.generate_blank_rows_code`(
    analysis_id = "An_01", groupids = "AG_01", num_grp = 1L,
    AG_vars = "AGEGR1", AG_dataDriven = c(FALSE),
    analysis_groupings = ag, blank_rows = TRUE
  ))

  expect_identical(code, "")
})

test_that(".generate_blank_rows_code skips data-driven groupings entirely", {
  ag <- tibble::tibble(
    id = "AG_01", group_id = NA_character_, group_order = NA_real_,
    group_condition_comparator = NA_character_, group_condition_value = NA_character_
  )
  code <- siera:::`.generate_blank_rows_code`(
    analysis_id = "An_01", groupids = "AG_01", num_grp = 1L,
    AG_vars = "AEDECOD", AG_dataDriven = c(TRUE),
    analysis_groupings = ag, blank_rows = TRUE
  )
  expect_identical(code, "")
})

test_that("blank_rows = TRUE produces zero-count rows for unobserved pre-defined categories at runtime", {
  skip_on_cran()

  ARS_path <- ARS_example("exampleARS_5.json")
  adam_dir <- withr::local_tempdir()

  # All subjects fall in the "< 6 months" AVALCAT1 category and the
  # "Xanomeline Low Dose" / "Xanomeline High Dose" arms; "Placebo" and
  # ">= 6 months" are never observed in the data.
  ADSL <- data.frame(
    USUBJID = c("S1", "S2", "S3"),
    SAFFL   = c("Y", "Y", "Y"),
    TRT01A  = c("Xanomeline Low Dose", "Xanomeline High Dose", "Xanomeline Low Dose"),
    stringsAsFactors = FALSE
  )
  ADEXSUM <- data.frame(
    USUBJID  = c("S1", "S2", "S3"),
    PARAM    = "Duration on Therapy (days)",
    AEXTRT   = "Xanomeline",
    AVALCAT1 = "< 6 months",
    AVAL     = c(30, 45, 60),
    stringsAsFactors = FALSE
  )
  write.csv(ADSL, file.path(adam_dir, "ADSL.csv"), row.names = FALSE)
  write.csv(ADEXSUM, file.path(adam_dir, "ADEXSUM.csv"), row.names = FALSE)

  out_false <- withr::local_tempdir()
  out_true  <- withr::local_tempdir()
  readARS(ARS_path, out_false, adam_dir, spec_output = "Out_01", blank_rows = FALSE)
  readARS(ARS_path, out_true,  adam_dir, spec_output = "Out_01", blank_rows = TRUE)

  e_false <- new.env(parent = baseenv())
  e_true  <- new.env(parent = baseenv())
  suppressWarnings(suppressPackageStartupMessages(
    source(file.path(out_false, "ARD_Out_01.R"), local = e_false, chdir = TRUE)
  ))
  suppressWarnings(suppressPackageStartupMessages(
    source(file.path(out_true, "ARD_Out_01.R"), local = e_true, chdir = TRUE)
  ))

  ard_false <- get("ARD", envir = e_false)
  ard_true  <- get("ARD", envir = e_true)

  an04_false <- ard_false[ard_false$AnalysisId == "An_04", ]
  an04_true  <- ard_true[ard_true$AnalysisId == "An_04", ]

  # Without blank_rows: only the observed "< 6 months" x 2-arm combinations.
  expect_false(any(an04_false$group4_groupId == "AnlsGrouping_03_Avalcat1_02"))
  expect_false(any(an04_false$group1_groupId == "AnlsGrouping_01_Trta_03"))

  # With blank_rows: unobserved ">= 6 months" category and "Placebo" arm
  # appear with n = 0, correctly mapped to their defined group ids.
  blank_cat_rows <- an04_true[
    an04_true$group4_groupId == "AnlsGrouping_03_Avalcat1_02" &
      an04_true$operationid == "Mth_02_01_n",
  ]
  expect_true(nrow(blank_cat_rows) > 0)
  expect_true(all(vapply(blank_cat_rows$stat, function(x) isTRUE(x == 0), logical(1))))

  blank_arm_rows <- an04_true[
    an04_true$group1_groupId == "AnlsGrouping_01_Trta_03" &
      an04_true$operationid == "Mth_02_01_n",
  ]
  expect_true(nrow(blank_arm_rows) > 0)
  expect_true(all(vapply(blank_arm_rows$stat, function(x) isTRUE(x == 0), logical(1))))

  # Observed rows still map their group ids correctly (no NA regression from
  # the underlying grouping variable now being a factor, #100).
  expect_false(any(is.na(an04_true$group1_groupId)))
  expect_false(any(is.na(an04_true$group4_groupId)))
})
