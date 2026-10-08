# readARS()-level tests for #197: every analysis is generated against the
# population its own analysisSetId declares, with no dependence on the
# position of the analysis within its output.

# exampleARS_2.json with output Out_01 trimmed to its first `n` analyses.
trimmed_ars <- function(n, dir) {
  src <- jsonlite::fromJSON(ARS_example("exampleARS_2.json"),
                            simplifyVector = FALSE, simplifyDataFrame = FALSE)
  li <- src$mainListOfContents$contentsList$listItems[[1]]$sublist$listItems
  src$mainListOfContents$contentsList$listItems[[1]]$sublist$listItems <- li[seq_len(n)]
  p <- file.path(dir, paste0("trimmed_", n, ".json"))
  writeLines(jsonlite::toJSON(src, auto_unbox = TRUE, null = "null"), p)
  p
}

generated_script <- function(ars, output) {
  out <- withr::local_tempdir(.local_envir = parent.frame())
  suppressWarnings(
    readARS(ars, out, withr::local_tempdir(.local_envir = parent.frame()),
            spec_output = output)
  )
  readLines(file.path(out, paste0("ARD_", output, ".R")))
}

test_that("outputs with fewer than three analyses generate (#197)", {
  dir <- withr::local_tempdir()

  # bigN (USUBJID on ADZSDER) + one content analysis subset on ADZSDER:
  # previously aborted with "invalid 'replacement' argument".
  two <- generated_script(trimmed_ars(2, dir), "Out_01")
  expect_true("df2_An_01 <- df_poptot" %in% two)
  expect_true(any(grepl("^df2_An_02 <- df_pop \\|>", two)))
  expect_true(any(grepl("merge(ADZSDER", two, fixed = TRUE)))

  one <- generated_script(trimmed_ars(1, dir), "Out_01")
  expect_true("df2_An_01 <- df_poptot" %in% one)
  expect_equal(sum(grepl("# Apply Analysis Set ---", one, fixed = TRUE)), 1)
})

test_that("each analysis reads its own analysis set (fda-ds-t04, #197)", {
  ars <- test_path("testdata", "etfl", "metadata", "fda-ds-t04-siera.json")
  skip_if_not(file.exists(ars))
  script <- generated_script(ars, "Out_02")

  # one population per declared analysis set, emitted once
  expect_equal(sum(grepl("# Apply Analysis Set ---", script, fixed = TRUE)), 1)
  for (set in paste0("AnalysisSet_0", 3:7)) {
    expect_true(paste0("df_pop__", set, " <- dplyr::filter(ADSL,") %in% script, info = set)
  }
  expect_true(any(grepl("PPROTFL == 'Y'", script, fixed = TRUE)))

  expect_true("df2_An_12 <- df_poptot__AnalysisSet_03" %in% script)
  expect_true("df2_An_13 <- df_poptot__AnalysisSet_04" %in% script)
  expect_true("df2_An_15 <- df_poptot__AnalysisSet_05" %in% script)
  expect_true("df2_An_17 <- df_poptot__AnalysisSet_06" %in% script)
  expect_true("df2_An_19 <- df_poptot__AnalysisSet_07" %in% script)

  # population-based method templates (risk difference) follow the analysis'
  # own set instead of a bare df_poptot that no longer exists
  expect_true(any(grepl("full_An_22 = df_poptot__AnalysisSet_03", script, fixed = TRUE)))
  expect_false(any(grepl("\\bdf_poptot\\b(?!__)", script, perl = TRUE)))
})
