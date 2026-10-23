# A percentage template whose DENOMINATOR analysis the ARS does not name (TFL
# Designer writes analysisId "" and operationId "<'Total N' information
# missing>" when the Total N analysis is absent) used to generate `df2_` and
# fail at run time. readARS() now stands in the analysis-set population, grouped
# like the numerator, and warns.

# exampleARS_7 with the Total N references of An_02 and An_04 removed, and the
# three `NE ''` Total groups dropped from the treatment grouping so it derives
# as an ARS-ordered factor (#203), which the denominator frame must match.
.ars_without_denominator <- function(dir) {
  ars <- jsonlite::fromJSON(ARS_example("exampleARS_7.json"), simplifyVector = FALSE)

  for (i in seq_along(ars$analyses)) {
    a <- ars$analyses[[i]]
    for (r in seq_along(a$referencedAnalysisOperations)) {
      ref <- a$referencedAnalysisOperations[[r]]
      if (grepl("_DEN$", ref$referencedOperationRelationshipId)) {
        a$referencedAnalysisOperations[[r]]$analysisId <-
          if (a$id == "An_02") "" else "<'Total N' information missing>"
      }
    }
    ars$analyses[[i]] <- a
  }
  for (i in seq_along(ars$analysisGroupings)) {
    g <- ars$analysisGroupings[[i]]
    if (g$id == "AnlsGrouping_01_Trt01An") {
      g$groups <- Filter(function(x) x$condition$comparator == "EQ", g$groups)
      ars$analysisGroupings[[i]] <- g
    }
  }
  path <- file.path(dir, "no_denominator.json")
  jsonlite::write_json(ars, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  path
}

# The same reporting event with its Total N references intact: the oracle.
.ars_with_denominator <- function(dir) {
  ars <- jsonlite::fromJSON(ARS_example("exampleARS_7.json"), simplifyVector = FALSE)
  for (i in seq_along(ars$analysisGroupings)) {
    g <- ars$analysisGroupings[[i]]
    if (g$id == "AnlsGrouping_01_Trt01An") {
      g$groups <- Filter(function(x) x$condition$comparator == "EQ", g$groups)
      ars$analysisGroupings[[i]] <- g
    }
  }
  path <- file.path(dir, "with_denominator.json")
  jsonlite::write_json(ars, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  path
}

test_that("an unresolved denominator falls back to the analysis-set population", {
  ars_dir <- withr::local_tempdir()
  output_dir <- withr::local_tempdir()
  adam_dir <- withr::local_tempdir()

  warnings <- testthat::capture_warnings(
    readARS(.ars_without_denominator(ars_dir), output_dir, adam_dir)
  )
  expect_true(any(grepl("\"An_02\".*names no denominator analysis", warnings)))
  expect_true(any(grepl(
    "\"An_04\".*names denominator analysis <'Total N' information missing>, which this reporting event does not define",
    warnings
  )))

  lines <- readLines(file.path(output_dir, "ARD_Out_01.R"))
  code <- paste(lines, collapse = "\n")

  # No dangling `df2_` reference is left for the template to trip over.
  expect_false(any(grepl("df2_\\s*(\\||$)", lines)))
  expect_false(any(grepl("df2_denomanaidhere", lines, fixed = TRUE)))

  # The fallback frame is the subject-level population, carrying the treatment
  # grouping as the same ARS-ordered factor the numerator derives.
  expect_true(any(grepl("^df2_An_02_denom <- df_poptot \\|>$", lines)))
  expect_true(grepl(
    "df2_An_02_denom <- df_poptot \\|>\\s*\\n\\s*dplyr::mutate\\(\\s*\\n\\s*TRT01AN = factor\\(dplyr::case_when\\(",
    code
  ))
  expect_true(grepl("denom_dataset = df2_An_02_denom |>", code, fixed = TRUE))
  expect_true(grepl("dplyr::select(TRT01AN)", code, fixed = TRUE))
  expect_true(grepl("denom_dataset = df2_An_04_denom |>", code, fixed = TRUE))

  # An_03 (continuous) has no denominator and gets no fallback.
  expect_false(grepl("df2_An_03_denom", code, fixed = TRUE))
})

test_that("the fallback denominator reproduces the Total N analysis' percentages", {
  skip_on_cran()
  skip_if_not_installed("cards")

  ars_dir <- withr::local_tempdir()
  adam_dir <- system.file("extdata", package = "siera")

  run <- function(ars_path) {
    output_dir <- withr::local_tempdir()
    suppressWarnings(readARS(ars_path, output_dir, adam_dir))
    e <- new.env(parent = baseenv())
    suppressWarnings(suppressPackageStartupMessages(
      source(file.path(output_dir, "ARD_Out_01.R"), local = e, chdir = TRUE)
    ))
    get("ARD", envir = e)
  }

  pct <- function(ard, analysis) {
    ard |>
      dplyr::filter(AnalysisId == analysis, stat_name %in% c("n", "p")) |>
      dplyr::mutate(
        group1_level = as.character(group1_level),
        group2_level = as.character(group2_level),
        stat = vapply(stat, function(v) as.numeric(v)[1], numeric(1))
      ) |>
      dplyr::select(group1_level, group2_level, stat_name, stat) |>
      dplyr::arrange(group1_level, group2_level, stat_name)
  }

  with_den <- run(.ars_with_denominator(ars_dir))
  without_den <- run(.ars_without_denominator(ars_dir))

  for (analysis in c("An_02", "An_04")) {
    expect_gt(nrow(pct(with_den, analysis)), 0)
    expect_equal(pct(without_den, analysis), pct(with_den, analysis))
  }

  # Every percentage is finite: the fallback frame joined on the factor.
  p <- pct(without_den, "An_02") |> dplyr::filter(stat_name == "p")
  expect_false(any(is.na(p$stat)))
})
