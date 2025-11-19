get_analysis_set_code <- function(...) {
  siera:::`.generate_analysis_set_code`(...)
}

make_analysis_sets <- function(...) {
  tibble::tibble(...)
}

make_analyses <- function(...) {
  tibble::tibble(...)
}

make_anas <- function(...) {
  tibble::tibble(...)
}

expect_code_lines <- function(code, expected) {
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]

  if (length(lines) > 0 && lines[[1]] == "") {
    lines <- lines[-1]
  }

  testthat::expect_equal(lines, expected)
}

test_that("non-first analyses do not generate analysis set code", {
  result <- get_analysis_set_code(
    j = 2,
    analysis_sets = NULL,
    analyses = NULL,
    anas = NULL,
    analysis_set_id = "AS1",
    analysis_id = "AN1"
  )

  expect_equal(result$code, "")
  expect_equal(result$data_subset, "df_pop")
})

test_that("missing analysis set id triggers a warning and fallback code", {
  analysis_sets <- make_analysis_sets(
    id = "AS1",
    condition_dataset = "ADSL",
    condition_variable = "SAFFL",
    condition_comparator = "EQ",
    condition_value = "Y",
    name = "Safety"
  )

  analyses <- make_analyses(
    id = "AN1",
    dataset = "ADSL"
  )

  anas <- make_anas(
    listItem_analysisId = c("AN1", "AN1", "AN1")
  )

  expect_warning(
    result <- get_analysis_set_code(
      j = 1,
      analysis_sets = analysis_sets,
      analyses = analyses,
      anas = anas,
      analysis_set_id = NA_character_,
      analysis_id = "AN1"
    ),
    "missing an analysisSetId"
  )

  expected_lines <- c(
    "# Apply Analysis Set ---",
    "df_pop <- ADSL",
    "df_poptot <- df_pop"
  )

  expect_code_lines(result$code, expected_lines)
  expect_equal(result$data_subset, "df_poptot")
})

test_that("missing analysis set components warn the user and fall back", {
  analysis_sets <- make_analysis_sets(
    id = "AS1",
    condition_dataset = "ADSL",
    condition_variable = NA_character_,
    condition_comparator = "EQ",
    condition_value = "Y",
    name = "Safety"
  )

  analyses <- make_analyses(
    id = c("AN1", "AN2"),
    dataset = c("ADSL", "ADSL")
  )

  anas <- make_anas(
    listItem_analysisId = c("AN1", "AN2", "AN1")
  )

  expect_warning(
    result <- get_analysis_set_code(
      j = 1,
      analysis_sets = analysis_sets,
      analyses = analyses,
      anas = anas,
      analysis_set_id = "AS1",
      analysis_id = "AN1"
    ),
    "missing condition variable"
  )

  expected_lines <- c(
    "# Apply Analysis Set ---",
    "df_pop <- ADSL",
    "df_poptot <- df_pop"
  )

  expect_code_lines(result$code, expected_lines)
  expect_equal(result$data_subset, "df_poptot")
})

test_that("analysis set code is generated when population comes from same dataset", {
  analysis_sets <- make_analysis_sets(
    id = "AS1",
    condition_dataset = "ADSL",
    condition_variable = "SAFFL",
    condition_comparator = "EQ",
    condition_value = "Y",
    name = "Safety"
  )

  analyses <- make_analyses(
    id = c("AN1", "AN2"),
    dataset = c("ADSL", "ADAE")
  )

  anas <- make_anas(
    listItem_analysisId = c("AN1", "AN2", "AN1")
  )

  result <- get_analysis_set_code(
    j = 1,
    analysis_sets = analysis_sets,
    analyses = analyses,
    anas = anas,
    analysis_set_id = "AS1",
    analysis_id = "AN1"
  )

  expected_lines <- c(
    "# Apply Analysis Set ---",
    "df_pop <- dplyr::filter(ADSL,",
    "            SAFFL == 'Y')",
    "df_poptot <- df_pop"
  )

  expect_code_lines(result$code, expected_lines)
  expect_equal(result$data_subset, "df_poptot")
})

test_that("analysis set code merges companion dataset when needed", {
  analysis_sets <- make_analysis_sets(
    id = "AS2",
    condition_dataset = "ADSL",
    condition_variable = "SAFFL",
    condition_comparator = "EQ",
    condition_value = "Y",
    name = "Safety"
  )

  analyses <- make_analyses(
    id = c("AN1", "AN2"),
    dataset = c("ADSL", "ADAE")
  )

  anas <- make_anas(
    listItem_analysisId = c("AN1", "AN2", "AN2")
  )

  result <- get_analysis_set_code(
    j = 1,
    analysis_sets = analysis_sets,
    analyses = analyses,
    anas = anas,
    analysis_set_id = "AS2",
    analysis_id = "AN1"
  )

  expected_lines <- c(
    "# Apply Analysis Set ---",
    "overlap <- intersect(names(ADSL), names(ADAE))",
    "overlapfin <- setdiff(overlap, 'USUBJID')",
    "df_pop <- dplyr::filter(ADSL,",
    "            SAFFL == 'Y') |>",
    "            merge(ADAE |> dplyr::select(-dplyr::all_of(overlapfin)),",
    "                  by = 'USUBJID',",
    "                  all = FALSE)",
    "df_poptot = dplyr::filter(ADSL,",
    "            SAFFL == 'Y')"
  )

  expect_code_lines(result$code, expected_lines)
  expect_equal(result$data_subset, "df_poptot")
})

test_that("missing condition values are replaced with empty strings", {
  analysis_sets <- make_analysis_sets(
    id = "AS3",
    condition_dataset = "ADSL",
    condition_variable = "SAFFL",
    condition_comparator = "EQ",
    condition_value = NA_character_,
    name = "Safety"
  )

  analyses <- make_analyses(
    id = c("AN1", "AN2"),
    dataset = c("ADSL", "ADSL")
  )

  anas <- make_anas(
    listItem_analysisId = c("AN1", "AN2", "AN1")
  )

  result <- get_analysis_set_code(
    j = 1,
    analysis_sets = analysis_sets,
    analyses = analyses,
    anas = anas,
    analysis_set_id = "AS3",
    analysis_id = "AN1"
  )

  expected_lines <- c(
    "# Apply Analysis Set ---",
    "df_pop <- dplyr::filter(ADSL,",
    "            SAFFL == '')",
    "df_poptot <- df_pop"
  )

  expect_code_lines(result$code, expected_lines)
})
