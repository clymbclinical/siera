get_adam_loading_code <- function(...) {
  siera:::`.generate_adam_loading_code`(...)
}

make_anas <- function(...) {
  tibble::tibble(...)
}

make_analyses <- function(...) {
  tibble::tibble(...)
}

make_analysis_sets <- function(...) {
  tibble::tibble(...)
}

make_data_subsets <- function(...) {
  tibble::tibble(...)
}

extract_code_lines <- function(code) {
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]

  if (length(lines) > 0 && lines[[1]] == "") {
    lines <- lines[-1]
  }

  if (length(lines) > 0 && lines[[length(lines)]] == "") {
    lines <- lines[-length(lines)]
  }

  lines
}

test_that("ADaM loading code includes all referenced datasets once", {
  Anas <- make_anas(
    listItem_analysisId = c("AN1", "AN2", "AN3")
  )

  Analyses <- make_analyses(
    id = c("AN1", "AN2", "AN3", "AN4"),
    dataset = c("ADSL", "ADAE", "ADSL", NA_character_),
    analysisSetId = c("AS1", "AS2", NA, NA),
    dataSubsetId = c("DS1", NA, "DS2", "DS3")
  )

  AnalysisSets <- make_analysis_sets(
    id = c("AS1", "AS2"),
    condition_dataset = c("ADSL", NA_character_)
  )

  DataSubsets <- make_data_subsets(
    id = c("DS1", "DS2", "DS3"),
    condition_dataset = c("ADAV", "", "ADAE")
  )

  code <- get_adam_loading_code(
    Anas,
    Analyses,
    AnalysisSets,
    DataSubsets,
    adam_path = "adam"
  )

  expected_lines <- c(
    "# Load ADaM -------",
    "ADSL <- readr::read_csv('adam/ADSL.csv',",
    "                                      show_col_types = FALSE,",
    "                                      progress = FALSE) |>",
    "  dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ tidyr::replace_na(.x, '')))",
    "ADAE <- readr::read_csv('adam/ADAE.csv',",
    "                                      show_col_types = FALSE,",
    "                                      progress = FALSE) |>",
    "  dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ tidyr::replace_na(.x, '')))",
    "ADAV <- readr::read_csv('adam/ADAV.csv',",
    "                                      show_col_types = FALSE,",
    "                                      progress = FALSE) |>",
    "  dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ tidyr::replace_na(.x, '')))"
  )

  expect_equal(extract_code_lines(code), expected_lines)
})

test_that("header-only code is returned when no datasets are referenced", {
  Anas <- make_anas(listItem_analysisId = character())

  Analyses <- make_analyses(
    id = character(),
    dataset = character(),
    analysisSetId = character(),
    dataSubsetId = character()
  )

  AnalysisSets <- make_analysis_sets(
    id = character(),
    condition_dataset = character()
  )

  DataSubsets <- make_data_subsets(
    id = character(),
    condition_dataset = character()
  )

  code <- get_adam_loading_code(
    Anas,
    Analyses,
    AnalysisSets,
    DataSubsets,
    adam_path = "adam"
  )

  expect_equal(code, "\n# Load ADaM -------\n")
})
