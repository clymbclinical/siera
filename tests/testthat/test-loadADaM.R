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

test_that("Windows backslash paths are normalised to forward slashes", {
  Anas <- make_anas(listItem_analysisId = "AN1")

  Analyses <- make_analyses(
    id = "AN1",
    dataset = "ADSL",
    analysisSetId = NA_character_,
    dataSubsetId = NA_character_
  )

  AnalysisSets  <- make_analysis_sets(id = character(), condition_dataset = character())
  DataSubsets   <- make_data_subsets(id = character(), condition_dataset = character())

  code <- get_adam_loading_code(
    Anas, Analyses, AnalysisSets, DataSubsets,
    adam_path = "C:\\Users\\mbosman\\data"
  )

  expect_false(grepl("\\\\", code))             # no backslashes in output
  expect_true(grepl("C:/Users/mbosman/data/ADSL.csv", code, fixed = TRUE))
})

test_that("ADaM loading code handles a completely blank DataSubsets tibble", {
  Anas <- make_anas(listItem_analysisId = c("AN1"))

  Analyses <- make_analyses(
    id = c("AN1"),
    dataset = c("ADSL"),
    analysisSetId = c("AS1"),
    dataSubsetId = c(NA_character_)
  )

  AnalysisSets <- make_analysis_sets(
    id = c("AS1"),
    condition_dataset = c("ADSL")
  )

  # Simulate a completely blank sheet (0 rows, 0 columns)
  DataSubsets <- tibble::tibble()

  code <- get_adam_loading_code(Anas, Analyses, AnalysisSets, DataSubsets, adam_path = "adam")

  expect_true(grepl("ADSL", code))
  expect_false(grepl("Error", code))
})

test_that("ADaM loading code handles NULL DataSubsets", {
  Anas <- make_anas(listItem_analysisId = c("AN1"))

  Analyses <- make_analyses(
    id = c("AN1"),
    dataset = c("ADSL"),
    analysisSetId = c("AS1"),
    dataSubsetId = c("DS1")
  )

  AnalysisSets <- make_analysis_sets(
    id = c("AS1"),
    condition_dataset = c("ADSL")
  )

  code <- get_adam_loading_code(Anas, Analyses, AnalysisSets, NULL, adam_path = "adam")

  expect_true(grepl("ADSL", code))
})

test_that("XPT datasets emit haven::read_xpt when only a .xpt exists", {
  adam_dir <- withr::local_tempdir()
  file.create(file.path(adam_dir, "ADSL.xpt"))

  Anas <- make_anas(listItem_analysisId = "AN1")
  Analyses <- make_analyses(
    id = "AN1",
    dataset = "ADSL",
    analysisSetId = NA_character_,
    dataSubsetId = NA_character_
  )
  AnalysisSets <- make_analysis_sets(id = character(), condition_dataset = character())
  DataSubsets  <- make_data_subsets(id = character(), condition_dataset = character())

  code <- get_adam_loading_code(Anas, Analyses, AnalysisSets, DataSubsets, adam_path = adam_dir)

  expect_true(grepl("ADSL <- haven::read_xpt(", code, fixed = TRUE))
  expect_true(grepl("ADSL.xpt", code, fixed = TRUE))
  expect_false(grepl("read_csv", code, fixed = TRUE))
  # NA-handling mutate is still applied for XPT inputs
  expect_true(grepl("tidyr::replace_na", code, fixed = TRUE))
})

test_that("CSV reader is used by default when no file is present", {
  # Mirrors real-world examples where adam_path is a placeholder folder.
  Anas <- make_anas(listItem_analysisId = "AN1")
  Analyses <- make_analyses(
    id = "AN1",
    dataset = "ADSL",
    analysisSetId = NA_character_,
    dataSubsetId = NA_character_
  )
  AnalysisSets <- make_analysis_sets(id = character(), condition_dataset = character())
  DataSubsets  <- make_data_subsets(id = character(), condition_dataset = character())

  code <- get_adam_loading_code(Anas, Analyses, AnalysisSets, DataSubsets, adam_path = "adam")

  expect_true(grepl("ADSL <- readr::read_csv('adam/ADSL.csv'", code, fixed = TRUE))
  expect_false(grepl("read_xpt", code, fixed = TRUE))
})

test_that("CSV takes precedence when both .csv and .xpt exist", {
  adam_dir <- withr::local_tempdir()
  file.create(file.path(adam_dir, "ADSL.csv"))
  file.create(file.path(adam_dir, "ADSL.xpt"))

  Anas <- make_anas(listItem_analysisId = "AN1")
  Analyses <- make_analyses(
    id = "AN1",
    dataset = "ADSL",
    analysisSetId = NA_character_,
    dataSubsetId = NA_character_
  )
  AnalysisSets <- make_analysis_sets(id = character(), condition_dataset = character())
  DataSubsets  <- make_data_subsets(id = character(), condition_dataset = character())

  code <- get_adam_loading_code(Anas, Analyses, AnalysisSets, DataSubsets, adam_path = adam_dir)

  expect_true(grepl("readr::read_csv", code, fixed = TRUE))
  expect_false(grepl("read_xpt", code, fixed = TRUE))
})

test_that("mixed CSV and XPT datasets each get the matching reader", {
  adam_dir <- withr::local_tempdir()
  file.create(file.path(adam_dir, "ADSL.csv"))
  file.create(file.path(adam_dir, "ADAE.xpt"))

  Anas <- make_anas(listItem_analysisId = c("AN1", "AN2"))
  Analyses <- make_analyses(
    id = c("AN1", "AN2"),
    dataset = c("ADSL", "ADAE"),
    analysisSetId = c(NA_character_, NA_character_),
    dataSubsetId = c(NA_character_, NA_character_)
  )
  AnalysisSets <- make_analysis_sets(id = character(), condition_dataset = character())
  DataSubsets  <- make_data_subsets(id = character(), condition_dataset = character())

  code <- get_adam_loading_code(Anas, Analyses, AnalysisSets, DataSubsets, adam_path = adam_dir)

  expect_true(grepl("ADSL <- readr::read_csv(", code, fixed = TRUE))
  expect_true(grepl("ADAE <- haven::read_xpt(", code, fixed = TRUE))
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
