# Tests for internal metadata helpers in R/metadata.R

test_that(".read_ars_metadata rejects unsupported file types", {
  unsupported <- withr::local_tempfile(fileext = ".txt")
  writeLines("{}", unsupported)

  expect_warning(
    res <- siera:::`.read_ars_metadata`(unsupported),
    "Input ARS file must be JSON or xlsx"
  )
  expect_null(res)
})

expected_components <- c(
  "Lopo",
  "Lopa",
  "DataSubsets",
  "AnalysisSets",
  "AnalysisGroupings",
  "Analyses",
  "AnalysisMethods",
  "AnalysisMethodCodeTemplate",
  "AnalysisMethodCodeParameters"
)

test_that(".read_ars_metadata returns harmonised JSON metadata", {
  json_path <- ARS_example("exampleARS_2.json")

  metadata <- siera:::`.read_ars_metadata`(json_path)

  expect_equal(metadata$file_ext, "json")
  expect_setequal(names(metadata)[-1], expected_components)
  expect_gt(nrow(metadata$Analyses), 0)
  expect_true(all(c("listItem_outputId", "listItem_name") %in% names(metadata$Lopo)))
})


test_that(".read_ars_json_metadata returns the expected tables", {
  json_path <- ARS_example("exampleARS_2.json")

  metadata <- siera:::`.read_ars_json_metadata`(json_path)

  expect_setequal(names(metadata), expected_components)
  expect_true(all(vapply(metadata, inherits, logical(1), "data.frame")))
  expect_true(all(c("listItem_analysisId", "listItem_outputId") %in% names(metadata$Lopa)))
  expect_false(any(metadata$DataSubsets$condition_value %in% "NULL"))
})


test_that(".read_ars_metadata dispatches to the XLSX reader", {
  skip_if_not_installed("readxl")

  xlsx_path <- ARS_example("exampleARS_2.xlsx")

  metadata <- siera:::`.read_ars_metadata`(xlsx_path)

  expect_equal(metadata$file_ext, "xlsx")
  expect_setequal(names(metadata)[-1], expected_components)
})


test_that(".read_ars_xlsx_metadata returns the expected tables", {
  skip_if_not_installed("readxl")

  xlsx_path <- ARS_example("exampleARS_2.xlsx")

  metadata <- siera:::`.read_ars_xlsx_metadata`(xlsx_path)

  expect_setequal(names(metadata), expected_components)
  expect_gt(nrow(metadata$Analyses), 0)
  expect_true(all(c("listItem_analysisId", "listItem_outputId") %in% names(metadata$Lopa)))
})


test_that("unnesting logic expands IN condition values to one row per value", {
  # Directly test the unnesting logic used in the JSON parser for multi-value
  # group conditions (e.g. variable IN [value1, value2]).  The parser stores
  # condition.value as a list-column; each element is a character vector whose
  # length equals the number of values in the ARS condition array.
  tmp_AG <- tibble::tibble(
    group_id   = c("AG_01_1", "AG_01_2"),
    group_name = c("Active", "Placebo"),
    # IN condition: two values map to one group id
    group_condition_value = list(c("1", "2"), c("3"))
  )

  # Apply the same unnesting logic as metadata.R
  if ("group_condition_value" %in% names(tmp_AG) && is.list(tmp_AG$group_condition_value)) {
    tmp_AG <- tidyr::unnest(tmp_AG, cols = group_condition_value)
  }

  # IN group must expand to two rows, both pointing to AG_01_1
  expect_equal(nrow(tmp_AG), 3L)
  expect_setequal(tmp_AG$group_condition_value[tmp_AG$group_id == "AG_01_1"], c("1", "2"))
  expect_equal(tmp_AG$group_condition_value[tmp_AG$group_id == "AG_01_2"], "3")
  # Column must be plain character after unnesting (not list)
  expect_type(tmp_AG$group_condition_value, "character")
})


test_that("JSON parser returns plain character group_condition_value", {
  # Integration check: after parsing a real fixture the group_condition_value
  # column must be character, not list (confirming the unnest ran).
  metadata <- siera:::`.read_ars_json_metadata`(ARS_example("exampleARS_1.json"))
  expect_type(metadata$AnalysisGroupings$group_condition_value, "character")
})


test_that("generated script coerces _level columns to character per df3 before bind_rows", {
  ARS_path <- ARS_example("Common_Safety_Displays_cards.xlsx")
  output_dir <- withr::local_tempdir()
  readARS(ARS_path, output_dir, tempdir(), spec_output = "Out14-1-1")

  lines <- readLines(file.path(output_dir, "ARD_Out14-1-1.R"))
  # Every analysis must have a targeted _level$ coercion block (before bind_rows)
  coerce_lines <- grep("matches('_level$')", lines, fixed = TRUE)
  expect_true(length(coerce_lines) > 0)
  # vapply with null guard must be present
  expect_true(any(grepl("vapply(.x", lines, fixed = TRUE)))
  # All coerce lines appear before the final bind_rows
  bind_rows_line <- grep("bind_rows", lines)
  expect_true(length(bind_rows_line) > 0)
  expect_true(all(coerce_lines < max(bind_rows_line)))
})
