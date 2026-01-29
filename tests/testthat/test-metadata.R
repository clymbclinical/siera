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


test_that(".read_ars_json_metadata handles empty dataSubsets array", {
  json_path <- ARS_example("exampleARS_2.json")
  json_data <- jsonlite::fromJSON(json_path, simplifyVector = TRUE)

  # Replace dataSubsets with an empty array (as TFL Designer exports)
  json_data$dataSubsets <- list()

  tmp_json <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(json_data, tmp_json, auto_unbox = TRUE, pretty = TRUE)

  metadata <- siera:::`.read_ars_json_metadata`(tmp_json)

  expect_true("DataSubsets" %in% names(metadata))
  expect_equal(nrow(metadata$DataSubsets), 0)
  expect_true(all(
    c("id", "condition_dataset", "condition_variable") %in%
      colnames(metadata$DataSubsets)
  ))
})

test_that(".read_ars_json_metadata handles missing dataSubsets key", {
  json_path <- ARS_example("exampleARS_2.json")
  json_data <- jsonlite::fromJSON(json_path, simplifyVector = TRUE)

  # Remove dataSubsets entirely
  json_data$dataSubsets <- NULL

  tmp_json <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(json_data, tmp_json, auto_unbox = TRUE, pretty = TRUE)

  metadata <- siera:::`.read_ars_json_metadata`(tmp_json)

  expect_true("DataSubsets" %in% names(metadata))
  expect_equal(nrow(metadata$DataSubsets), 0)
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
