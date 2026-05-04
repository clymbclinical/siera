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


test_that(".extract_lopa_ids returns empty tibble for NULL input", {
  result <- siera:::`.extract_lopa_ids`(NULL, "Out_01")

  expect_equal(nrow(result), 0L)
  expect_true(all(c("listItem_analysisId", "listItem_outputId") %in% names(result)))
})


test_that(".extract_lopa_ids handles depth-3 nesting", {
  # Use jsonlite to produce the same nested-data-frame structure the production
  # code sees, rather than hand-crafting the fixture.
  raw <- jsonlite::fromJSON(
    '{"listItems": [{"analysisId": null, "sublist": {"listItems":
       [{"analysisId": null, "sublist": {"listItems":
         [{"analysisId": "An_deep"}]}}]}}]}'
  )
  node_df <- raw$listItems

  result <- siera:::`.extract_lopa_ids`(node_df, "Out_01")

  expect_equal(nrow(result), 1L)
  expect_equal(result$listItem_analysisId, "An_deep")
  expect_equal(result$listItem_outputId, "Out_01")
})


test_that(".read_ars_json_metadata captures all Lopa IDs including depth 3+", {
  json_path <- ARS_example("exampleARS_6.json")

  metadata <- siera:::`.read_ars_json_metadata`(json_path)

  expect_true(all(c("listItem_analysisId", "listItem_outputId") %in% names(metadata$Lopa)))
  expect_setequal(
    metadata$Lopa$listItem_analysisId,
    c("An_01", "An_02", "An_03", "An_04")
  )
})
