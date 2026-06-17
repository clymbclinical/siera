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
  # dataSubsets is optional; an empty array [] must produce a zero-row tibble.
  ars_json <- r"[{
    "name": "Test RE", "id": "TEST_RE",
    "otherListsOfContents": [{"name": "LOPO", "label": "LOPO",
      "contentsList": {"listItems": [
        {"name": "Out1", "level": 1, "order": 1, "outputId": "Out_01"}]}}],
    "mainListOfContents": {"name": "LOPA", "label": "LOPA",
      "contentsList": {"listItems": [
        {"name": "Out1", "level": 1, "order": 1, "outputId": "Out_01",
         "sublist": {"listItems": [
           {"name": "An1", "level": 2, "order": 1, "analysisId": "An_01"}]}}]}},
    "dataSubsets": [],
    "analysisSets": [{"name": "Safety", "id": "AnalysisSet_01", "level": 1, "order": 1,
      "condition": {"dataset": "ADSL", "variable": "SAFFL", "comparator": "EQ", "value": ["Y"]}}],
    "analysisGroupings": [{"name": "Trt", "id": "AG_01",
      "dataDriven": false, "groupingDataset": "ADSL", "groupingVariable": "TRT01A",
      "groups": [{"name": "A", "id": "AG_01_1", "level": 1, "order": 1,
        "condition": {"dataset": "ADSL", "variable": "TRT01A", "comparator": "EQ", "value": ["A"]}}]}],
    "methods": [{"name": "Count", "label": "Count", "description": "n", "id": "Mth_01",
      "operations": [{"name": "n", "label": "n", "id": "Mth_01_01_n", "order": 1, "resultPattern": "XX"}],
      "codeTemplate": {"context": "R (siera)",
        "code": "df3_analysisidhere <- cards::ard_tabulate(data = df2_analysisidhere, variables = anavarhere)",
        "parameters": [{"name": "anavarhere", "description": "var", "valueSource": "ana_var"}]}}],
    "analyses": [{"name": "An1", "id": "An_01", "methodId": "Mth_01", "version": 1,
      "dataset": "ADSL", "variable": "USUBJID", "analysisSetId": "AnalysisSet_01",
      "orderedGroupings": [{"order": 1, "groupingId": "AG_01", "resultsByGroup": true}]}]
  }]"

  ars_file <- withr::local_tempfile(fileext = ".json")
  writeLines(ars_json, ars_file)

  metadata <- siera:::`.read_ars_json_metadata`(ars_file)

  expect_true("DataSubsets" %in% names(metadata))
  expect_equal(nrow(metadata$DataSubsets), 0L)
  expect_true(all(c("id", "condition_dataset", "condition_variable") %in% colnames(metadata$DataSubsets)))
})


test_that(".read_ars_json_metadata handles missing dataSubsets key", {
  # dataSubsets may be absent; the parser must still return a zero-row tibble.
  ars_json <- r"[{
    "name": "Test RE", "id": "TEST_RE",
    "otherListsOfContents": [{"name": "LOPO", "label": "LOPO",
      "contentsList": {"listItems": [
        {"name": "Out1", "level": 1, "order": 1, "outputId": "Out_01"}]}}],
    "mainListOfContents": {"name": "LOPA", "label": "LOPA",
      "contentsList": {"listItems": [
        {"name": "Out1", "level": 1, "order": 1, "outputId": "Out_01",
         "sublist": {"listItems": [
           {"name": "An1", "level": 2, "order": 1, "analysisId": "An_01"}]}}]}},
    "analysisSets": [{"name": "Safety", "id": "AnalysisSet_01", "level": 1, "order": 1,
      "condition": {"dataset": "ADSL", "variable": "SAFFL", "comparator": "EQ", "value": ["Y"]}}],
    "analysisGroupings": [{"name": "Trt", "id": "AG_01",
      "dataDriven": false, "groupingDataset": "ADSL", "groupingVariable": "TRT01A",
      "groups": [{"name": "A", "id": "AG_01_1", "level": 1, "order": 1,
        "condition": {"dataset": "ADSL", "variable": "TRT01A", "comparator": "EQ", "value": ["A"]}}]}],
    "methods": [{"name": "Count", "label": "Count", "description": "n", "id": "Mth_01",
      "operations": [{"name": "n", "label": "n", "id": "Mth_01_01_n", "order": 1, "resultPattern": "XX"}],
      "codeTemplate": {"context": "R (siera)",
        "code": "df3_analysisidhere <- cards::ard_tabulate(data = df2_analysisidhere, variables = anavarhere)",
        "parameters": [{"name": "anavarhere", "description": "var", "valueSource": "ana_var"}]}}],
    "analyses": [{"name": "An1", "id": "An_01", "methodId": "Mth_01", "version": 1,
      "dataset": "ADSL", "variable": "USUBJID", "analysisSetId": "AnalysisSet_01",
      "orderedGroupings": [{"order": 1, "groupingId": "AG_01", "resultsByGroup": true}]}]
  }]"

  ars_file <- withr::local_tempfile(fileext = ".json")
  writeLines(ars_json, ars_file)

  metadata <- siera:::`.read_ars_json_metadata`(ars_file)

  expect_true("DataSubsets" %in% names(metadata))
  expect_equal(nrow(metadata$DataSubsets), 0L)
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


test_that(".extract_lopa_ids handles node_df with no analysisId column", {
  # A category-header node: has a sublist but no analysisId key at all.
  # This covers the else branch on the 'analysisId %in% names(node_df)' check.
  raw <- jsonlite::fromJSON(
    '{"listItems": [{"name": "Category", "sublist": {"listItems":
       [{"analysisId": "An_01"}]}}]}'
  )
  node_df <- raw$listItems

  result <- siera:::`.extract_lopa_ids`(node_df, "Out_01")

  expect_equal(nrow(result), 1L)
  expect_equal(result$listItem_analysisId, "An_01")
  expect_equal(result$listItem_outputId, "Out_01")
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


test_that(".read_ars_json_metadata handles ARS with no referencedAnalysisOperations", {
  # Regression test: bare data.frame() initialisers for AN_refs / JSONAML3 had
  # no columns, causing merge(..., by = "id") to fail on continuous-only ARS.
  ars_json <- r"[{
    "name": "Continuous Only",
    "id": "TEST_CONT",
    "otherListsOfContents": [
      {
        "name": "LOPO", "label": "LOPO",
        "contentsList": {
          "listItems": [
            {"name": "Output 1", "level": 1, "order": 1, "outputId": "Out_01"}
          ]
        }
      }
    ],
    "mainListOfContents": {
      "name": "LOPA", "label": "LOPA",
      "contentsList": {
        "listItems": [
          {
            "name": "Output 1", "level": 1, "order": 1, "outputId": "Out_01",
            "sublist": {
              "listItems": [
                {"name": "Continuous summary", "level": 2, "order": 1, "analysisId": "An_01"}
              ]
            }
          }
        ]
      }
    },
    "dataSubsets": [
      {
        "name": "Deaths", "label": "Deaths", "id": "Dss_01", "level": 1, "order": 1,
        "condition": {"dataset": "ADSL", "variable": "DTHFL", "comparator": "EQ", "value": ["Y"]}
      }
    ],
    "analysisSets": [
      {
        "name": "Safety", "id": "AnalysisSet_01", "level": 1, "order": 1,
        "condition": {"dataset": "ADSL", "variable": "SAFFL", "comparator": "EQ", "value": ["Y"]}
      }
    ],
    "analysisGroupings": [
      {
        "name": "Treatment", "id": "AnlsGrouping_01_Trt01a",
        "dataDriven": false, "groupingDataset": "ADSL", "groupingVariable": "TRT01A",
        "groups": [
          {
            "name": "Active", "id": "AnlsGrouping_01_Trt01a_01", "level": 1, "order": 1,
            "condition": {"dataset": "ADSL", "variable": "TRT01A", "comparator": "EQ", "value": ["Active"]}
          }
        ]
      }
    ],
    "methods": [
      {
        "name": "Continuous summary", "description": "Descriptive stats",
        "label": "Continuous summary", "id": "Mth_03",
        "operations": [
          {"name": "n",    "label": "n",    "id": "Mth_03_01_n",    "order": 1, "resultPattern": "XX"},
          {"name": "Mean", "label": "Mean", "id": "Mth_03_02_Mean", "order": 2, "resultPattern": "XX.X"}
        ],
        "codeTemplate": {
          "context": "R (siera)",
          "code": "df3_analysisidhere <- cards::ard_continuous(data = df2_analysisidhere, by = c(byvarshere), variables = anavarhere)",
          "parameters": [
            {"name": "byvarshere", "description": "by vars",      "valueSource": "by_listc"},
            {"name": "anavarhere", "description": "analysis var", "valueSource": "ana_var"}
          ]
        }
      }
    ],
    "analyses": [
      {
        "name": "Continuous analysis", "id": "An_01",
        "methodId": "Mth_03", "version": 1,
        "dataset": "ADSL", "variable": "AGE", "analysisSetId": "AnalysisSet_01",
        "orderedGroupings": [
          {"order": 1, "groupingId": "AnlsGrouping_01_Trt01a", "resultsByGroup": true}
        ]
      }
    ]
  }]"

  ars_file <- withr::local_tempfile(fileext = ".json")
  writeLines(ars_json, ars_file)

  meta <- siera:::`.read_ars_json_metadata`(ars_file)

  expect_type(meta, "list")
  expect_setequal(names(meta), expected_components)
  expect_gt(nrow(meta$Analyses), 0L)
  expect_true("An_01" %in% meta$Analyses$id)
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
