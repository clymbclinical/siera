# Tests for internal metadata helpers in R/metadata.R

test_that(".read_ars_metadata rejects unsupported file types", {
  unsupported <- withr::local_tempfile(fileext = ".txt")
  writeLines("{}", unsupported)

  expect_error(
    siera:::`.read_ars_metadata`(unsupported),
    "reads ARS metadata from a .*json"
  )
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

  expect_setequal(names(metadata), expected_components)
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


test_that(".read_ars_metadata routes a deprecated xlsx workbook through the JSON reader", {
  xlsx_path <- ARS_example("exampleARS_6.xlsx")

  expect_warning(
    metadata <- siera:::`.read_ars_metadata`(xlsx_path),
    class = "siera_deprecated_xlsx"
  )

  # Same harmonised tables, and the same content, as the committed JSON twin.
  twin <- siera:::`.read_ars_metadata`(ARS_example("exampleARS_6.json"))
  expect_setequal(names(metadata), expected_components)
  expect_identical(sort(metadata$Analyses$id), sort(twin$Analyses$id))
  expect_identical(metadata$Lopa, twin$Lopa)
  expect_identical(
    metadata$AnalysisMethodCodeTemplate$templateCode,
    twin$AnalysisMethodCodeTemplate$templateCode
  )
})

test_that(".read_ars_xlsx_via_json warns and returns NULL when sheets are missing", {
  # exampleARS_2a.xlsx lacks the DataSubsets and AnalysisMethods sheets.
  expect_warning(
    res <- .quiet_xlsx_deprecation(
      siera:::.read_ars_xlsx_via_json(ARS_example("exampleARS_2a.xlsx"))
    ),
    "missing required sheets: DataSubsets, AnalysisMethods"
  )
  expect_null(res)
})

test_that(".read_ars_xlsx_via_json aborts when the ReportingEvent sheet is missing", {
  skip_if_not_installed("openxlsx")
  # Every sheet the old xlsx reader needed is present, but the converter also
  # needs the ReportingEvent header sheet.
  sheets <- c("OtherListsOfContents", "MainListOfContents", "DataSubsets",
              "AnalysisSets", "AnalysisGroupings", "Analyses", "AnalysisMethods",
              "AnalysisMethodCodeTemplate", "AnalysisMethodCodeParameters")
  wb <- withr::local_tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(
    stats::setNames(lapply(sheets, function(x) data.frame(id = "x")), sheets), wb
  )
  expect_error(
    .quiet_xlsx_deprecation(siera:::.read_ars_xlsx_via_json(wb)),
    "missing required sheet.*ReportingEvent"
  )
})

test_that(".read_ars_json_metadata accepts a parsed object and an explicit ars_dir", {
  # json_from lets a caller hand over an already-parsed ARS; ars_dir is where
  # relative referenceDocuments locations resolve (here: the manifest beside
  # the bundled documentRef example, from a file name that does not exist).
  json_path <- ARS_example("exampleARS_5_documentref.json")
  parsed <- jsonlite::fromJSON(json_path)

  from_obj <- siera:::.read_ars_json_metadata(
    "not-a-real-file.json",
    ars_dir = dirname(json_path),
    json_from = parsed
  )
  from_file <- siera:::.read_ars_json_metadata(json_path)
  expect_identical(from_obj, from_file)
  expect_false(anyNA(from_file$AnalysisMethodCodeTemplate$templateCode))
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

test_that("JSON parser keeps a group whose condition carries no value", {
  # e.g. an "Overall" group `TRT01AN NE <blank>` written without a value array.
  # Unnesting must keep that group (value NA) instead of silently dropping it.
  ars <- jsonlite::fromJSON(ARS_example("exampleARS_2.json"),
                            simplifyVector = FALSE)
  for (g in seq_along(ars$analysisGroupings)) {
    for (k in seq_along(ars$analysisGroupings[[g]]$groups)) {
      grp <- ars$analysisGroupings[[g]]$groups[[k]]
      if (identical(grp$id, "AnlsGrouping_01_Trt01An_04")) {
        ars$analysisGroupings[[g]]$groups[[k]]$condition$value <- NULL
      }
    }
  }
  f <- withr::local_tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(ars, auto_unbox = TRUE, null = "null"), f)

  ag <- siera:::.read_ars_json_metadata(f)$AnalysisGroupings
  overall <- ag[!is.na(ag$group_id) & ag$group_id == "AnlsGrouping_01_Trt01An_04", ]
  expect_equal(nrow(overall), 1L)
  expect_true(is.na(overall$group_condition_value))
  # the grouping's other groups are unaffected
  expect_equal(sum(ag$id == "AnlsGrouping_01_Trt01An", na.rm = TRUE), 4L)
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
  ARS_path <- ARS_example("Common_Safety_Displays_cards.json")
  output_dir <- withr::local_tempdir()
  readARS(ARS_path, output_dir, tempdir(), spec_output = "Out14-1-1",
          code_style = "expanded")

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

test_that("wrapped script delegates the _level coercion to ars_stamp() per analysis", {
  ARS_path <- ARS_example("Common_Safety_Displays_cards.json")
  output_dir <- withr::local_tempdir()
  readARS(ARS_path, output_dir, withr::local_tempdir(), spec_output = "Out14-1-1",
          code_style = "wrapped")

  lines <- readLines(file.path(output_dir, "ARD_Out14-1-1.R"))
  # no inline coercion block ...
  expect_false(any(grepl("matches('_level$')", lines, fixed = TRUE)))
  # ... every analysis ends with one ars_stamp() call, before the bind_rows
  stamp_lines <- grep("<- siera::ars_stamp(", lines, fixed = TRUE)
  n_analyses <- length(grep("^# Analysis ", lines))
  expect_gt(n_analyses, 0L)
  expect_length(stamp_lines, n_analyses)
  expect_true(all(stamp_lines < max(grep("bind_rows", lines))))
})

test_that(".as_value_list always returns a list-column-ready value (#217)", {
  expect_identical(siera:::.as_value_list(c("Y", "N")), list("Y", "N"))
  expect_identical(siera:::.as_value_list(list("A", c("B", "C"))),
                   list("A", c("B", "C")))
  expect_null(siera:::.as_value_list(NULL))
})

test_that("JSON data subsets mixing scalar and array values are read (#217)", {
  # One subset's where-clauses all carry scalar values (jsonlite simplifies
  # them to a character column), another's an array (a list column).
  ars <- jsonlite::fromJSON(ARS_example("exampleARS_6.json"),
                            simplifyVector = FALSE, simplifyDataFrame = FALSE)
  clause <- function(var, value, order) list(
    level = 2L, order = order,
    condition = list(dataset = "ADAE", variable = var,
                     comparator = if (length(value) > 1) "IN" else "EQ",
                     value = value)
  )
  ars$dataSubsets <- list(
    list(id = "Dss_S", name = "scalar", level = 1L, order = 1L,
         compoundExpression = list(logicalOperator = "AND", whereClauses = list(
           clause("TRTEMFL", "Y", 1L), clause("AESER", "Y", 2L)))),
    list(id = "Dss_A", name = "array", level = 1L, order = 2L,
         compoundExpression = list(logicalOperator = "AND", whereClauses = list(
           clause("TRTEMFL", "Y", 1L), clause("AEREL", list("POSSIBLE", "PROBABLE"), 2L))))
  )
  f <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(ars, f, auto_unbox = TRUE)

  meta <- siera:::.read_ars_json_metadata(f)
  expect_type(meta$DataSubsets$condition_value, "list")
  rel <- meta$DataSubsets[meta$DataSubsets$condition_variable %in% "AEREL", ]
  expect_identical(unlist(rel$condition_value), c("POSSIBLE", "PROBABLE"))
})
