# Tests for the Dataset-JSON export option of readARS() (output_format =
# "datasetjson"). The cheap text-level tests assert the export code is (or is
# not) appended to the generated script; the full round-trip test sources the
# generated script, writes the JSON, and reads it back. The latter is guarded
# with skip_on_cran() and skip_if_not_installed() because it needs the optional
# {datasetjson} and {cards} packages plus real ADaM data.

test_that("output_format defaults to 'none' (no Dataset-JSON code appended)", {
  ARS_path <- ARS_example("exampleARS_4.json")
  out <- withr::local_tempdir()
  readARS(ARS_path, out, withr::local_tempdir(), spec_output = "Out_01")

  f <- list.files(out, pattern = "\\.R$", full.names = TRUE)
  expect_length(f, 1)
  lines <- paste(readLines(f), collapse = "\n")
  expect_false(grepl("Export ARD as CDISC Dataset-JSON", lines))
  expect_false(grepl("write_dataset_json", lines))
})

test_that("output_format = 'datasetjson' appends guarded export code", {
  ARS_path <- ARS_example("exampleARS_4.json")
  out <- withr::local_tempdir()
  readARS(ARS_path, out, withr::local_tempdir(), spec_output = "Out_01",
          output_format = "datasetjson")

  f <- list.files(out, pattern = "\\.R$", full.names = TRUE)
  expect_length(f, 1)
  lines <- paste(readLines(f), collapse = "\n")
  expect_match(lines, "Export ARD as CDISC Dataset-JSON")
  # guarded so a user without the optional package gets a message, not an error
  expect_match(lines, "requireNamespace\\(\"datasetjson\"")
  expect_match(lines, "datasetjson::dataset_json")
  expect_match(lines, "datasetjson::write_dataset_json")
  expect_match(lines, "ARD_Out_01\\.json")
})

test_that("output_format strictly rejects unknown, partial, and empty values", {
  ARS_path <- ARS_example("exampleARS_4.json")

  # unknown value
  expect_error(
    readARS(ARS_path, withr::local_tempdir(), withr::local_tempdir(),
            spec_output = "Out_01", output_format = "parquet"),
    "output_format"
  )
  # strict: partial matches are NOT accepted (no match.arg pmatch)
  expect_error(
    readARS(ARS_path, withr::local_tempdir(), withr::local_tempdir(),
            spec_output = "Out_01", output_format = "dataset"),
    "output_format"
  )
  # empty string
  expect_error(
    readARS(ARS_path, withr::local_tempdir(), withr::local_tempdir(),
            spec_output = "Out_01", output_format = ""),
    "output_format"
  )
})

test_that("output_format omitted or NULL defaults to 'none'", {
  ARS_path <- ARS_example("exampleARS_4.json")

  out1 <- withr::local_tempdir()
  readARS(ARS_path, out1, withr::local_tempdir(), spec_output = "Out_01")
  f1 <- list.files(out1, pattern = "\\.R$", full.names = TRUE)
  expect_false(any(grepl("write_dataset_json", readLines(f1))))

  out2 <- withr::local_tempdir()
  readARS(ARS_path, out2, withr::local_tempdir(), spec_output = "Out_01",
          output_format = NULL)
  f2 <- list.files(out2, pattern = "\\.R$", full.names = TRUE)
  expect_false(any(grepl("write_dataset_json", readLines(f2))))
})

test_that("generated Dataset-JSON export round-trips and carries labels", {
  skip_on_cran()
  skip_if_not_installed("datasetjson")
  skip_if_not_installed("cards")

  ARS_path <- ARS_example("exampleARS_4.json")
  adam_dir <- system.file("extdata", package = "siera")
  out <- withr::local_tempdir()
  readARS(ARS_path, out, adam_dir, spec_output = "Out_01",
          output_format = "datasetjson")

  f <- list.files(out, pattern = "\\.R$", full.names = TRUE)[1]
  e <- new.env(parent = baseenv())
  expect_error(
    suppressWarnings(suppressMessages(source(f, local = e, chdir = TRUE))),
    NA
  )
  ARD <- get("ARD", envir = e)

  jf <- list.files(out, pattern = "\\.json$", full.names = TRUE)
  expect_length(jf, 1)

  rj <- datasetjson::read_dataset_json(jf)
  # round-trip preserves dimensions
  expect_equal(nrow(rj), nrow(ARD))
  expect_equal(ncol(rj), ncol(ARD))
  # all {cards} list-columns were flattened to atomic
  expect_false(any(vapply(rj, is.list, logical(1))))

  # descriptive labels applied: dictionary entry + group[n]_* regex
  meta <- jsonlite::fromJSON(jf, simplifyVector = FALSE)$columns
  nms  <- vapply(meta, function(col) col$name, character(1))
  labs <- vapply(meta, function(col) col$label, character(1))
  names(labs) <- nms
  expect_equal(unname(labs[["AnalysisId"]]), "Analysis Identifier")
  expect_equal(unname(labs[["group1_groupingId"]]), "Group 1 Grouping Identifier")
  # stat written as a numeric (float) variable
  dts <- vapply(meta, function(col) col$dataType, character(1))
  names(dts) <- nms
  expect_equal(unname(dts[["stat"]]), "float")
})

# column_labels overrides (#165) ----

test_that("column_labels are validated at generation time", {
  ARS_path <- ARS_example("exampleARS_4.json")
  run <- function(labels) {
    readARS(ARS_path, withr::local_tempdir(), withr::local_tempdir(),
            spec_output = "Out_01", output_format = "datasetjson",
            column_labels = labels)
  }

  expect_error(run(c(1, 2)), "named character vector")
  expect_error(run(character(0)), "named character vector")
  expect_error(run(c("Actual Treatment")), "must be named")
  expect_error(run(c(TRT01A = "a", "b")), "must be named")
  expect_error(run(stats::setNames("a", NA_character_)), "must be named")
  expect_error(run(c(TRT01A = "a", TRT01A = "b")), "duplicated name")
  expect_error(run(c(TRT01A = NA_character_)), "missing")
})

test_that("column_labels without datasetjson output warn and are ignored", {
  ARS_path <- ARS_example("exampleARS_4.json")
  out <- withr::local_tempdir()
  expect_warning(
    readARS(ARS_path, out, withr::local_tempdir(), spec_output = "Out_01",
            column_labels = c(TRT01A = "Actual Treatment")),
    "ignored unless"
  )
  f <- list.files(out, pattern = "\\.R$", full.names = TRUE)
  expect_false(any(grepl("Actual Treatment", readLines(f))))
})

test_that("default export block carries no override code", {
  code <- .generate_datasetjson_code("Out_01", "C:\\some\\dir")
  expect_false(grepl(".ds_label_overrides", code, fixed = TRUE))
  # Windows separators are normalised in the embedded output path
  expect_match(code, "C:/some/dir", fixed = TRUE)
})

test_that("column_labels are embedded into the export block", {
  code <- .generate_datasetjson_code(
    "Out_01", "out",
    column_labels = c(TRT01A = "Actual Treatment", AnalysisId = "Analysis ID")
  )
  expect_match(
    code,
    '.ds_label_overrides <- c(TRT01A = "Actual Treatment", AnalysisId = "Analysis ID")',
    fixed = TRUE
  )
  expect_match(code, "if (nm %in% names(.ds_label_overrides))", fixed = TRUE)
  # the overrides are consulted before the built-in dictionary
  expect_lt(
    regexpr(".ds_label_overrides[nm]", code, fixed = TRUE),
    regexpr(".dict <- c(", code, fixed = TRUE)
  )
  # the emitted block is valid R
  expect_error(parse(text = code), NA)
})

test_that("column_labels export block is identical for JSON and XLSX input", {
  labels <- c(TRT01A = "Actual Treatment", AGEGR1 = "Pooled Age Group 1")
  export_block <- function(ars_file) {
    out <- withr::local_tempdir(.local_envir = parent.frame())
    suppressWarnings(
      readARS(ARS_example(ars_file), out, withr::local_tempdir(),
              output_format = "datasetjson", column_labels = labels)
    )
    vapply(sort(list.files(out, pattern = "\\.R$", full.names = TRUE)),
           function(f) {
             txt <- paste(readLines(f), collapse = "\n")
             txt <- sub("^.*# Export ARD as CDISC Dataset-JSON ----", "", txt)
             # the embedded output folder differs per call
             gsub("file\\.path\\(\"[^\"]*\"", "file.path(\"<out>\"", txt)
           },
           character(1), USE.NAMES = FALSE)
  }
  json_blocks <- export_block("exampleARS_6.json")
  xlsx_blocks <- export_block("exampleARS_6.xlsx")
  expect_gt(length(json_blocks), 0)
  expect_identical(json_blocks, xlsx_blocks)
  expect_true(all(grepl("Pooled Age Group 1", json_blocks, fixed = TRUE)))
})

test_that("column_labels override labels in the written Dataset-JSON", {
  skip_on_cran()
  skip_if_not_installed("datasetjson")
  skip_if_not_installed("cards")

  ARS_path <- ARS_example("exampleARS_4.json")
  adam_dir <- system.file("extdata", package = "siera")
  out <- withr::local_tempdir()
  readARS(ARS_path, out, adam_dir, spec_output = "Out_01",
          output_format = "datasetjson",
          column_labels = c(AnalysisId = "Analysis ID (sponsor)",
                            NOTACOLUMN = "Typo"))

  f <- list.files(out, pattern = "\\.R$", full.names = TRUE)[1]
  e <- new.env(parent = baseenv())
  # an override naming no ARD column warns at runtime but does not stop export;
  # collect all warnings since {cards} may emit its own along the way
  warns <- character(0)
  withCallingHandlers(
    suppressMessages(source(f, local = e, chdir = TRUE)),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("not found in the ARD.*NOTACOLUMN", warns)))

  jf <- list.files(out, pattern = "\\.json$", full.names = TRUE)
  expect_length(jf, 1)
  meta <- jsonlite::fromJSON(jf, simplifyVector = FALSE)$columns
  labs <- vapply(meta, function(col) col$label, character(1))
  names(labs) <- vapply(meta, function(col) col$name, character(1))
  # override beats the built-in dictionary
  expect_equal(unname(labs[["AnalysisId"]]), "Analysis ID (sponsor)")
  # non-overridden columns keep their built-in labels
  expect_equal(unname(labs[["group1_groupingId"]]), "Group 1 Grouping Identifier")
  expect_false("NOTACOLUMN" %in% names(labs))
})
