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
