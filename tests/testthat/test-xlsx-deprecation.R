# Deprecated xlsx ARS input (#179) ---------------------------------------------
# readARS() still accepts an .xlsx workbook for one release, but routes it
# through the ARS JSON reader (via the same in-memory conversion as
# ars_xlsx_to_json()) and warns with class `siera_deprecated_xlsx`.

test_that("readARS() warns that xlsx input is deprecated, on every call", {
  for (k in 1:2) {
    expect_warning(
      readARS(ARS_example("exampleARS_6.xlsx"), withr::local_tempdir(),
              withr::local_tempdir()),
      class = "siera_deprecated_xlsx"
    )
  }
})

test_that("readARS() on xlsx generates the same scripts as on the committed JSON twin", {
  skip_on_cran()
  adam <- withr::local_tempdir()
  for (base in c("exampleARS_2", "exampleARS_3", "exampleARS_5", "exampleARS_6",
                 "Common_Safety_Displays_cards")) {
    for (style in c("wrapped", "expanded")) {
      from_xlsx <- .gen_scripts(ARS_example(paste0(base, ".xlsx")), adam, style)
      from_json <- .gen_scripts(ARS_example(paste0(base, ".json")), adam, style)
      if (base == "exampleARS_2") {
        # The workbook's "Overall" group is `TRT01AN NE <blank cell>` (NA); the
        # hand-written JSON twin spells the same blank as "". Only that
        # group-level literal differs.
        from_xlsx <- lapply(from_xlsx, function(x) gsub("'NA'", "''", x, fixed = TRUE))
      }
      if (base == "Common_Safety_Displays_cards") {
        from_xlsx <- .drop_predefined_group_methods(from_xlsx)
        from_json <- .drop_predefined_group_methods(from_json)
      }
      expect_identical(names(from_xlsx), names(from_json), info = base)
      for (f in names(from_json)) {
        expect_identical(from_xlsx[[f]], from_json[[f]],
                         info = paste(base, style, f))
      }
    }
  }
})

test_that("generated scripts no longer load readxl", {
  out <- withr::local_tempdir()
  readARS(ARS_example("exampleARS_6.json"), out, withr::local_tempdir())
  lines <- readLines(file.path(out, "ARD_Out_01.R"))
  expect_false(any(grepl("library(readxl)", lines, fixed = TRUE)))
  expect_true(any(grepl("library(cards)", lines, fixed = TRUE)))
})

test_that("xlsx multi-value IN cells are trimmed on the way in (#213)", {
  # The bundled workbook writes "POSSIBLE | PROBABLE"; the value before the pipe
  # used to keep its trailing blank and silently match no records.
  out <- withr::local_tempdir()
  .quiet_xlsx_deprecation(
    readARS(ARS_example("Common_Safety_Displays_cards.xlsx"), out,
            withr::local_tempdir(), spec_output = "Out14-3-1-1")
  )
  lines <- readLines(file.path(out, "ARD_Out14-3-1-1.R"))
  expect_true(any(grepl("AEREL %in% c('POSSIBLE', 'PROBABLE')", lines, fixed = TRUE)))
  expect_true(any(grepl("AEACN %in% c('DOSE REDUCED', 'DRUG INTERRUPTED')", lines,
                        fixed = TRUE)))
  expect_false(any(grepl("'POSSIBLE '", lines, fixed = TRUE)))
})
