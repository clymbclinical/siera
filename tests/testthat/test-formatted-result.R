# Unit tests for the scalar pattern formatter ----

test_that(".format_ars_result renders every observed pattern shape", {
  # counts
  expect_equal(.format_ars_result(86, "(N=XX)"), "(N=86)")
  expect_equal(.format_ars_result(254, "XXX"), "254")
  expect_equal(.format_ars_result(34, "XX"), "34")

  # decimals honour the X-count after the dot, with trailing zeros
  expect_equal(.format_ars_result(12.345, "XX.X"), "12.3")
  expect_equal(.format_ars_result(105, "XX.X"), "105.0")
  expect_equal(.format_ars_result(3.14159, "(XX.XX)"), "(3.14)")
  expect_equal(.format_ars_result(0.04321, "X.XXXX"), "0.0432")

  # values are ROUNDED, not truncated: 1.369 -> 1.4 (not 1.3)
  expect_equal(.format_ars_result(1.369, "X.X"), "1.4")
  expect_equal(.format_ars_result(2.649, "X.XX"), "2.65")
  expect_equal(.format_ars_result(85.6, "(N=)"), "(N=86)")

  # prefix/suffix around the X-run is preserved (incl. leading space)
  expect_equal(.format_ars_result(75.58, "( XX.X)"), "( 75.6)")

  # negative values keep their sign inside the pattern
  expect_equal(.format_ars_result(-5.678, "XX.X"), "-5.7")
  expect_equal(.format_ars_result(-5.678, "(XX.XX)"), "(-5.68)")
})

test_that(".format_ars_result handles patterns without any X", {
  # number inserted before a trailing paren
  expect_equal(.format_ars_result(86, "(N=)"), "(N=86)")
  # no trailing paren: number appended
  expect_equal(.format_ars_result(86, "N="), "N=86")
})

test_that(".format_ars_result guards missing/invalid inputs", {
  expect_identical(.format_ars_result(NA, "XX.X"), NA_character_)
  expect_identical(.format_ars_result(5, NA), NA_character_)
  expect_identical(.format_ars_result(5, NULL), NA_character_)
  expect_identical(.format_ars_result(5, ""), NA_character_)
  expect_identical(.format_ars_result(NULL, "XX"), NA_character_)
  expect_identical(.format_ars_result(c(1, 2), "XX"), NA_character_)
  expect_identical(.format_ars_result(Inf, "XX"), NA_character_)
  # non-numeric values are coerced when possible, NA when not
  expect_equal(.format_ars_result("12.34", "XX.X"), "12.3")
  expect_identical(.format_ars_result("abc", "XX"), NA_character_)
})

# Unit tests for the code generator ----

test_that(".generate_formatted_result_code embeds the operation patterns", {
  am <- tibble::tibble(
    operation_id = c("Mth_01_01_n", "Mth_02_02_%", "Mth_02_02_%", "Mth_x", NA),
    operation_resultPattern = c("(N=XX)", "( XX.X)", "( XX.X)", NA, "XX")
  )
  code <- .generate_formatted_result_code(am)

  expect_true(grepl('"Mth_01_01_n"', code, fixed = TRUE))
  expect_true(grepl('"(N=XX)"', code, fixed = TRUE))
  # duplicated operation ids are embedded once
  expect_equal(lengths(regmatches(code, gregexpr('"Mth_02_02_%"', code))), 1L)
  # rows with NA pattern or NA operation id are dropped
  expect_false(grepl('"Mth_x"', code, fixed = TRUE))
  # the formatter itself is deparsed into the block
  expect_true(grepl(".format_ars_result <- function", code, fixed = TRUE))
  expect_true(grepl("fmt_fun", code, fixed = TRUE))
  # block is parseable R code
  expect_no_error(parse(text = code))
})

test_that(".generate_formatted_result_code handles absent pattern metadata", {
  # no metadata at all
  code_null <- .generate_formatted_result_code(NULL)
  expect_true(grepl("operationid = character(0)", code_null, fixed = TRUE))
  expect_no_error(parse(text = code_null))

  # metadata without a resultPattern column (defensive path)
  code_nocol <- .generate_formatted_result_code(
    tibble::tibble(operation_id = "Mth_01_01_n")
  )
  expect_true(grepl("operationid = character(0)", code_nocol, fixed = TRUE))

  # metadata whose patterns are all blank/NA
  code_blank <- .generate_formatted_result_code(
    tibble::tibble(
      operation_id = c("a", "b"),
      operation_resultPattern = c(NA, "")
    )
  )
  expect_true(grepl("operationid = character(0)", code_blank, fixed = TRUE))
})

test_that(".quote_csv_lines quotes, escapes and comma-separates", {
  expect_equal(.quote_csv_lines("a"), '"a"')
  expect_equal(.quote_csv_lines(c("a", "b")), c('"a",', '"b"'))
  expect_equal(.quote_csv_lines('sa"y'), '"sa\\"y"')
})

test_that("generated block works at runtime on ARDs without stat/operationid", {
  # stub-only ARD (empty-data guard path): no stat, no operationid, no stat_name
  code <- .generate_formatted_result_code(
    tibble::tibble(
      operation_id = "Mth_01_01_n",
      operation_resultPattern = "(N=XX)"
    )
  )
  e <- new.env(parent = globalenv())
  e$ARD <- data.frame(
    AnalysisId = "An_01", MethodId = "Mth_01", OutputId = "Out_01",
    stringsAsFactors = FALSE
  )
  eval(parse(text = code), envir = e)
  expect_identical(e$ARD$res, NA_real_)
  expect_identical(e$ARD$pattern, NA_character_)
  expect_identical(e$ARD$disp, NA_character_)

  # plain numeric (non-list) stat column is also supported
  e2 <- new.env(parent = globalenv())
  e2$ARD <- data.frame(
    operationid = c("Mth_01_01_n", "unknown_op"),
    stat = c(86, 1),
    fmt_fun = c("f", "f"),
    stringsAsFactors = FALSE
  )
  eval(parse(text = code), envir = e2)
  expect_equal(e2$ARD$res, c(86, 1))
  expect_equal(e2$ARD$disp, c("(N=86)", NA_character_))
  expect_false("fmt_fun" %in% names(e2$ARD))
})

# Integration: generated scripts produce res / pattern / disp ----

test_that("sourced ARD carries res/pattern/disp and drops fmt_fun/fmt_fn", {
  skip_on_cran()

  ARS_path <- ARS_example("exampleARS_6.json")
  adam_dir <- system.file("extdata", package = "siera")
  output_dir <- withr::local_tempdir()

  readARS(ARS_path, output_dir, adam_dir)
  f <- file.path(output_dir, "ARD_Out_01.R")
  expect_true(file.exists(f))

  e <- new.env(parent = baseenv())
  suppressWarnings(suppressPackageStartupMessages(
    source(f, local = e, chdir = TRUE)
  ))
  ARD <- get("ARD", envir = e)

  expect_true(all(c("res", "pattern", "disp") %in% names(ARD)))
  expect_false(any(c("fmt_fun", "fmt_fn") %in% names(ARD)))
  expect_type(ARD$disp, "character")
  expect_type(ARD$res, "double")

  # count operation: pattern (N=) renders as (N=<int>)
  n_rows <- ARD[!is.na(ARD$operationid) & ARD$operationid == "Mth_01_01_n", ]
  expect_true(all(grepl("^\\(N=\\d+\\)$", n_rows$disp)))
  expect_equal(n_rows$disp, paste0("(N=", n_rows$res, ")"))

  # {cards} proportion (stat_name == "p"): disp is formatted from res * 100
  p_rows <- ARD[!is.na(ARD$stat_name) & ARD$stat_name == "p" &
                  !is.na(ARD$pattern), ]
  expect_gt(nrow(p_rows), 0)
  expect_equal(
    p_rows$disp,
    vapply(
      seq_len(nrow(p_rows)),
      function(i) .format_ars_result(p_rows$res[i] * 100, p_rows$pattern[i]),
      character(1)
    )
  )

  # continuous mean honours its pattern's decimal count (XX.X -> 1 dp);
  # rows whose raw stat is missing (empty-data analyses) stay NA
  mean_rows <- ARD[!is.na(ARD$operationid) &
                     ARD$operationid == "Mth_03_02_Mean", ]
  expect_identical(is.na(mean_rows$disp), is.na(mean_rows$res))
  fmt <- mean_rows$disp[!is.na(mean_rows$disp)]
  expect_gt(length(fmt), 0)
  expect_true(all(grepl("^-?\\d+\\.\\d$", fmt)))
})

test_that("formatting block is identical for JSON and XLSX ARS sources", {
  ARS_json <- ARS_example("exampleARS_6.json")
  ARS_xlsx <- ARS_example("exampleARS_6.xlsx")
  out_json <- withr::local_tempdir()
  out_xlsx <- withr::local_tempdir()
  adam_dir <- withr::local_tempdir()

  readARS(ARS_json, out_json, adam_dir)
  readARS(ARS_xlsx, out_xlsx, adam_dir)

  extract_block <- function(dir) {
    lines <- readLines(file.path(dir, "ARD_Out_01.R"))
    start <- grep("Format results per ARS resultPattern", lines)
    expect_length(start, 1)
    lines[start:length(lines)]
  }

  expect_identical(extract_block(out_json), extract_block(out_xlsx))
})
