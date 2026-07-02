# Tests for external method-template resolution (issue #175):
# ARS codeTemplate.documentRef -> referenceDocuments -> manifest / code file.

# ---- helpers ---------------------------------------------------------------

# Write a minimal method manifest (Option B) to a tempdir and return its dir.
.write_manifest <- function(methods, dir = withr::local_tempdir(.local_envir = parent.frame())) {
  manifest <- list(
    `$schema` = "siera-method-manifest/v1",
    context   = "R (siera)",
    methods   = methods
  )
  writeLines(
    jsonlite::toJSON(manifest, auto_unbox = TRUE, pretty = TRUE, null = "null"),
    file.path(dir, "manifest.json")
  )
  dir
}

.total_n_method <- function(id = "total_n") {
  list(
    id           = id,
    operations   = list(list(order = 1, name = "n", label = "Count", resultPattern = "xx")),
    parameters   = list(
      list(name = "anavarhere",    valueSource = "ana_var",  label = "a", description = "a"),
      list(name = "groupvar1here", valueSource = "AG_var1",  label = "g", description = "g"),
      list(name = "bystmthere",    valueSource = "by_vars",  label = "b", description = "b")
    ),
    templateCode = "df3_analysisidhere <- cards::ard_tabulate(data = in_data bystmthere)"
  )
}

.ref_docs <- function(location, id = "RefDoc") {
  tibble::tibble(id = id, name = "doc", location = location)
}

# ---- URI / path helpers ----------------------------------------------------

test_that(".is_remote_uri detects schemes but not local paths", {
  expect_true(siera:::.is_remote_uri("https://example.com/m.json"))
  expect_true(siera:::.is_remote_uri("http://x/m.json"))
  expect_true(siera:::.is_remote_uri("ftp://x/m.json"))
  expect_false(siera:::.is_remote_uri("methods/m.json"))
  expect_false(siera:::.is_remote_uri("C:/methods/m.json"))
  expect_false(siera:::.is_remote_uri("/abs/m.json"))
})

test_that(".is_absolute_path recognises POSIX and Windows absolutes", {
  expect_true(siera:::.is_absolute_path("/abs/m.json"))
  expect_true(siera:::.is_absolute_path("C:/m.json"))
  expect_true(siera:::.is_absolute_path("C:\\m.json"))
  expect_false(siera:::.is_absolute_path("rel/m.json"))
  expect_false(siera:::.is_absolute_path("m.json"))
})

test_that(".split_page_names splits and trims, empty for blank", {
  expect_identical(siera:::.split_page_names("total_n"), "total_n")
  expect_identical(siera:::.split_page_names("a, b ; c"), c("a", "b", "c"))
  expect_identical(siera:::.split_page_names(NA), character(0))
  expect_identical(siera:::.split_page_names(""), character(0))
  expect_identical(siera:::.split_page_names(NULL), character(0))
})

test_that(".extract_reference_documents normalises NULL, data frame and list", {
  expect_identical(nrow(siera:::.extract_reference_documents(NULL)), 0L)
  df <- data.frame(id = "RD", name = "n", location = "x.json", stringsAsFactors = FALSE)
  expect_identical(siera:::.extract_reference_documents(df)$location, "x.json")
  lst <- list(list(id = "RD", name = "n", location = "x.json"))
  expect_identical(siera:::.extract_reference_documents(lst)$id, "RD")
  # empty data frame and empty list both collapse to a 0-row tibble
  expect_identical(nrow(siera:::.extract_reference_documents(df[0, ])), 0L)
  expect_identical(nrow(siera:::.extract_reference_documents(list())), 0L)
})

test_that(".documentref_reference_id reads the i-th id or NA when absent", {
  expect_identical(
    siera:::.documentref_reference_id(list(referenceDocumentId = c("A", "B")), 2),
    "B"
  )
  expect_true(is.na(siera:::.documentref_reference_id(list(other = 1), 1)))
})

test_that(".documentref_page_names handles every documentRef shape", {
  # no pageRefs at all
  expect_identical(siera:::.documentref_page_names(list(), 1), character(0))
  # pageRefs present but the i-th entry is NULL
  expect_identical(
    siera:::.documentref_page_names(list(pageRefs = list(NULL)), 1), character(0)
  )
  # data-frame pageRefs without a pageNames column
  pr_df <- list(pageRefs = list(data.frame(refType = "x", stringsAsFactors = FALSE)))
  expect_identical(siera:::.documentref_page_names(pr_df, 1), character(0))
  # data-frame pageRefs with a pageNames list-column
  pr_ok <- list(pageRefs = list(data.frame(pageNames = I(list("total_n")))))
  expect_identical(siera:::.documentref_page_names(pr_ok, 1), "total_n")
  # list-form pageRefs (records not simplified to a data frame)
  pr_lst <- list(pageRefs = list(list(list(pageNames = "total_n"))))
  expect_identical(siera:::.documentref_page_names(pr_lst, 1), "total_n")
})

# ---- manifest resolution (Option B) ----------------------------------------

test_that("pageRefs named destination selects the right method from a catalog", {
  d <- .write_manifest(list(.total_n_method("alpha"), .total_n_method("beta")))
  res <- siera:::.resolve_method_documentref(
    "RefDoc", "beta", .ref_docs("manifest.json"), d
  )
  expect_match(res$templateCode, "ard_tabulate", fixed = TRUE)
  expect_setequal(res$parameters$valueSource, c("ana_var", "AG_var1", "by_vars"))
})

test_that("single-method manifest needs no pageRefs", {
  d <- .write_manifest(list(.total_n_method("only")))
  res <- siera:::.resolve_method_documentref("RefDoc", NULL, .ref_docs("manifest.json"), d)
  expect_match(res$templateCode, "ard_tabulate", fixed = TRUE)
})

test_that("multi-method manifest without pageRefs errors", {
  d <- .write_manifest(list(.total_n_method("a"), .total_n_method("b")))
  expect_error(
    siera:::.resolve_method_documentref("RefDoc", NULL, .ref_docs("manifest.json"), d),
    "no .*pageRefs"
  )
})

test_that("unknown method id in manifest errors", {
  d <- .write_manifest(list(.total_n_method("a")))
  expect_error(
    siera:::.resolve_method_documentref("RefDoc", "nope", .ref_docs("manifest.json"), d),
    "no method with id"
  )
})

test_that("manifest with an unsupported valueSource is rejected", {
  m <- .total_n_method("bad")
  m$parameters[[1]]$valueSource <- "trt_filter_stm"
  d <- .write_manifest(list(m))
  expect_error(
    siera:::.resolve_method_documentref("RefDoc", "bad", .ref_docs("manifest.json"), d),
    "unsupported valueSource"
  )
})

test_that("operation_N valueSources are accepted in a manifest", {
  m <- .total_n_method("ops")
  m$parameters[[1]]$valueSource <- "operation_1"
  d <- .write_manifest(list(m))
  res <- siera:::.resolve_method_documentref("RefDoc", "ops", .ref_docs("manifest.json"), d)
  expect_true("operation_1" %in% res$parameters$valueSource)
})

test_that("context defaults to R (siera) when the manifest declares none", {
  d <- withr::local_tempdir()
  m <- .total_n_method("noctx")
  # manifest with no top-level context and a method with no context
  writeLines(
    jsonlite::toJSON(list(methods = list(m)), auto_unbox = TRUE, null = "null"),
    file.path(d, "manifest.json")
  )
  res <- siera:::.resolve_method_documentref("RefDoc", "noctx", .ref_docs("manifest.json"), d)
  expect_identical(res$context, "R (siera)")
})

test_that("a method with no parameters resolves to NULL parameters", {
  m <- .total_n_method("noparams")
  m$parameters <- NULL
  d <- .write_manifest(list(m))
  res <- siera:::.resolve_method_documentref("RefDoc", "noparams", .ref_docs("manifest.json"), d)
  expect_null(res$parameters)
})

test_that("manifest method without templateCode errors", {
  m <- .total_n_method("empty")
  m$templateCode <- ""
  d <- .write_manifest(list(m))
  expect_error(
    siera:::.resolve_method_documentref("RefDoc", "empty", .ref_docs("manifest.json"), d),
    "no .*templateCode"
  )
})

test_that("manifest with no methods errors", {
  d <- withr::local_tempdir()
  writeLines(
    jsonlite::toJSON(list(`$schema` = "siera-method-manifest/v1", methods = list()),
                     auto_unbox = TRUE),
    file.path(d, "manifest.json")
  )
  expect_error(
    siera:::.resolve_method_documentref("RefDoc", NULL, .ref_docs("manifest.json"), d),
    "no .*methods"
  )
})

# ---- bare code file (Option A) ---------------------------------------------

test_that("a bare .R code file resolves to templateCode with NULL parameters", {
  d <- withr::local_tempdir()
  writeLines("df3_analysisidhere <- cards::ard_tabulate(data = in_data)",
             file.path(d, "code.R"))
  res <- siera:::.resolve_method_documentref("RefDoc", NULL, .ref_docs("code.R"), d)
  expect_match(res$templateCode, "ard_tabulate", fixed = TRUE)
  expect_null(res$parameters)
})

# ---- error paths -----------------------------------------------------------

test_that("an empty referenceDocumentId errors", {
  expect_error(
    siera:::.resolve_method_documentref("", NULL, .ref_docs("manifest.json"), tempdir()),
    "empty"
  )
})

test_that("an id not declared in referenceDocuments errors", {
  expect_error(
    siera:::.resolve_method_documentref("Missing", NULL, .ref_docs("manifest.json", id = "RefDoc"), tempdir()),
    "not declared in"
  )
})

test_that("a reference document with no location errors", {
  expect_error(
    siera:::.resolve_method_documentref("RefDoc", NULL, .ref_docs(NA_character_), tempdir()),
    "no .*location"
  )
})

test_that("a remote location is rejected", {
  expect_error(
    siera:::.resolve_method_documentref("RefDoc", NULL, .ref_docs("https://x/m.json"), tempdir()),
    "remote"
  )
})

test_that("a missing local location errors", {
  d <- withr::local_tempdir()
  expect_error(
    siera:::.resolve_method_documentref("RefDoc", NULL, .ref_docs("nope.json"), d),
    "does not exist"
  )
})

test_that("an unsupported document type errors", {
  d <- withr::local_tempdir()
  writeLines("x", file.path(d, "doc.pdf"))
  expect_error(
    siera:::.resolve_method_documentref("RefDoc", NULL, .ref_docs("doc.pdf"), d),
    "unsupported type"
  )
})

test_that("an absolute location is used as-is (not joined to ars_dir)", {
  d <- .write_manifest(list(.total_n_method("abs")))
  abs_loc <- normalizePath(file.path(d, "manifest.json"), winslash = "/")
  res <- siera:::.resolve_method_documentref(
    "RefDoc", "abs", .ref_docs(abs_loc), "C:/some/other/dir"
  )
  expect_match(res$templateCode, "ard_tabulate", fixed = TRUE)
})

# ---- end-to-end parity: documentRef vs inline produce the same script -------

# Generated scripts only; no script is sourced, so no ADaM data / cards needed.
# Both ARS variants must use the SAME adam dir (its path is embedded in the
# generated read_csv() calls). The inline xlsx path does not strip \r/_x000D_
# from templateCode, so blank lines can differ cosmetically - compare with blank
# lines (and the timestamp header) removed, asserting the logic is identical.
.gen_script_norm <- function(ars_path, adam, env = parent.frame()) {
  out <- withr::local_tempdir(.local_envir = env)
  suppressWarnings(suppressMessages(readARS(ars_path, out, adam)))
  files <- sort(list.files(out, pattern = "\\.R$"))
  lapply(files, function(f) {
    x <- readLines(file.path(out, f), warn = FALSE)
    x <- x[!grepl("^# Date created:", x)]
    x[nzchar(trimws(x))]
  })
}

test_that("JSON documentRef example generates the same script as the inline ARS", {
  adam   <- withr::local_tempdir()
  inline <- .gen_script_norm(ARS_example("exampleARS_5.json"), adam)
  docref <- .gen_script_norm(ARS_example("exampleARS_5_documentref.json"), adam)
  expect_identical(docref, inline)
})

test_that("XLSX documentRef example generates the same script as the inline ARS", {
  adam   <- withr::local_tempdir()
  inline <- .gen_script_norm(ARS_example("exampleARS_5.xlsx"), adam)
  docref <- .gen_script_norm(ARS_example("exampleARS_5_documentref.xlsx"), adam)
  expect_identical(docref, inline)
})

test_that("methods mix inline code and documentRef; empty refIds are skipped", {
  # Build a 3-method ARS where Mth_01 keeps inline code (resolution is skipped
  # for it), Mth_02 resolves from a manifest, and Mth_03 carries a documentRef
  # with an empty referenceDocumentId (also skipped, leaving its template NA).
  src <- jsonlite::fromJSON(ARS_example("exampleARS_5.json"),
                            simplifyVector = FALSE, simplifyDataFrame = FALSE)
  d <- withr::local_tempdir()
  manifest <- list(
    `$schema` = "siera-method-manifest/v1", context = "R (siera)",
    methods = list(list(
      id           = src$methods[[2]]$id,
      parameters   = src$methods[[2]]$codeTemplate$parameters,
      templateCode = src$methods[[2]]$codeTemplate$code
    ))
  )
  writeLines(jsonlite::toJSON(manifest, auto_unbox = TRUE, null = "null"),
             file.path(d, "manifest.json"))

  src$referenceDocuments <- list(list(id = "RD", name = "m", location = "manifest.json"))
  # Mth_01 left inline; Mth_02 -> manifest (and drops its inline context, so the
  # context falls back to the resolved one); Mth_03 -> empty refId (skipped) and
  # also drops its inline parameters (so it has neither inline nor resolved ones).
  src$methods[[2]]$codeTemplate$code <- NULL
  src$methods[[2]]$codeTemplate$parameters <- NULL
  src$methods[[2]]$codeTemplate$context <- NULL
  src$methods[[2]]$codeTemplate$documentRef <- list(
    referenceDocumentId = "RD",
    pageRefs = list(list(refType = "NamedDestination",
                         pageNames = list(src$methods[[2]]$id))))
  src$methods[[3]]$codeTemplate$code <- NULL
  src$methods[[3]]$codeTemplate$parameters <- NULL
  src$methods[[3]]$codeTemplate$documentRef <- list(referenceDocumentId = "")
  ars <- file.path(d, "mixed.json")
  writeLines(jsonlite::toJSON(src, auto_unbox = TRUE, null = "null"), ars)

  meta <- siera:::.read_ars_metadata(ars)
  tc <- meta$AnalysisMethodCodeTemplate
  m1 <- meta$AnalysisMethods$id[1]
  m2 <- src$methods[[2]]$id
  m3 <- src$methods[[3]]$id
  expect_true(nzchar(tc$templateCode[tc$method_id == m1]))   # inline kept
  expect_true(nzchar(tc$templateCode[tc$method_id == m2]))   # resolved
  expect_true(is.na(tc$templateCode[tc$method_id == m3]))    # empty refId skipped
  # Mth_02 dropped its inline context -> filled from the resolved manifest.
  expect_identical(tc$context[tc$method_id == m2], "R (siera)")
})

test_that("the shipped documentRef example carries no inline templateCode", {
  # Proves resolution (not a leftover inline copy) produced the script above.
  meta <- siera:::.read_ars_metadata(ARS_example("exampleARS_5_documentref.json"))
  expect_true(all(nzchar(meta$AnalysisMethodCodeTemplate$templateCode)))
  raw <- jsonlite::fromJSON(ARS_example("exampleARS_5_documentref.json"),
                            simplifyVector = FALSE, simplifyDataFrame = FALSE)
  has_inline <- vapply(raw$methods, function(m) !is.null(m$codeTemplate$code), logical(1))
  expect_false(any(has_inline))
})
