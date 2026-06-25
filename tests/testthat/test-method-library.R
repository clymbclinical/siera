# Contract tests for the plain-text method-template library
# (inst/method-library/). These guarantee every bundled recipe is structurally
# valid and references only valueSources siera can actually resolve - the class
# of checks that would have caught the stale/broken legacy R_siera_codes.xlsx
# sheets (unsupported trt_filter_stm, deprecated cards names, the conf.low/high
# case_when copy-paste bug).

.mlib_dir <- function() {
  lib <- system.file("method-library", package = "siera")
  if (is.null(lib) || identical(lib, "") || !dir.exists(lib)) {
    lib <- testthat::test_path("..", "..", "inst", "method-library")
  }
  lib
}

# Substitute placeholder tokens with syntactically valid dummy values so the
# template can be parsed. Mirrors the comma conventions of siera's real
# valueSource resolution (by_vars/strata_vars/by_stmt inject a leading comma).
.mlib_dummy <- function(value_source) {
  switch(value_source,
    ana_var           = "ANAVAR",
    AG_var1           = "TRT01A",
    AG_var2           = "AESOC",
    AG_var3           = "GRP3",
    DEN_analysisid    = "An_99",
    AG_denom_var1     = "TRT01A",
    AG_max_dataDriven = "FALSE",
    distinct_list     = "TRT01A, ANAVAR",
    by_listc          = "'TRT01A', 'AESOC'",
    by_list           = "TRT01A, AESOC",
    by_vars           = ", by = 'TRT01A', variables = 'AESOC'",
    strata_vars       = ", strata = 'TRT01A', variables = 'AESOC'",
    by_stmt           = ", by = TRT01A",
    stop("no dummy defined for valueSource ", value_source)
  )
}

.mlib_substitute <- function(code, params) {
  for (i in seq_len(nrow(params))) {
    code <- gsub(params$name[i], .mlib_dummy(params$valueSource[i]), code, fixed = TRUE)
  }
  # Internal tokens siera substitutes automatically. opidNhere already appears
  # quoted in templates, so substitute the bare id (no added quotes).
  code <- gsub("opid([0-9]+)here", "op_\\1", code)
  code <- gsub("analysisidhere", "An_X", code, fixed = TRUE)
  code <- gsub("methodidhere", "Mth_X", code, fixed = TRUE)
  code <- gsub("outputidhere", "Out_X", code, fixed = TRUE)
  code
}

.mlib_methods <- function() {
  lib <- .mlib_dir()
  dirs <- list.dirs(lib, recursive = FALSE)
  dirs[file.exists(file.path(dirs, "method.json"))]
}

test_that("method-library directory exists and holds methods", {
  expect_true(dir.exists(.mlib_dir()))
  expect_gt(length(.mlib_methods()), 0)
})

test_that("committed METHODS.md matches the rendered catalog (no drift)", {
  md_file <- file.path(.mlib_dir(), "METHODS.md")
  expect_true(file.exists(md_file))
  expect_identical(
    readLines(md_file, warn = FALSE),
    siera:::.render_method_library_md(.mlib_dir())
  )
})

test_that("constructs.json valueSources match the .supported_value_sources() contract", {
  cons <- jsonlite::fromJSON(
    file.path(.mlib_dir(), "constructs.json"),
    simplifyVector = TRUE
  )$constructs
  documented <- cons$valueSource[cons$type != "pattern"]
  expect_setequal(documented, siera:::.supported_value_sources())
  # the operation family is documented only as a pattern entry
  expect_true("operation_N" %in% cons$valueSource[cons$type == "pattern"])
})

test_that("every method references only supported valueSources", {
  supported <- siera:::.supported_value_sources()
  op_pat <- siera:::.operation_value_source_pattern()
  for (d in .mlib_methods()) {
    meta <- jsonlite::fromJSON(file.path(d, "method.json"), simplifyVector = TRUE)
    vs <- meta$parameters$valueSource
    ok <- vs %in% supported | grepl(op_pat, vs)
    expect_true(all(ok),
      info = sprintf("%s: unsupported valueSource(s): %s",
                     meta$id, paste(vs[!ok], collapse = ", ")))
  }
})

test_that("every template parses once placeholders are substituted", {
  for (d in .mlib_methods()) {
    meta <- jsonlite::fromJSON(file.path(d, "method.json"), simplifyVector = TRUE)
    code <- paste(readLines(file.path(d, "template.R"), warn = FALSE), collapse = "\n")
    sub  <- .mlib_substitute(code, meta$parameters)
    expect_silent(parse(text = sub))
  }
})

test_that("templates assign df3 and have no orphan params or unknown placeholders", {
  for (d in .mlib_methods()) {
    meta <- jsonlite::fromJSON(file.path(d, "method.json"), simplifyVector = TRUE)
    code <- paste(readLines(file.path(d, "template.R"), warn = FALSE), collapse = "\n")

    expect_match(code, "df3_analysisidhere", fixed = TRUE,
      info = sprintf("%s: template must assign df3_analysisidhere", meta$id))

    # No orphan parameters: every declared token must appear in the template.
    for (p in meta$parameters$name) {
      expect_true(grepl(p, code, fixed = TRUE),
        info = sprintf("%s: declared parameter %s not used in template", meta$id, p))
    }

    # No unknown `...here` placeholder tokens. A token is known if it carries an
    # internal marker (analysisidhere/methodidhere/outputidhere/opidNhere) or
    # contains a declared parameter name (covers prefixed forms like
    # df2_denomanaidhere where the param is a suffix siera substitutes).
    toks <- unique(unlist(regmatches(code, gregexpr("[A-Za-z_][A-Za-z0-9_.]*here", code))))
    is_internal <- grepl("analysisidhere|methodidhere|outputidhere", toks) |
                   grepl("^opid[0-9]+here$", toks)
    is_param <- vapply(toks, function(tk) {
      any(vapply(meta$parameters$name, function(p) grepl(p, tk, fixed = TRUE), logical(1)))
    }, logical(1))
    unknown <- toks[!(is_internal | is_param)]
    expect_length(unknown, 0)
  }
})

test_that("no case_when maps the same stat_name twice (conf.low/high bug class)", {
  for (d in .mlib_methods()) {
    meta <- jsonlite::fromJSON(file.path(d, "method.json"), simplifyVector = TRUE)
    code <- paste(readLines(file.path(d, "template.R"), warn = FALSE), collapse = "\n")
    lhs <- unlist(regmatches(
      code,
      gregexpr("stat_name\\s*==\\s*['\"][^'\"]+['\"]", code)
    ))
    dups <- lhs[duplicated(lhs)]
    expect_length(dups, 0)
  }
})

test_that("operation IDs referenced do not exceed declared operations", {
  for (d in .mlib_methods()) {
    meta <- jsonlite::fromJSON(file.path(d, "method.json"), simplifyVector = TRUE)
    code <- paste(readLines(file.path(d, "template.R"), warn = FALSE), collapse = "\n")
    refs <- as.integer(unlist(regmatches(code, gregexpr("(?<=opid)[0-9]+(?=here)", code, perl = TRUE))))
    if (length(refs) > 0) {
      expect_true(max(refs) <= nrow(meta$operations),
        info = sprintf("%s: references opid%d but only %d operations declared",
                       meta$id, max(refs), nrow(meta$operations)))
    }
  }
})
