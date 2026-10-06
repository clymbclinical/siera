# Wrapped-vs-expanded parity (#206) ---------------------------------------
#
# The two code styles of readARS() must produce IDENTICAL ARDs: "wrapped" only
# moves the ID-linking boilerplate (identifier stamp, group[n]_groupId lookup,
# *_level coercion) into siera::ars_stamp(). Every runnable example ARS file is
# generated in both styles, sourced, and the ARDs compared with
# expect_identical().

# Source a generated script into an isolated env; returns the env, or an error
# message string when the script cannot run (missing ADaM, package mismatch...).
.source_script <- function(path) {
  env <- new.env(parent = baseenv())
  err <- tryCatch(
    {
      suppressWarnings(suppressMessages(source(path, local = env)))
      NULL
    },
    error = function(e) conditionMessage(e)
  )
  list(env = env, error = err)
}

# cards' fmt_fun list-column holds closures created per run, so two runs of the
# very same script are never identical() on it; the final ARD drops it, so drop
# it from the per-analysis data frames too before comparing.
.drop_fmt <- function(x) {
  dplyr::select(x, -dplyr::any_of(c("fmt_fun", "fmt_fn")))
}

# Generate both styles for one ARS file and compare every output's ARD (and the
# per-analysis df3_* objects). Returns the number of outputs actually compared.
.expect_style_parity <- function(ars_name) {
  adam <- dirname(ARS_example("ADSL.csv"))
  ars <- ARS_example(ars_name)

  out_w <- withr::local_tempdir(.local_envir = parent.frame())
  out_e <- withr::local_tempdir(.local_envir = parent.frame())
  suppressWarnings(suppressMessages(
    readARS(ars, out_w, adam, code_style = "wrapped")
  ))
  suppressWarnings(suppressMessages(
    readARS(ars, out_e, adam, code_style = "expanded")
  ))

  scripts <- list.files(out_w, pattern = "^ARD_.*\\.R$")
  expect_gt(length(scripts), 0L)
  expect_setequal(scripts, list.files(out_e, pattern = "^ARD_.*\\.R$"))

  compared <- 0L
  for (s in scripts) {
    w <- .source_script(file.path(out_w, s))
    e <- .source_script(file.path(out_e, s))

    if (!is.null(e$error)) {
      # The reference (expanded) script cannot run in this environment (e.g. an
      # ADaM file is not bundled, or the installed cards/cardx versions
      # mismatch), so there is nothing to compare this output against. The
      # wrapped script must not do better than the reference.
      next
    }
    expect_null(w$error, label = paste(ars_name, s, "wrapped script error"))

    # per-analysis ARDs (those the expanded script produced)
    for (nm in grep("^df3_", ls(e$env), value = TRUE)) {
      expect_identical(
        .drop_fmt(w$env[[nm]]), .drop_fmt(e$env[[nm]]),
        label = paste(ars_name, s, nm)
      )
    }
    # the combined, formatted ARD
    expect_true(exists("ARD", envir = w$env) && exists("ARD", envir = e$env))
    expect_identical(w$env$ARD, e$env$ARD, label = paste(ars_name, s, "ARD"))
    compared <- compared + 1L
  }
  compared
}

parity_examples <- c(
  "Common_Safety_Displays_cards.xlsx",
  "exampleARS_1.json",
  "exampleARS_2.json",
  "exampleARS_2.xlsx",
  "exampleARS_3.json",
  "exampleARS_3.xlsx",
  "exampleARS_4.json",
  "exampleARS_5.json",
  "exampleARS_5.xlsx",
  "exampleARS_5_documentref.json",
  "exampleARS_5_documentref.xlsx",
  "exampleARS_6.json",
  "exampleARS_6.xlsx"
)

for (ars_name in parity_examples) {
  local({
    nm <- ars_name
    test_that(paste("wrapped and expanded styles give identical ARDs:", nm), {
      skip_on_cran()
      for (pkg in c("cards", "cardx", "broom", "parameters", "readr", "tidyr")) {
        skip_if_not_installed(pkg)
      }
      compared <- .expect_style_parity(nm)
      # Examples whose every output needs unavailable data/packages are skipped
      # rather than silently passing without a comparison.
      if (compared == 0L) {
        skip(paste("no output of", nm, "can run in this environment"))
      }
    })
  })
}

# eTFL parity: predefined + population-based (t06) and data-driven (t13) --------

for (tbl in c("fda-ae-t06", "fda-ae-t13")) {
  local({
    tb <- tbl
    test_that(paste("eTFL", tb, "wrapped and expanded styles give identical ARDs"), {
      skip_on_cran()
      # (sourcing the AE scripts emits prop.test() small-count warnings)
      ard_wrapped <- suppressWarnings(
        .run_etfl_pipeline(tb, withr::local_tempdir(), code_style = "wrapped")
      )
      ard_expanded <- suppressWarnings(
        .run_etfl_pipeline(tb, withr::local_tempdir(), code_style = "expanded")
      )

      expect_gt(nrow(ard_wrapped), 0L)
      expect_identical(ard_wrapped, ard_expanded)
    })
  })
}

test_that("an empty data subset yields the same stub row in both styles", {
  skip_on_cran()
  for (pkg in c("cards", "readr", "tidyr")) skip_if_not_installed(pkg)

  # An ARS file whose data subset matches nothing: the method is skipped and
  # both styles must emit the statistics-free stub row.
  ars <- jsonlite::fromJSON(ARS_example("exampleARS_6.json"), simplifyVector = FALSE)
  expect_gt(length(ars$dataSubsets), 0L)
  for (i in seq_along(ars$dataSubsets)) {
    cond <- ars$dataSubsets[[i]]$condition
    if (!is.null(cond)) {
      ars$dataSubsets[[i]]$condition$value <- list("__NO_SUCH_VALUE__")
      ars$dataSubsets[[i]]$condition$comparator <- "EQ"
    }
  }
  ars_file <- file.path(withr::local_tempdir(), "empty_subset.json")
  jsonlite::write_json(ars, ars_file, auto_unbox = TRUE)

  adam <- dirname(ARS_example("ADSL.csv"))
  res <- lapply(c("wrapped", "expanded"), function(st) {
    out <- withr::local_tempdir()
    suppressWarnings(suppressMessages(readARS(ars_file, out, adam, code_style = st)))
    .source_script(file.path(out, "ARD_Out_01.R"))
  })
  skip_if(!is.null(res[[2]]$error), "expanded reference script cannot run here")

  expect_null(res[[1]]$error)
  expect_identical(res[[1]]$env$ARD, res[[2]]$env$ARD)
})
