# Generate the R code that exports a finished ARD as a CDISC Dataset-JSON file.
#
# siera is a code generator: the ARD does not exist until the user sources the
# generated script, and its column set is only known at that point (it depends
# on the analysis type and the data-driven groupings discovered at runtime).
# This helper therefore returns a self-contained block of R code that is
# appended to the generated ARD_<Output>.R script. When that script runs, the
# block introspects the just-built `ARD` object, derives the column metadata
# Dataset-JSON requires, and writes ARD_<Output>.json next to the script.
#
# The variable-label dictionary covers siera's own ARD vocabulary plus the
# {cards} columns siera builds on; data-driven ADaM columns (e.g. TRT01A) fall
# back to the raw column name. User-supplied `column_labels` overrides (#165)
# sit at the front of that precedence chain; they are deparse()'d into the
# block so the generated script keeps no runtime dependency on siera. The
# {datasetjson} dependency is optional and is guarded with requireNamespace()
# in the emitted code, so a user without the package gets a message rather
# than an error.
.generate_datasetjson_code <- function(output_id, output_path,
                                       column_labels = NULL) {
  ds_name <- paste0(output_id, "_ARD")
  # Embed an absolute, forward-slashed path so the JSON lands beside the script
  # regardless of the user's working directory when they source it.
  out_dir <- gsub("\\\\", "/", output_path)

  # Overrides can only be checked against the ARD's columns at script runtime,
  # so an unmatched name warns there (likely a typo) instead of erroring here.
  # Without overrides nothing is emitted, keeping the default block unchanged.
  override_lines <- character(0)
  override_lookup <- character(0)
  if (!is.null(column_labels)) {
    labels <- stats::setNames(as.character(column_labels), names(column_labels))
    override_lines <- c(
      "  # User-supplied label overrides (readARS(column_labels = ...)).",
      paste0(
        "  .ds_label_overrides <- ",
        paste(deparse(labels, width.cutoff = 500L), collapse = "")
      ),
      "  .ds_unknown <- setdiff(names(.ds_label_overrides), names(.ds_df))",
      "  if (length(.ds_unknown) > 0) {",
      "    warning(",
      '      "column_labels name(s) not found in the ARD and ignored: ",',
      '      paste(.ds_unknown, collapse = ", "),',
      "      call. = FALSE",
      "    )",
      "  }",
      ""
    )
    override_lookup <- paste0(
      "    if (nm %in% names(.ds_label_overrides)) ",
      "return(unname(.ds_label_overrides[nm]))"
    )
  }

  lines <- c(
    "",
    "",
    "# Export ARD as CDISC Dataset-JSON ----",
    'if (requireNamespace("datasetjson", quietly = TRUE)) {',
    "  .ds_df <- ARD",
    "",
    "  # Dataset-JSON columns must be atomic; flatten {cards} list-columns.",
    "  for (.nm in names(.ds_df)) {",
    "    if (is.list(.ds_df[[.nm]])) {",
    '      if (.nm == "stat") {',
    "        .ds_df[[.nm]] <- vapply(.ds_df[[.nm]], function(v)",
    "          if (is.null(v) || length(v) == 0) NA_real_ else suppressWarnings(as.numeric(v[[1]])),",
    "          numeric(1))",
    "      } else {",
    "        .ds_df[[.nm]] <- vapply(.ds_df[[.nm]], function(v)",
    "          if (is.null(v) || length(v) == 0) NA_character_",
    '          else tryCatch(paste(as.character(unlist(v)), collapse = "; "),',
    "                        error = function(e) NA_character_),",
    "          character(1))",
    "      }",
    "    }",
    "  }",
    "",
    override_lines,
    "  # Variable labels: siera + {cards} vocabulary; unknown columns keep name.",
    "  .ds_label_for <- function(nm) {",
    override_lookup,
    "    .dict <- c(",
    '      variable = "Analysis Variable", variable_level = "Analysis Variable Level",',
    '      context = "Statistic Context", stat_name = "Statistic Name",',
    '      stat_label = "Statistic Label", stat = "Statistic Value",',
    '      res = "Result Value", pattern = "Result Pattern",',
    '      disp = "Formatted Result",',
    '      warning = "Warning", error = "Error",',
    '      operationid = "Operation Identifier", AnalysisId = "Analysis Identifier",',
    '      MethodId = "Method Identifier", OutputId = "Output Identifier"',
    "    )",
    "    if (nm %in% names(.dict)) return(unname(.dict[nm]))",
    '    m <- regmatches(nm, regexec("^group([0-9]+)_(groupingId|groupId|groupValue|level)$", nm))[[1]]',
    "    if (length(m) == 3) {",
    '      .sfx <- c(groupingId = "Grouping Identifier", groupId = "Group Identifier",',
    '                groupValue = "Group Value", level = "Value")',
    '      return(paste0("Group ", m[2], " ", .sfx[[m[3]]]))',
    "    }",
    '    m2 <- regmatches(nm, regexec("^group([0-9]+)$", nm))[[1]]',
    '    if (length(m2) == 2) return(paste0("Group ", m2[2], " Variable"))',
    "    nm",
    "  }",
    "",
    "  .ds_cols <- data.frame(",
    paste0('    itemOID  = paste0("IT.', ds_name, '.", names(.ds_df)),'),
    "    name     = names(.ds_df),",
    "    label    = vapply(names(.ds_df), .ds_label_for, character(1)),",
    "    dataType = vapply(.ds_df, function(x)",
    '      if (is.integer(x)) "integer" else if (is.numeric(x)) "float" else "string",',
    "      character(1)),",
    "    stringsAsFactors = FALSE",
    "  )",
    "",
    "  .ds_obj <- datasetjson::dataset_json(",
    "    .data         = .ds_df,",
    paste0('    item_oid      = "IG.', ds_name, '",'),
    paste0('    name          = "', ds_name, '",'),
    '    dataset_label = "Analysis Results Dataset",',
    "    columns       = .ds_cols",
    "  )",
    paste0('  .ds_file <- file.path("', out_dir, '", "ARD_', output_id, '.json")'),
    "  datasetjson::write_dataset_json(.ds_obj, file = .ds_file)",
    '  message("Wrote Dataset-JSON: ", .ds_file)',
    "} else {",
    '  message("Package \'datasetjson\' is not installed; skipping Dataset-JSON export.")',
    "}",
    ""
  )

  paste(lines, collapse = "\n")
}

# Validate readARS(column_labels = ...) at generation time (#165). Only the
# vector's shape can be checked here; whether each name is a real ARD column
# is checked by the generated export block at runtime.
.check_column_labels <- function(column_labels) {
  if (is.null(column_labels)) {
    return(invisible(NULL))
  }
  if (!is.character(column_labels) || length(column_labels) == 0L) {
    cli::cli_abort(c(
      "{.arg column_labels} must be a non-empty named character vector.",
      "i" = "For example {.code c(TRT01A = \"Actual Treatment\")}."
    ))
  }
  nms <- names(column_labels)
  if (is.null(nms) || anyNA(nms) || any(!nzchar(trimws(nms)))) {
    cli::cli_abort(c(
      "Every element of {.arg column_labels} must be named with an ARD column.",
      "i" = "For example {.code c(TRT01A = \"Actual Treatment\")}."
    ))
  }
  dup <- unique(nms[duplicated(nms)])
  if (length(dup) > 0L) {
    cli::cli_abort(
      "{.arg column_labels} has duplicated name{?s}: {.val {dup}}."
    )
  }
  missing_label <- nms[is.na(column_labels)]
  if (length(missing_label) > 0L) {
    cli::cli_abort(
      "{.arg column_labels} has a missing ({.val {NA}}) label for {.val {missing_label}}."
    )
  }
  invisible(column_labels)
}
