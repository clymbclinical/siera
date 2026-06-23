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
# back to the raw column name. The {datasetjson} dependency is optional and is
# guarded with requireNamespace() in the emitted code, so a user without the
# package gets a message rather than an error.
.generate_datasetjson_code <- function(output_id, output_path) {
  ds_name <- paste0(output_id, "_ARD")
  # Embed an absolute, forward-slashed path so the JSON lands beside the script
  # regardless of the user's working directory when they source it.
  out_dir <- gsub("\\\\", "/", output_path)

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
    "  # Variable labels: siera + {cards} vocabulary; unknown columns keep name.",
    "  .ds_label_for <- function(nm) {",
    "    .dict <- c(",
    '      variable = "Analysis Variable", variable_level = "Analysis Variable Level",',
    '      context = "Statistic Context", stat_name = "Statistic Name",',
    '      stat_label = "Statistic Label", stat = "Statistic Value",',
    '      fmt_fun = "Format Function", fmt_fn = "Format Function",',
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
