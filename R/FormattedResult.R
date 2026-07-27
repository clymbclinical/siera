# Generate the R code that adds formatted-result columns to a finished ARD.
#
# The ARS metadata attaches a resultPattern (e.g. "(N=XX)", "XX.X", "(XX.XX)")
# to every method operation, describing how the raw statistic should be
# displayed in the final table. siera is a code generator, so the formatting
# has to happen at script runtime, once the ARD exists: the block returned by
# .generate_formatted_result_code() is appended to each ARD_<Output>.R script
# and, when sourced, adds three columns matching the CDISC eTFL reference ARD
# layout:
#   res     - the raw statistic flattened to numeric
#   pattern - the ARS resultPattern for the row's operation
#   disp    - the printable, table-ready formatted string
# It also drops {cards}' internal fmt_fun/fmt_fn list-columns, which hold R
# format *functions* and are not printable (issue #167).
#
# {cards} reports proportions on the 0-1 scale under stat_name == "p"; result
# patterns express percentages, so those rows are multiplied by 100 before
# formatting. p-values arrive as stat_name == "p.value" and are left as-is.

# Scalar, base-R formatter: render one numeric value per one ARS resultPattern.
# The X-run marks where the number goes; any prefix/suffix around it (parens,
# "N=", leading spaces) is preserved. Decimals = number of X after the ".".
# A pattern with no X at all (e.g. "(N=)") gets the number inserted before a
# trailing ")" if present, else appended.
.format_ars_result <- function(value, pattern) {
  if (is.null(pattern) || length(pattern) != 1L || is.na(pattern) ||
      !nzchar(pattern)) {
    return(NA_character_)
  }
  if (!is.numeric(value)) {
    value <- suppressWarnings(as.numeric(value))
  }
  if (is.null(value) || length(value) != 1L || is.na(value) ||
      !is.finite(value)) {
    return(NA_character_)
  }
  dec <- 0L
  dec_match <- regmatches(pattern, regexpr("\\.X+", pattern))
  if (length(dec_match) == 1L) {
    dec <- nchar(dec_match) - 1L
  }
  num <- formatC(round(value, dec), format = "f", digits = dec)
  x_pos <- gregexpr("X", pattern, fixed = TRUE)[[1L]]
  if (x_pos[1L] != -1L) {
    prefix <- substr(pattern, 1L, x_pos[1L] - 1L)
    suffix <- substr(pattern, x_pos[length(x_pos)] + 1L, nchar(pattern))
    return(paste0(prefix, num, suffix))
  }
  if (endsWith(pattern, ")")) {
    return(paste0(substr(pattern, 1L, nchar(pattern) - 1L), num, ")"))
  }
  paste0(pattern, num)
}

# Build the self-contained runtime block appended to each generated script.
# The operationid -> pattern lookup is embedded at generation time from the
# AnalysisMethods tibble (identical for JSON- and XLSX-sourced ARS files);
# .format_ars_result() itself is deparse()'d in so the generated script needs
# no siera at runtime, and the package function stays the single source of
# truth.
.generate_formatted_result_code <- function(analysis_methods) {
  ops <- data.frame(
    operationid = character(0), pattern = character(0),
    stringsAsFactors = FALSE
  )
  if (!is.null(analysis_methods) &&
      all(c("operation_id", "operation_resultPattern") %in%
            names(analysis_methods))) {
    ops <- data.frame(
      operationid = as.character(analysis_methods$operation_id),
      pattern     = as.character(analysis_methods$operation_resultPattern),
      stringsAsFactors = FALSE
    )
    ops <- ops[!is.na(ops$operationid) & nzchar(ops$operationid) &
                 !is.na(ops$pattern) & nzchar(ops$pattern), , drop = FALSE]
    ops <- ops[!duplicated(ops$operationid), , drop = FALSE]
  }

  if (nrow(ops) == 0L) {
    op_lookup_lines <- paste0(
      ".op_patterns <- data.frame(operationid = character(0), ",
      "pattern = character(0), stringsAsFactors = FALSE)"
    )
  } else {
    op_lookup_lines <- c(
      ".op_patterns <- data.frame(",
      "  operationid = c(",
      paste0("    ", .quote_csv_lines(ops$operationid), collapse = "\n"),
      "  ),",
      "  pattern = c(",
      paste0("    ", .quote_csv_lines(ops$pattern), collapse = "\n"),
      "  ),",
      "  stringsAsFactors = FALSE",
      ")"
    )
  }

  fmt_fun_lines <- deparse(.format_ars_result)
  fmt_fun_lines[1] <- paste0(".format_ars_result <- ", fmt_fun_lines[1])

  lines <- c(
    "",
    "",
    "# Format results per ARS resultPattern (res / pattern / disp) ----",
    fmt_fun_lines,
    "",
    op_lookup_lines,
    "",
    'if ("stat" %in% names(ARD)) {',
    "  ARD$res <- if (is.list(ARD$stat)) {",
    "    vapply(ARD$stat, function(v)",
    "      if (is.null(v) || length(v) == 0) NA_real_",
    "      else suppressWarnings(as.numeric(v[[1]])),",
    "      numeric(1))",
    "  } else {",
    "    suppressWarnings(as.numeric(ARD$stat))",
    "  }",
    "} else {",
    "  ARD$res <- NA_real_",
    "}",
    'if ("operationid" %in% names(ARD)) {',
    '  ARD <- dplyr::left_join(ARD, .op_patterns, by = "operationid")',
    "} else {",
    "  ARD$pattern <- NA_character_",
    "}",
    "",
    "# {cards} proportions (stat_name == 'p') are 0-1; patterns expect percent.",
    ".fmt_val <- ARD$res",
    'if ("stat_name" %in% names(ARD)) {',
    '  .is_prop <- !is.na(ARD$stat_name) & ARD$stat_name == "p"',
    "  .fmt_val[.is_prop] <- ARD$res[.is_prop] * 100",
    "}",
    "ARD$disp <- vapply(seq_len(nrow(ARD)), function(.i)",
    "  .format_ars_result(.fmt_val[.i], ARD$pattern[.i]), character(1))",
    "",
    "# Drop {cards}' internal format-function list-columns (not printable).",
    'ARD <- dplyr::select(ARD, -dplyr::any_of(c("fmt_fun", "fmt_fn")))',
    ""
  )

  paste(lines, collapse = "\n")
}

# Render a character vector as quoted, comma-separated code lines (one value
# per line, trailing comma on all but the last) for embedding in generated
# data.frame() calls.
.quote_csv_lines <- function(x) {
  quoted <- paste0('"', gsub('"', '\\\\"', x), '"')
  paste0(quoted, c(rep(",", length(x) - 1L), ""))
}
