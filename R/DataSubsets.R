#' Build a data subset condition
#'
#' Internal helper that translates ARS data-subset metadata into a filter
#' expression suitable for inclusion in generated R code. Handles comparator
#' translation, type coercion, and workbook-specific formatting differences.
#' @param variable Variable name used in the subset definition.
#' @param comparator Comparison operator from the metadata.
#' @param value Value(s) associated with the comparator.
#' @param file_ext Extension of the source ARS file, used to normalise parsing.
#'
#' @return Character string representing the filter expression to apply.
#' @keywords internal
.generate_data_subset_condition <- function(variable,
                                            comparator,
                                            value,
                                            file_ext) {
  if (is.null(variable) || is.na(variable)) {
    return("")
  }

  comparator <- ifelse(is.null(comparator), "", comparator)
  value_vector <- unlist(value)

  if (identical(file_ext, "xlsx")) {
    value_vector <- gsub("\\|", ",", as.character(value_vector))
  }

  if (identical(comparator, "IN")) {
    if (identical(file_ext, "xlsx")) {
      value_vector <- strsplit(value_vector[1], ",\\s*")[[1]]
    }

    if (length(value_vector) == 0) {
      value_vector <- character()
    }

    numeric_values <- suppressWarnings(as.numeric(value_vector))
    is_numeric <- !is.na(numeric_values)

    if (length(is_numeric) > 0 && is_numeric[1]) {
      value_string <- paste0(
        "c(",
        paste0(numeric_values[is_numeric], collapse = ", "),
        ")"
      )
    } else {
      value_string <- paste0(
        "c(",
        paste0("'", value_vector, "'", collapse = ", "),
        ")"
      )
    }

    return(paste0(variable, " %in% ", value_string))
  }

  operator <- dplyr::case_when(
    comparator == "EQ" ~ "==",
    comparator == "NE" ~ "!=",
    comparator == "GT" ~ ">",
    comparator == "GE" ~ ">=",
    comparator == "LT" ~ "<",
    comparator == "LE" ~ "<=",
    TRUE ~ comparator
  )

  value_string <- as.character(value_vector)[1]
  numeric_value <- suppressWarnings(as.numeric(value_string))

  if (!is.na(numeric_value)) {
    formatted_value <- numeric_value
  } else {
    formatted_value <- paste0("'", value_string, "'")
  }

  blank_value <- suppressWarnings(as.character(value_string))[1]

  if (
    !is.null(comparator) && !is.na(comparator) && comparator == "NE" &&
      !is.null(blank_value) && (identical(blank_value, "") || identical(blank_value, "NA") || is.na(blank_value))
  ) {
    return(paste0("!is.na(", variable, ") & ", variable, "!= ''"))
  }

  paste0(variable, " ", operator, " ", formatted_value)
}

#' Generate data subset code
#'
#' Internal helper that assembles the code required to apply a data subset for a
#' given analysis. The function prepares default code, augments it with
#' conditional filters when required, and returns metadata describing the subset
#' that was applied.
#' @param data_subsets DataSubsets metadata for the reporting event.
#' @param subset_id Identifier of the subset tied to the current analysis.
#' @param analysis_id Identifier of the analysis for which code is generated.
#' @param analysis_set_dataset Dataset name produced by the analysis set step.
#' @param file_ext Extension of the source ARS metadata file (json or xlsx).
#'
#' @return A list containing the generated code, subset name, and filter
#'   expression.
#' @keywords internal
.generate_data_subset_code <- function(data_subsets,
                                       subset_id,
                                       analysis_id,
                                       analysis_set_dataset,
                                       file_ext) {
  default_code <- paste0(
    "\n#Apply Data Subset ---\n",
    "df2_", analysis_id, " <- ", analysis_set_dataset, "\n\n"
  )

  result <- list(
    code = default_code,
    subset_name = NA_character_,
    filter_expression = NULL
  )

  if (is.null(data_subsets)) {
    return(result)
  }

  if (is.na(subset_id)) {
    return(result)
  }

  subsetrule <- data_subsets %>%
    dplyr::filter(id == subset_id)

  if (nrow(subsetrule) == 0) {
    return(result)
  }

  subset_name <- subsetrule %>%
    dplyr::select(name) %>%
    unique() %>%
    as.character() %>%
    gsub("[\r\n]", " ", .)

  result$subset_name <- subset_name[1]

  if (nrow(subsetrule) == 1) {
    variable <- subsetrule$condition_variable
    comparator <- subsetrule$condition_comparator
    value <- stringr::str_trim(subsetrule$condition_value)

    filter_expression <- .generate_data_subset_condition(
      variable,
      comparator,
      value,
      file_ext
    )
  } else {
    maxlev <- max(subsetrule$level)

    if (maxlev <= 1) {
      cli::cli_abort(
        "Metadata issue in DataSubsets {subset_id}: DataSubset levels not incrementing"
      )
    }

    filter_expressions <- character()

    for (m in seq_len(maxlev - 1)) {
      log_oper <- subsetrule %>%
        dplyr::filter(
          level == m,
          !is.na(compoundExpression_logicalOperator)
        ) %>%
        dplyr::select(compoundExpression_logicalOperator) %>%
        as.character()

      if (identical(log_oper, "character(0)")) {
        log_oper <- NA_character_
      }

      rlog_oper <- dplyr::case_when(
        log_oper == "AND" ~ "&",
        log_oper == "OR" ~ "|",
        TRUE ~ NA_character_
      )

      lev <- subsetrule %>%
        dplyr::filter(
          level == m + 1,
          is.na(compoundExpression_logicalOperator)
        )

      if (nrow(lev) == 0) {
        next
      }

      level_expressions <- vapply(
        seq_len(nrow(lev)),
        function(idx) {
          ord1_ <- lev[idx, ]

          .generate_data_subset_condition(
            ord1_$condition_variable,
            ord1_$condition_comparator,
            ord1_$condition_value,
            file_ext
          )
        },
        character(1)
      )

      level_expressions <- level_expressions[level_expressions != ""]

      if (length(level_expressions) == 0) {
        next
      }

      if (!is.na(rlog_oper) && length(level_expressions) > 1) {
        filter_expressions <- c(
          filter_expressions,
          paste(level_expressions, collapse = paste0(" ", rlog_oper, " "))
        )
      } else {
        filter_expressions <- c(filter_expressions, level_expressions[1])
      }
    }

    if (length(filter_expressions) > 1) {
      filter_expression <- paste(filter_expressions, collapse = ", ")
    } else if (length(filter_expressions) == 1) {
      filter_expression <- filter_expressions
    } else {
      filter_expression <- ""
    }
  }

  if (identical(filter_expression, "") || is.null(filter_expression)) {
    return(result)
  }

  result$filter_expression <- filter_expression

  result$code <- paste0(
    "# Apply Data Subset ---\n",
    "# Data subset: ", result$subset_name, "\n",
    "df2_", analysis_id, " <- ", analysis_set_dataset, " |>\n",
    "        dplyr::filter(", filter_expression, ")\n"
  )

  result
}
