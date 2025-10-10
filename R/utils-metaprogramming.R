# Internal utilities supporting metaprogramming helpers

.expr_to_code <- function(expr, indent = 0L) {
  rlang::check_installed("rlang")
  stopifnot(rlang::is_bare_integer(indent, n = 1L), indent >= 0L)

  indent_string <- if (indent > 0L) paste(rep(" ", indent), collapse = "") else ""
  lines <- rlang::expr_deparse(expr, width = Inf)
  paste(paste0(indent_string, lines), collapse = "\n")
}

.render_template <- function(template, ..., .envir = parent.frame()) {
  rlang::check_installed("glue")
  glue::glue(
    template,
    ...,
    .envir = .envir,
    .trim = FALSE
  )
}

.combine_condition_expressions <- function(exprs, operator) {
  exprs <- Filter(Negate(is.null), exprs)

  if (length(exprs) == 0) {
    return(NULL)
  }

  if (length(exprs) == 1 || is.null(operator) || is.na(operator)) {
    return(exprs[[1]])
  }

  Reduce(
    function(lhs, rhs) {
      rlang::call2(as.name(operator), lhs, rhs)
    },
    exprs[-1],
    init = exprs[[1]]
  )
}

.normalise_condition_values <- function(value, comparator, file_ext) {
  if (is.list(value)) {
    value <- unlist(value, recursive = TRUE, use.names = FALSE)
  }

  if (length(value) == 0 || all(is.na(value))) {
    return(character())
  }

  value <- value[!is.na(value)]
  value <- as.character(value)

  if (identical(file_ext, "xlsx")) {
    value <- gsub("\\|", ",", value)
  }

  value <- trimws(value)

  if (identical(comparator, "IN") && length(value) == 1L) {
    value <- unlist(strsplit(value, ",\\s*"), use.names = FALSE)
    value <- trimws(value)
  }

  value
}

.is_blank_condition_value <- function(values) {
  if (length(values) == 0) {
    return(TRUE)
  }

  all(values == "" | values == "NA" | is.na(values))
}

.coerce_condition_vector <- function(values) {
  numeric_values <- suppressWarnings(as.numeric(values))

  if (!any(is.na(numeric_values))) {
    return(numeric_values)
  }

  values
}

.coerce_condition_scalar <- function(value) {
  if (length(value) == 0) {
    return(NA_character_)
  }

  candidate <- value[[1]]
  numeric_value <- suppressWarnings(as.numeric(candidate))

  if (!is.na(numeric_value)) {
    return(numeric_value)
  }

  candidate
}

.build_data_subset_condition <- function(row, file_ext) {
  variable <- row$condition_variable
  comparator <- row$condition_comparator
  values <- .normalise_condition_values(row$condition_value, comparator, file_ext)

  var_sym <- rlang::sym(variable)

  if (identical(comparator, "NE") && .is_blank_condition_value(values)) {
    return(rlang::expr(!is.na(!!var_sym) & !!var_sym != ""))
  }

  if (identical(comparator, "IN")) {
    values_vec <- .coerce_condition_vector(values)

    if (length(values_vec) == 0) {
      return(NULL)
    }

    return(rlang::call2(quote(`%in%`), var_sym, rlang::expr(!!values_vec)))
  }

  operator <- switch(
    comparator,
    EQ = "==",
    NE = "!=",
    GT = ">",
    GE = ">=",
    LT = "<",
    LE = "<=",
    comparator
  )

  value_scalar <- .coerce_condition_scalar(values)
  rlang::call2(as.name(operator), var_sym, rlang::expr(!!value_scalar))
}

.build_subset_filter_expressions <- function(subsetrule, file_ext) {
  if (nrow(subsetrule) == 0) {
    return(list())
  }

  if (!"level" %in% names(subsetrule) || all(is.na(subsetrule$level))) {
    conds <- lapply(
      seq_len(nrow(subsetrule)),
      function(idx) .build_data_subset_condition(subsetrule[idx, , drop = FALSE], file_ext)
    )
    return(Filter(Negate(is.null), conds))
  }

  levels <- subsetrule$level
  levels <- levels[!is.na(levels)]

  if (length(levels) == 0) {
    conds <- lapply(
      seq_len(nrow(subsetrule)),
      function(idx) .build_data_subset_condition(subsetrule[idx, , drop = FALSE], file_ext)
    )
    return(Filter(Negate(is.null), conds))
  }

  max_level <- max(levels)

  if (max_level <= 1) {
    conds <- lapply(
      seq_len(nrow(subsetrule)),
      function(idx) .build_data_subset_condition(subsetrule[idx, , drop = FALSE], file_ext)
    )
    return(Filter(Negate(is.null), conds))
  }

  filter_exprs <- list()

  for (m in seq_len(max_level - 1)) {
    current_operator <- subsetrule |>
      dplyr::filter(level == m, !is.na(compoundExpression_logicalOperator)) |>
      dplyr::pull(compoundExpression_logicalOperator)

    current_operator <- current_operator[current_operator %in% c("AND", "OR")]
    operator_symbol <- switch(
      current_operator[1],
      AND = "&",
      OR = "|",
      NULL
    )

    level_rows <- subsetrule |>
      dplyr::filter(level == m + 1, is.na(compoundExpression_logicalOperator))

    if (nrow(level_rows) == 0) {
      next
    }

    level_conditions <- lapply(
      seq_len(nrow(level_rows)),
      function(idx) .build_data_subset_condition(level_rows[idx, , drop = FALSE], file_ext)
    )

    combined <- .combine_condition_expressions(level_conditions, operator_symbol)

    if (!is.null(combined)) {
      filter_exprs <- append(filter_exprs, list(combined))
    }
  }

  if (length(filter_exprs) == 0) {
    conds <- lapply(
      seq_len(nrow(subsetrule)),
      function(idx) .build_data_subset_condition(subsetrule[idx, , drop = FALSE], file_ext)
    )
    filter_exprs <- Filter(Negate(is.null), conds)
  }

  filter_exprs
}
