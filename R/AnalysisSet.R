.generate_analysis_set_code <- function(j,
                                        analysis_sets,
                                        analyses,
                                        anas,
                                        analysis_set_id,
                                        analysis_id) {
  if (j != 1) {
    return(list(
      code = "",
      data_subset = "df_pop"
    ))
  }

  temp_AnSet <- analysis_sets |>
    dplyr::filter(id == analysis_set_id)

  cond_adam <- temp_AnSet |>
    dplyr::pull(condition_dataset) |>
    as.character()
  cond_adam <- cond_adam[1]

  cond_var <- temp_AnSet |>
    dplyr::pull(condition_variable) |>
    as.character()
  cond_var <- cond_var[1]

  cond_oper <- temp_AnSet |>
    dplyr::pull(condition_comparator) |>
    as.character()
  cond_oper <- cond_oper[1]

  cond_val <- temp_AnSet |>
    dplyr::pull(condition_value)
  cond_val <- cond_val[1]

  operator <- dplyr::case_when(
    cond_oper == "EQ" ~ "==",
    cond_oper == "NE" ~ "!=",
    cond_oper == "GE" ~ ">=",
    cond_oper == "GT" ~ ">",
    cond_oper == "LE" ~ "<=",
    cond_oper == "LT" ~ "<",
    TRUE ~ cond_oper
  )

  numeric_candidate <- suppressWarnings(as.numeric(cond_val))
  if (!is.na(numeric_candidate)) {
    condition_value <- numeric_candidate
  } else if (is.na(cond_val)) {
    condition_value <- ""
  } else {
    condition_value <- as.character(cond_val)
  }

  condition_expr <- rlang::call2(
    as.name(operator),
    rlang::sym(cond_var),
    rlang::expr(!!condition_value)
  )

  source_sym <- rlang::sym(cond_adam)
  filtered_dataset <- rlang::expr((!!source_sym) |> dplyr::filter(!!condition_expr))

  Anas_2 <- anas[3, ]$listItem_analysisId
  Anas_s2 <- analyses %>%
    dplyr::filter(id == Anas_2)

  ana_adam2 <- Anas_s2$dataset

  if (isTRUE(cond_adam == ana_adam2)) {
    code_lines <- c(
      "# Apply Analysis Set ---",
      .expr_to_code(rlang::expr(df_pop <- !!filtered_dataset)),
      .expr_to_code(rlang::expr(df_poptot <- df_pop))
    )
  } else {
    analysis_source_sym <- rlang::sym(ana_adam2)

    overlap_expr <- rlang::expr(
      overlap <- intersect(names(!!source_sym), names(!!analysis_source_sym))
    )
    overlapfin_expr <- rlang::expr(overlapfin <- setdiff(overlap, "USUBJID"))

    merged_expr <- rlang::expr(
      df_pop <- !!filtered_dataset |>
        merge(
          !!analysis_source_sym |>
            dplyr::select(-dplyr::all_of(overlapfin)),
          by = "USUBJID",
          all = FALSE
        )
    )

    poptot_expr <- rlang::expr(df_poptot <- !!filtered_dataset)

    code_lines <- c(
      "# Apply Analysis Set ---",
      .expr_to_code(overlap_expr),
      .expr_to_code(overlapfin_expr),
      .expr_to_code(merged_expr),
      .expr_to_code(poptot_expr)
    )
  }

  list(
    code = paste(code_lines, collapse = "\n"),
    data_subset = "df_poptot"
  )
}
