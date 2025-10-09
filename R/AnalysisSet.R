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
    dplyr::select(condition_dataset) |>
    as.character()
  cond_adam <- cond_adam[1]

  cond_var <- temp_AnSet |>
    dplyr::select(condition_variable) |>
    as.character()
  cond_var <- cond_var[1]

  cond_oper <- temp_AnSet |>
    dplyr::select(condition_comparator) |>
    as.character()
  cond_oper <- cond_oper[1]

  cond_val <- temp_AnSet |>
    dplyr::select(condition_value) |>
    unlist()

  cond_val <- cond_val[1]

  anSetName <- temp_AnSet |>
    dplyr::select(name) |>
    as.character()
  anSetName <- anSetName[1]

  oper <- dplyr::case_when(
    cond_oper == "EQ" ~ "==",
    cond_oper == "NE" ~ "!=",
    cond_oper == "GE" ~ ">=",
    cond_oper == "GT" ~ ">",
    cond_oper == "LE" ~ "<=",
    cond_oper == "LT" ~ "<",
    TRUE ~ cond_oper
  )

  if (is.na(cond_val)) {
    cond_val <- ""
  } else if (!is.numeric(cond_val)) {
    cond_val <- paste0(cond_val)
  }

  Anas_2 <- anas[3, ]$listItem_analysisId
  Anas_s2 <- analyses %>%
    dplyr::filter(id == Anas_2)

  ana_adam2 <- Anas_s2$dataset

  if (isTRUE(cond_adam == ana_adam2)) {
    template <- "
# Apply Analysis Set ---
df_pop <- dplyr::filter(ADaM,
            var operator 'value')
df_poptot <- df_pop
"

    code <- gsub("ADaM", cond_adam, template)
    code <- gsub("var", cond_var, code)
    code <- gsub("operator", oper, code)
    code <- gsub("value", cond_val, code)
    code <- gsub("analysisidhere", analysis_id, code)
    code <- gsub("Analysissetnamehere", anSetName, code)
  } else {
    template <- "
# Apply Analysis Set ---
overlap <- intersect(names(ADaM), names(analysisADAMhere))
overlapfin <- setdiff(overlap, 'USUBJID')
df_pop <- dplyr::filter(ADaM,
            var operator 'value') |>
            merge(analysisADAMhere |> dplyr::select(-dplyr::all_of(overlapfin)),
                  by = 'USUBJID',
                  all = FALSE)
df_poptot = dplyr::filter(ADaM,
            var operator 'value')
"

    code <- gsub("ADaM", cond_adam, template)
    code <- gsub("var", cond_var, code)
    code <- gsub("operator", oper, code)
    code <- gsub("value", cond_val, code)
    code <- gsub("analysisidhere", analysis_id, code)
    code <- gsub("analysisADAMhere", ana_adam2, code)
    code <- gsub("Analysissetnamehere", anSetName, code)
  }

  list(
    code = code,
    data_subset = "df_poptot"
  )
}
