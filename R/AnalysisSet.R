#' Generate analysis set code and context
#'
#' Internal helper that builds the code needed to apply an analysis set to the
#' current analysis. The function inspects the ARS metadata to determine filter
#' conditions and returns the templated code together with the name of the
#' resulting population dataset.
#' @param j Index of the current analysis within the output loop.
#' @param analysis_sets AnalysisSets metadata for the reporting event.
#' @param analyses Analyses metadata for the reporting event.
#' @param anas The analyses tied to the current output (Lopa subset).
#' @param analysis_set_id Identifier for the analysis set used by the analysis.
#' @param analysis_id Identifier for the analysis that is being generated.
#'
#' @return A list containing the generated code and the name of the dataset that
#'   holds the filtered analysis population.
#' @keywords internal
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

  analysis_dataset <- analyses %>%
    dplyr::filter(id == analysis_id) %>%
    dplyr::select(dataset) %>%
    as.character()

  analysis_dataset <- analysis_dataset[1]

  if (is.na(analysis_dataset) || analysis_dataset %in% c("", "NA")) {
    analysis_dataset <- "df_pop"
  }

  fallback_result <- list(
    code = paste0(
      "# Apply Analysis Set ---\n",
      "df_pop <- ", analysis_dataset, "\n",
      "df_poptot <- df_pop\n"
    ),
    data_subset = "df_poptot"
  )

  if (is.null(analysis_set_id) || analysis_set_id %in% c("", "NA") || is.na(analysis_set_id)) {
    cli::cli_warn(
      "Metadata issue in Analyses {analysis_id}: Analysis is missing an analysisSetId; using the analysis dataset without filtering"
    )
    return(fallback_result)
  }

  if (is.null(analysis_sets)) {
    cli::cli_warn(
      "Metadata issue in Analyses {analysis_id}: AnalysisSets metadata not supplied; using the analysis dataset without filtering"
    )
    return(fallback_result)
  }

  # Pull the metadata describing the analysis set that applies to
  # the first analysis for this output.
  temp_AnSet <- analysis_sets |>
    dplyr::filter(id == analysis_set_id)

  if (nrow(temp_AnSet) == 0) {
    cli::cli_warn(
      "Metadata issue in Analyses {analysis_id}: AnalysisSet {analysis_set_id} not found; using the analysis dataset without filtering"
    )
    return(fallback_result)
  }

  # Extract the dataset, variable, comparison operator, and value used to
  # define the population for this analysis set.
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

  required_components <- c(
    `condition dataset` = cond_adam,
    `condition variable` = cond_var,
    `condition comparator` = cond_oper
  )

  is_missing <- function(x) {
    length(x) == 0 || is.na(x) || x %in% c("", "NA")
  }

  missing_components <- names(required_components)[vapply(required_components, is_missing, logical(1))]

  if (length(missing_components) > 0) {
    cli::cli_warn(
      "Metadata issue in AnalysisSets {analysis_set_id} for Analysis {analysis_id}: missing {paste(missing_components, collapse = ', ')}; using the analysis dataset without filtering"
    )
    return(fallback_result)
  }

  # Translate the ARS comparator codes to R operators for evaluation in
  # the generated script.
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

  # Generate different population code depending on whether the analysis
  # set is sourced from the same ADaM as the primary analysis or a
  # companion dataset.
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
