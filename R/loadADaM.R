.generate_adam_loading_code <- function(Anas, Analyses, AnalysisSets, DataSubsets, adam_path) {
  Analyses_IDs <- Analyses %>%
    dplyr::filter(id %in% Anas$listItem_analysisId)

  unique_datasets <- c(
    Analyses_IDs$dataset,
    AnalysisSets %>%
      dplyr::filter(id %in% Analyses_IDs$analysisSetId) %>%
      dplyr::pull(condition_dataset),
    DataSubsets %>%
      dplyr::filter(id %in% Analyses_IDs$dataSubsetId, !is.na(condition_dataset)) %>%
      dplyr::pull(condition_dataset)
  )

  unique_datasets <- unique(unique_datasets)
  unique_datasets <- unique_datasets[!is.na(unique_datasets) & unique_datasets != ""]

  if (length(unique_datasets) == 0) {
    return("\n# Load ADaM -------\n")
  }

  lines <- vapply(unique_datasets, function(ad) {
    ad_path <- paste0(adam_path, "/", ad, ".csv")
    paste0(
      ad, " <- readr::read_csv('", ad_path, "',\n",
      "                                      show_col_types = FALSE,\n",
      "                                      progress = FALSE) |>\n",
      "  dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ tidyr::replace_na(.x, '')))\n"
    )
  }, character(1))

  paste0("\n# Load ADaM -------\n", paste(unique(lines), collapse = ""))
}
