#' Generate ADaM loading code
#'
#' Internal helper that inspects the analyses referenced by an output and
#' builds the code required to read the relevant ADaM datasets. Only datasets
#' referenced in the analyses, analysis sets, or data subsets are included in
#' the returned code block.
#' @param Anas Subset of analyses tied to the current output (Lopa rows).
#' @param Analyses Analyses metadata for the reporting event.
#' @param AnalysisSets AnalysisSets metadata for the reporting event.
#' @param DataSubsets DataSubsets metadata for the reporting event.
#' @param adam_path Directory containing the ADaM datasets on disk. ADaM
#'  datasets may be supplied as CSV (`.csv`) or SAS transport (`.xpt`) files;
#'  the reader emitted for each dataset is chosen from the file extension found
#'  on disk (see [.generate_one_adam_read()]).
#'
#' @return Character string containing the code used to load ADaM datasets.
#' @keywords internal
.generate_adam_loading_code <- function(Anas, Analyses, AnalysisSets, DataSubsets, adam_path) {
  # Identify the analyses referenced for the current output.
  Analyses_IDs <- Analyses %>%
    dplyr::filter(id %in% Anas$listItem_analysisId)

  # Collect every ADaM dataset referenced directly, in an analysis set,
  # or within data subset metadata.
  ds_datasets <- character(0)
  if (!is.null(DataSubsets) &&
      nrow(DataSubsets) > 0 &&
      all(c("id", "condition_dataset") %in% colnames(DataSubsets))) {
    ds_datasets <- DataSubsets %>%
      dplyr::filter(id %in% Analyses_IDs$dataSubsetId, !is.na(condition_dataset)) %>%
      dplyr::pull(condition_dataset)
  }

  unique_datasets <- c(
    Analyses_IDs$dataset,
    AnalysisSets %>%
      dplyr::filter(id %in% Analyses_IDs$analysisSetId) %>%
      dplyr::pull(condition_dataset),
    ds_datasets
  )

  unique_datasets <- unique(unique_datasets)
  unique_datasets <- unique_datasets[!is.na(unique_datasets) & unique_datasets != ""]

  if (length(unique_datasets) == 0) {
    return("\n# Load ADaM -------\n")
  }

  # Build one read call per dataset, standardising character NA handling to
  # avoid downstream joins failing on missing text values. CSV and XPT inputs
  # are both supported; the reader is chosen per dataset from the extension of
  # the file found on disk.
  adam_dir <- gsub("\\\\", "/", adam_path)
  lines <- vapply(unique_datasets, function(ad) {
    .generate_one_adam_read(ad, adam_dir)
  }, character(1))

  paste0("\n# Load ADaM -------\n", paste(unique(lines), collapse = ""))
}

#' Generate the read call for a single ADaM dataset
#'
#' Internal helper that emits the code to read one ADaM dataset, choosing the
#' reader from the file extension present on disk. SAS transport files
#' (`.xpt`) are read with [haven::read_xpt()]; everything else (and the default
#' when no matching file is found) is read with [readr::read_csv()]. To remain
#' backward compatible, CSV takes precedence when both a `.csv` and a `.xpt`
#' exist for the same dataset.
#'
#' The lookup is case-insensitive on the file name, because real-world CDISC
#' SAS transport files are commonly lower-case (e.g. `adsl.xpt`) while the ARS
#' metadata names the dataset in upper case (`ADSL`). The dataset is still
#' assigned to an object named after the metadata dataset name, but the path in
#' the generated code points at the actual file found on disk.
#'
#' @param ad Dataset name (without extension), e.g. `"ADSL"`.
#' @param adam_dir Directory containing the ADaM datasets, with forward slashes.
#'
#' @return Character string containing the read call for the dataset.
#' @keywords internal
.generate_one_adam_read <- function(ad, adam_dir) {
  # Resolve the actual on-disk file for one extension, allowing the file name
  # to differ in case from the metadata dataset name. Returns NA when absent.
  resolve <- function(ext) {
    exact <- paste0(adam_dir, "/", ad, ".", ext)
    if (file.exists(exact)) return(exact)
    if (dir.exists(adam_dir)) {
      hits <- list.files(
        adam_dir,
        pattern     = paste0("^", ad, "\\.", ext, "$"),
        ignore.case = TRUE,
        full.names  = TRUE
      )
      if (length(hits) > 0) return(gsub("\\\\", "/", hits[[1]]))
    }
    NA_character_
  }

  csv_file <- resolve("csv")
  xpt_file <- resolve("xpt")

  # Use the XPT reader only when a .xpt exists and a .csv does not, so existing
  # CSV-based workflows are never altered.
  if (is.na(csv_file) && !is.na(xpt_file)) {
    paste0(
      ad, " <- haven::read_xpt('", xpt_file, "') |>\n",
      "  dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ tidyr::replace_na(.x, '')))\n"
    )
  } else {
    # Fall back to the conventional <dataset>.csv path when no file is found,
    # preserving the historical behaviour for example/placeholder folders.
    csv_path <- if (!is.na(csv_file)) csv_file else paste0(adam_dir, "/", ad, ".csv")
    paste0(
      ad, " <- readr::read_csv('", csv_path, "',\n",
      "                                      show_col_types = FALSE,\n",
      "                                      progress = FALSE) |>\n",
      "  dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ tidyr::replace_na(.x, '')))\n"
    )
  }
}
