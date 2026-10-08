#' Generate the population code for one output
#'
#' Internal helper that builds, once per output, the code defining every
#' analysis-set population frame the output's analyses need, and reports which
#' frame each analysis reads from. Everything is derived from the ARS metadata
#' rather than from the position of an analysis within the output (#197):
#'
#' * one population per distinct `analysisSetId` used by the output;
#' * an analysis reads the **subject-level** frame (`df_poptot`, the analysis
#'   set condition applied to its own dataset, typically ADSL) when every
#'   dataset it touches is the analysis set's dataset - its groupings'
#'   `groupingDataset`, its data subset's `condition.dataset` and, unless its
#'   variable is the subject key `USUBJID`, its own `dataset`. This keeps the
#'   bigN analysis counting every subject in the population, including those
#'   with no records in the content dataset;
#' * otherwise it reads the **record-level** frame (`df_pop`): the population
#'   inner-joined onto the one foreign dataset the analysis touches.
#'
#' Frame names stay `df_pop` / `df_poptot` for the common single-set,
#' single-content-dataset output. With more than one analysis set the set id is
#' appended (`df_poptot__AnalysisSet_07`), and when one set is merged onto more
#' than one content dataset the dataset is appended to the merged frame
#' (`df_pop__ADLB`).
#'
#' @param anas The analyses tied to the current output (Lopa subset), in order.
#' @param analyses Analyses metadata for the reporting event.
#' @param analysis_sets AnalysisSets metadata for the reporting event.
#' @param analysis_groupings AnalysisGroupings metadata for the reporting event.
#' @param data_subsets DataSubsets metadata for the reporting event.
#'
#' @return A list with `code` (the population block), `frames` (named character
#'   vector: analysis id -> frame its data subset starts from) and `poptot`
#'   (named character vector: analysis id -> subject-level frame of its
#'   analysis set, substituted for `df_poptot` in method templates).
#' @keywords internal
.generate_population_code <- function(anas,
                                      analyses,
                                      analysis_sets,
                                      analysis_groupings = NULL,
                                      data_subsets = NULL) {
  analysis_ids <- unique(as.character(anas$listItem_analysisId))

  is_blank <- function(x) {
    length(x) == 0 || is.na(x[1]) || x[1] %in% c("", "NA")
  }

  # Resolve each analysis' set, dataset and the datasets it touches ----
  info <- lapply(analysis_ids, function(aid) {
    a <- analyses[analyses$id == aid, , drop = FALSE]
    set_id <- if (nrow(a) > 0 && "analysisSetId" %in% names(a)) {
      as.character(a$analysisSetId[1])
    } else {
      NA_character_
    }
    dataset <- if (nrow(a) > 0) as.character(a$dataset[1]) else NA_character_
    variable <- if (nrow(a) > 0 && "variable" %in% names(a)) {
      as.character(a$variable[1])
    } else {
      NA_character_
    }

    group_cols <- grep("^groupingId[0-9]+$", names(a), value = TRUE)
    group_ids <- if (nrow(a) > 0) as.character(unlist(a[1, group_cols])) else character(0)
    group_ids <- group_ids[!is.na(group_ids) & nzchar(group_ids)]
    group_ds <- if (!is.null(analysis_groupings) && length(group_ids) > 0) {
      analysis_groupings$groupingDataset[analysis_groupings$id %in% group_ids]
    } else {
      character(0)
    }

    subset_id <- if (nrow(a) > 0 && "dataSubsetId" %in% names(a)) {
      as.character(a$dataSubsetId[1])
    } else {
      NA_character_
    }
    subset_ds <- if (!is.null(data_subsets) && !is_blank(subset_id)) {
      data_subsets$condition_dataset[data_subsets$id == subset_id]
    } else {
      character(0)
    }

    # The subject key exists in every ADaM, so a USUBJID analysis does not
    # need its declared dataset (a bigN declared on ADAE still counts ADSL).
    own_ds <- if (identical(variable, "USUBJID")) character(0) else dataset
    touched <- unique(as.character(c(own_ds, group_ds, subset_ds)))
    touched <- touched[!is.na(touched) & !touched %in% c("", "NA")]

    list(
      id = aid,
      set_id = if (is_blank(set_id)) NA_character_ else set_id,
      dataset = dataset,
      touched = touched
    )
  })

  # Resolve each distinct analysis set to a population dataset + filter ----
  set_keys <- unique(vapply(info, function(x) x$set_id, character(1)))
  multi_set <- length(set_keys) > 1

  sets <- lapply(set_keys, function(set_id) {
    first <- info[[which(vapply(info, function(x) identical(x$set_id, set_id), logical(1)))[1]]]
    .resolve_analysis_set(set_id, analysis_sets, first$id, first$dataset)
  })

  frames <- stats::setNames(character(length(info)), analysis_ids)
  poptot <- frames
  blocks <- character(0)

  for (s in seq_along(set_keys)) {
    set <- sets[[s]]
    set_info <- Filter(function(x) identical(x$set_id, set_keys[s]), info)
    sfx <- if (multi_set) paste0("__", if (is.na(set_keys[s])) "unspecified" else set_keys[s]) else ""
    poptot_name <- paste0("df_poptot", sfx)

    # Foreign datasets each analysis of this set touches (unfiltered fallback
    # populations are never merged, matching the pre-#197 behaviour).
    foreign <- lapply(set_info, function(x) {
      if (is.null(set$filter)) character(0) else setdiff(x$touched, set$dataset)
    })
    for (k in seq_along(set_info)) {
      if (length(foreign[[k]]) > 1) {
        cli::cli_abort(c(
          "Metadata issue in Analyses {set_info[[k]]$id}: the analysis uses more than one dataset besides its analysis set dataset {.val {set$dataset}}: {.val {foreign[[k]]}}.",
          "i" = "siera merges the population onto a single content dataset per analysis."
        ))
      }
    }
    merge_ds <- unique(unlist(foreign))
    multi_ds <- length(merge_ds) > 1
    pop_name_for <- function(ds) paste0("df_pop", sfx, if (multi_ds) paste0("__", ds) else "")

    for (k in seq_along(set_info)) {
      aid <- set_info[[k]]$id
      poptot[[aid]] <- poptot_name
      frames[[aid]] <- if (length(foreign[[k]]) == 0) poptot_name else pop_name_for(foreign[[k]])
    }

    header <- if (multi_set) {
      # Names may carry padding or line breaks; keep the comment on one line.
      set_name <- trimws(gsub("[\r\n]+", " ", set$name))
      paste0("# Analysis set: ", if (is.na(set_keys[s])) "(none)" else set_keys[s],
             if (!is.na(set_name) && nzchar(set_name)) paste0(" - ", set_name) else "", "\n")
    } else {
      ""
    }

    if (is.null(set$filter)) {
      body <- paste0(
        "df_pop", sfx, " <- ", set$dataset, "\n",
        poptot_name, " <- df_pop", sfx, "\n"
      )
    } else if (length(merge_ds) == 0) {
      body <- paste0(
        "df_pop", sfx, " <- dplyr::filter(", set$dataset, ",\n",
        "            ", set$filter, ")\n",
        poptot_name, " <- df_pop", sfx, "\n"
      )
    } else {
      merges <- vapply(merge_ds, function(ds) {
        paste0(
          "overlap <- intersect(names(", set$dataset, "), names(", ds, "))\n",
          "overlapfin <- setdiff(overlap, 'USUBJID')\n",
          pop_name_for(ds), " <- dplyr::filter(", set$dataset, ",\n",
          "            ", set$filter, ") |>\n",
          "            merge(", ds, " |> dplyr::select(-dplyr::all_of(overlapfin)),\n",
          "                  by = 'USUBJID',\n",
          "                  all = FALSE)\n"
        )
      }, character(1))
      body <- paste0(
        paste(merges, collapse = ""),
        poptot_name, " = dplyr::filter(", set$dataset, ",\n",
        "            ", set$filter, ")\n"
      )
    }
    blocks <- c(blocks, paste0(header, body))
  }

  # A leading newline separates the block from the analysis heading comment;
  # the unfiltered single-set fallback historically had none.
  lead <- if (!multi_set && is.null(sets[[1]]$filter)) "" else "\n"
  code <- paste0(lead, "# Apply Analysis Set ---\n", paste(blocks, collapse = "\n"))

  list(code = code, frames = frames, poptot = poptot)
}

#' Resolve an analysis set to its population dataset and filter
#'
#' Internal helper for [.generate_population_code()]. Translates one ARS
#' analysis set into the dataset and filter expression of its population. When
#' the set cannot be used (no id, no metadata, unknown id or incomplete
#' condition) it warns and falls back to the unfiltered dataset of the first
#' analysis using it, signalled by a `NULL` filter.
#'
#' @param set_id Analysis set identifier (or `NA`).
#' @param analysis_sets AnalysisSets metadata for the reporting event.
#' @param analysis_id First analysis using the set (for messages).
#' @param analysis_dataset Dataset of that analysis (fallback population).
#'
#' @return A list with `dataset`, `filter` (character or `NULL`) and `name`.
#' @keywords internal
.resolve_analysis_set <- function(set_id, analysis_sets, analysis_id, analysis_dataset) {
  if (is.na(analysis_dataset) || analysis_dataset %in% c("", "NA")) {
    analysis_dataset <- "df_pop"
  }
  fallback <- list(dataset = analysis_dataset, filter = NULL, name = NA_character_)

  if (is.na(set_id)) {
    cli::cli_warn(
      "Metadata issue in Analyses {analysis_id}: Analysis is missing an analysisSetId; using the analysis dataset without filtering"
    )
    return(fallback)
  }

  if (is.null(analysis_sets)) {
    cli::cli_warn(
      "Metadata issue in Analyses {analysis_id}: AnalysisSets metadata not supplied; using the analysis dataset without filtering"
    )
    return(fallback)
  }

  temp_AnSet <- analysis_sets |>
    dplyr::filter(id == set_id)

  if (nrow(temp_AnSet) == 0) {
    cli::cli_warn(
      "Metadata issue in Analyses {analysis_id}: AnalysisSet {set_id} not found; using the analysis dataset without filtering"
    )
    return(fallback)
  }

  cond_adam <- as.character(temp_AnSet$condition_dataset[1])
  cond_var <- as.character(temp_AnSet$condition_variable[1])
  cond_oper <- as.character(temp_AnSet$condition_comparator[1])

  cond_val <- unlist(temp_AnSet$condition_value)
  # unlist() of a list-column containing NULL returns NULL (not character(0));
  # normalise to NA early so the checks below see a length-one value.
  cond_val <- if (is.null(cond_val) || length(cond_val) == 0) NA_character_ else cond_val[1]

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
      "Metadata issue in AnalysisSets {set_id} for Analysis {analysis_id}: missing {paste(missing_components, collapse = ', ')}; using the analysis dataset without filtering"
    )
    return(fallback)
  }

  # Translate the ARS comparator codes to R operators for evaluation in the
  # generated script.
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
  }

  list(
    dataset = cond_adam,
    filter = paste0(cond_var, " ", oper, " '", cond_val, "'"),
    name = if ("name" %in% names(temp_AnSet)) as.character(temp_AnSet$name[1]) else NA_character_
  )
}
