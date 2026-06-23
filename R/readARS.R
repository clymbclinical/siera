#' Ingest ARS (Analysis Results Standard) metadata, produce ARD (Analysis Results Dataset) code for each output
#'
#' Ingest ARS (Analysis Results Standard) metadata, and meta-programme R scripts
#' that could be run as-is to produce Analysis Results Datasets when ingesting ADaM
#' datasets
#'
#' @param ARS_path A file containing ARS metadata for a reporting event
#' @param output_path Path to store .R ARD scripts
#' @param adam_path Path to folder containing ADaM datasets, to be run in
#'  ARD program. Datasets may be supplied as CSV (`.csv`) or SAS transport
#'  (`.xpt`) files. siera chooses the reader for each dataset from the file
#'  extension found in this folder — `.csv` files are read with
#'  \code{readr::read_csv()} and `.xpt` files with \code{haven::read_xpt()} —
#'  so no extra argument is required (mirroring how the ARS input format is
#'  inferred from `.json` vs `.xlsx`). Reading `.xpt` datasets in the generated
#'  script requires the \pkg{haven} package. When both a `.csv` and a `.xpt`
#'  exist for the same dataset, the `.csv` is used.
#' @param spec_output The output ID for a specific output to be run from
#' the metadata
#' @param output_format Format for emitting the generated ARD. Must be exactly
#'  one of `"none"` or `"datasetjson"` (no partial matching). `"none"`
#'  (default) generates the ARD scripts only. `"datasetjson"` additionally
#'  appends code that writes each ARD as a CDISC Dataset-JSON file
#'  (`ARD_<OutputId>.json`) when the generated script is run. The
#'  Dataset-JSON export requires the optional \pkg{datasetjson} package to be
#'  installed in the environment that runs the generated script.
#'
#' @importFrom readxl read_excel
#'
#' @returns R programmes generating ARDs - one for each output (or analysis from an output) specified in the ARS metadata
#' @export
#'
#' @examples
#' # path to file containing ARS metadata
#'
#' ARS_path <- ARS_example("Common_Safety_Displays_cards.xlsx")
#'
#' # output path for R programs
#' output_dir <- tempdir()
#'
#' # folder containing ADaM datasets
#' adam_folder <- tempdir()
#'
#' # run function, write to temp directory
#' readARS(ARS_path, output_dir, adam_folder)
#'
readARS <- function(ARS_path,
                    output_path = tempdir(),
                    adam_path = tempdir(),
                    spec_output = "",
                    output_format = "none") {
  # Strict validation: no partial matching, omitted/NULL -> "none"
  if (is.null(output_format)) {
    output_format <- "none"
  }
  if (length(output_format) != 1L ||
      !output_format %in% c("none", "datasetjson")) {
    cli::cli_abort(c(
      "{.arg output_format} must be one of {.val none} or {.val datasetjson}.",
      "x" = "You supplied {.val {output_format}}."
    ))
  }
  code_libraries <- .generate_library_code()

  # Read in ARS metadata ----------------------------------------------------

  metadata <- .read_ars_metadata(ARS_path)

  if (is.null(metadata)) {
    return(invisible(NULL))
  }

  file_ext <- metadata$file_ext
  Lopo <- metadata$Lopo
  Lopa <- metadata$Lopa
  DataSubsets <- metadata$DataSubsets
  AnalysisSets <- metadata$AnalysisSets
  AnalysisGroupings <- metadata$AnalysisGroupings
  Analyses <- metadata$Analyses
  AnalysisMethods <- metadata$AnalysisMethods
  AnalysisMethodCodeTemplate <- metadata$AnalysisMethodCodeTemplate
  AnalysisMethodCodeParameters <- metadata$AnalysisMethodCodeParameters

  # Handle specific outputs or analyses -------------

  # specific output
  if (spec_output != "") {
    Lopo <- Lopo |>
      dplyr::filter(listItem_outputId == spec_output)

    Lopa <- Lopa |>
      dplyr::filter(listItem_outputId == spec_output)
  }

  # Prework and loops ----------------------------------------------------

  for (i in 1:nrow(Lopo)) {
    Output <- Lopo[i, ]$listItem_outputId
    OutputName <- Lopo[i, ]$listItem_name

    Anas <- Lopa |> # get all analyses for current output
      dplyr::filter(
        listItem_outputId == Output,
        listItem_analysisId %in% Analyses$id
      ) # exclude if not methodid

    # Load ADaMs ----
    code_ADaM <- .generate_adam_loading_code(
      Anas = Anas,
      Analyses = Analyses,
      AnalysisSets = AnalysisSets,
      DataSubsets = DataSubsets,
      adam_path = adam_path
    )

    run_code <- "" # variable to contain generated code
    combine_analysis_code <- "" # variable containing code to combine analyses

    # Programme header ----
    timenow <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    code_header <- .generate_program_header(Output, OutputName, timenow)

    # loop through individual analyses
    max_j <- nrow(Anas)
    for (j in 1:max_j) {
      # for (j in 1:nrow(Anas)) {

      # Analysis
      Anas_j <- Anas[j, ]$listItem_analysisId # AnalysisID from
      # PA to dplyr::filter AN -jsonized
      Anas_s <- Analyses |> # row from AN to get other IDs
        dplyr::filter(id == Anas_j)

      ana_adam <- Anas_s$dataset # ADaM used for this analysis (esp. to be used in ChiSq)
      ana_name <- Anas_s$name
      # Analysis Set
      ana_setId <- Anas_s$analysisSetId # AS ID (to be used in AS)
      ana_var <- Anas_s$variable # AS variable (to be used in MT)

      # Analysis Grouping — dynamic extraction supporting any number of groupings
      column_names <- colnames(Analyses)
      group_cols <- column_names[grep("^groupingId[0-9]+$", column_names)]
      max_group_number <- max(as.numeric(sub("groupingId", "", group_cols)))

      groupids <- character(0)
      results_by_group <- logical(0)
      for (k in seq_len(max_group_number)) {
        gid <- Anas_s[[paste0("groupingId", k)]]
        if (is.null(gid) || length(gid) == 0 || is.na(gid) || nchar(as.character(gid)) == 0) break
        groupids <- c(groupids, as.character(gid))
        rbg_val <- Anas_s[[paste0("resultsByGroup", k)]]
        results_by_group <- c(results_by_group, isTRUE(as.logical(rbg_val)))
      }
      n_actual_groups <- length(groupids)

      # Data Subset
      subsetid <- Anas_s$dataSubsetId # data subset ID (to be used in DS)
      if (subsetid %in% c("", "NA")) {
        subsetid <- NA
      } else {
        subsetid <- subsetid
      }

      # Method
      methodid <- Anas_s$method_id #

      # Apply Analysis Set -----
      analysis_set_result <- .generate_analysis_set_code(
        j = j,
        analysis_sets = AnalysisSets,
        analyses = Analyses,
        anas = Anas,
        analysis_set_id = ana_setId,
        analysis_id = Anas_j
      )

      code_as <- analysis_set_result$code
      AnSetDataSubsets <- analysis_set_result$data_subset
      # Apply Grouping ----------------------------

      # consider denominator analysis
      NUM_analysisid <- Anas_s$referencedAnalysisOperations_analysisId1
      DEN_analysisid <- Anas_s$referencedAnalysisOperations_analysisId2

      # Continuous-only tables have no denominator; column may be absent (NULL).
      has_denom <- !is.null(DEN_analysisid) &&
                   length(DEN_analysisid) > 0 &&
                   !is.na(DEN_analysisid) &&
                   !DEN_analysisid %in% c("", "NA")

      if (has_denom) {
        AG_denom_id <- Analyses |>
          dplyr::filter(id == DEN_analysisid) |>
          dplyr::select(groupingId1) |>
          unique() |>
          as.character()

        AG_denom_temp1 <- AnalysisGroupings |>
          dplyr::filter(id == AG_denom_id)

        AG_denom_var1 <- AG_denom_temp1 |>
          dplyr::select(groupingVariable) |>
          unique() |>
          as.character()

        if (AG_denom_var1 %in% c(NA, "NA", "")) {
          cli::cli_alert("Metadata issue in AnalysisGroupings {AG_denom_id}: AnalysisGrouping has missing groupingVariable")
        }
      } else {
        AG_denom_id   <- NA_character_
        AG_denom_var1 <- NA_character_
      }

      # Fetch grouping variable info for each active grouping
      AG_vars <- character(n_actual_groups)
      AG_ds <- character(n_actual_groups)
      AG_dataDriven <- character(n_actual_groups)

      for (k in seq_len(n_actual_groups)) {
        ag_temp <- AnalysisGroupings |> dplyr::filter(id == groupids[k])
        AG_vars[k] <- ag_temp |> dplyr::select(groupingVariable) |> unique() |> as.character()
        AG_ds[k] <- ag_temp |> dplyr::select(groupingDataset) |> unique() |> as.character()
        AG_dataDriven[k] <- ag_temp |> dplyr::select(dataDriven) |> unique() |> as.character()
      }

      # Backward-compatible scalar aliases for method templates that reference AG_var1/2/3
      if (n_actual_groups >= 1) AG_var1 <- AG_vars[1]
      if (n_actual_groups >= 2) AG_var2 <- AG_vars[2]
      if (n_actual_groups >= 3) AG_var3 <- AG_vars[3]

      # num_grp: count of leading consecutive TRUE resultsByGroup flags
      num_grp <- 0
      AG_max_dataDriven <- "FALSE"
      for (k in seq_len(n_actual_groups)) {
        if (isTRUE(results_by_group[k])) {
          num_grp <- k
          AG_max_dataDriven <- AG_dataDriven[k]
        } else {
          break
        }
      }

      num_grp_any <- n_actual_groups

      if (num_grp > 0) {
        active_vars <- AG_vars[seq_len(num_grp)]
        distinct_list <- paste(c(active_vars, ana_var), collapse = ", ")
        by_stmt <- paste0(", by = ", paste(active_vars, collapse = ", "))
        by_listc <- paste(paste0("'", active_vars, "'"), collapse = ", ")
        by_list <- paste(active_vars, collapse = ", ")

        if (num_grp == 1) {
          by_vars <- paste0(", variables = '", active_vars[1], "'")
          strata_vars <- paste0(", variables = '", active_vars[1], "'")
        } else if (num_grp == 2) {
          by_vars <- paste0(", by = '", active_vars[1], "' , variables = '", active_vars[2], "'")
          strata_vars <- paste0(", strata = '", active_vars[1], "' , variables = '", active_vars[2], "'")
        } else {
          inner_vars <- paste(paste0("'", active_vars[-num_grp], "'"), collapse = ", ")
          by_vars <- paste0(", by = c(", inner_vars, ") , variables = '", active_vars[num_grp], "'")
          strata_vars <- paste0(", strata = c(", inner_vars, ") , variables = '", active_vars[num_grp], "'")
        }
      } else {
        distinct_list <- ana_var
        by_stmt <- ""
        cont_by_stmt <- ""
        by_listc <- ""
        by_list <- ""
        by_vars <- ""
        strata_vars <- ""
      }

      # Apply DataSubset -------------------------------------------------------------
      data_subset_result <- .generate_data_subset_code(
        data_subsets = DataSubsets,
        subset_id = subsetid,
        analysis_id = Anas_j,
        analysis_set_dataset = AnSetDataSubsets,
        file_ext = file_ext
      )

      code_ds <- data_subset_result$code

      # Apply AnalysisMethod -------------------------------------------------------------

      # Build the explicit lookup table of all computed string values that ARS
      # code-template parameters can reference via their valueSource key.
      # Operation IDs (operation_1 etc.) are computed inside the function itself.
      value_sources <- c(
        list(
          distinct_list     = distinct_list,
          by_stmt           = by_stmt,
          by_vars           = by_vars,
          strata_vars       = strata_vars,
          by_listc          = by_listc,
          by_list           = by_list,
          ana_var           = ana_var,
          AG_max_dataDriven = AG_max_dataDriven,
          DEN_analysisid    = DEN_analysisid,
          AG_denom_var1     = AG_denom_var1
        ),
        if (n_actual_groups >= 1) list(AG_var1 = AG_var1),
        if (n_actual_groups >= 2) list(AG_var2 = AG_var2),
        if (n_actual_groups >= 3) list(AG_var3 = AG_var3)
      )

      analysis_method_result <- .generate_analysis_method_section(
        analysis_methods = AnalysisMethods,
        analysis_method_code_template = AnalysisMethodCodeTemplate,
        analysis_method_code_parameters = AnalysisMethodCodeParameters,
        method_id = methodid,
        analysis_id = Anas_j,
        output_id = Output,
        value_sources = value_sources
      )

      code_method <- analysis_method_result$code

      method <- analysis_method_result$method
      methodname <- method$name
      methoddesc <- method$description
      methodlabel <- method$label
      methodid <- method$id

      code_method_frag <- paste0("#Apply Method --- \n", code_method)

      # Determine how many group[n] columns this method produces in the ARD.
      # Inspect the method CODE TEMPLATE (before substitution) to find which
      # parameters are actually present as placeholder tokens, then check their
      # valueSource types. This avoids both (a) inspecting the generated code
      # string and (b) false positives from parameter-table rows whose
      # placeholder names do not appear in the template.
      n_group_cols <- .n_group_cols_from_template(
        num_grp                       = num_grp,
        method_id                     = methodid,
        analysis_method_code_template = AnalysisMethodCodeTemplate,
        analysis_method_code_parameters = AnalysisMethodCodeParameters
      )

      code_groupid <- .generate_groupid_code(
        analysis_id        = Anas_j,
        groupids           = groupids,
        n_group_cols       = n_group_cols,
        AG_dataDriven      = AG_dataDriven,
        analysis_groupings = AnalysisGroupings
      )

      # Generate code for analysis ----------------------------------------------

      # Coerce *_level columns (variable_level, group[n]_level) to character on
      # each individual df3 BEFORE bind_rows combines them.  Cards returns
      # variable_level as a list-of-NULLs for continuous analyses, and as
      # character for categorical ones; left as-is they create type conflicts
      # during bind_rows.  The vapply converts NULL -> NA_character_ safely
      # without touching the numeric 'stat' list column.
      code_listcoerce <- paste0(
        "df3_", Anas_j, " <- df3_", Anas_j, " |>\n",
        "  dplyr::mutate(dplyr::across(\n",
        "    dplyr::matches('_level$'),\n",
        "    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))\n",
        "  ))\n"
      )

      code_per_analysis <- paste0(
        "\n\n# Analysis ", Anas_j, "----\n#",
        ana_name,
        code_as,
        code_ds,
        code_method_frag,
        code_groupid,
        code_listcoerce
      )

      run_code <- paste0(run_code, code_per_analysis)

      if (j < max_j) {
        combine_analysis_code <- paste0(
          combine_analysis_code,
          "df3_", Anas_j, ", \n"
        )
      } else {
        combine_analysis_code <- paste0(
          combine_analysis_code,
          "df3_", Anas_j
        )
      }
    } # end of analysis

    # add pattern formatting


    code_pattern <- paste0("ARD <- df4 |>
      dplyr::mutate(dec = ifelse(grepl('X.X',
                                df4$pattern, ),
                          stringr::str_count(substr(df4$pattern,
                                          stringr::str_locate(df4$pattern,
                                                    'X.X')[, 1]+2,
                                          nchar(df4$pattern)), 'X'),
                          0)) |>
      dplyr::rowwise() |>
      dplyr::mutate(rnd = round(res, dec)) |>
      tibble::as_tibble() |>
      dplyr::mutate(disp = ifelse(grepl('\\\\(N=', df4$pattern),
                           paste0('(N=', rnd, ')'),
                           ifelse(grepl('\\\\(', df4$pattern),
                                  paste0('(', rnd, ')'),
                                  as.character(rnd)))) |>
                         dplyr::select(-rnd, -dec)")


    # add all code, combine analyses ARDs and apply pattern
    code_output <- paste0(
      code_header,
      code_libraries,
      code_ADaM,
      run_code,
      "\n\n# combine analyses to create ARD ----\n",
      "ARD <- dplyr::bind_rows(",
      combine_analysis_code,
      ") "
    )

    # Optionally append CDISC Dataset-JSON export code ----
    if (output_format == "datasetjson") {
      code_output <- paste0(
        code_output,
        .generate_datasetjson_code(Output, output_path)
      )
    }

    writeLines(
      code_output,
      paste0(output_path, "/ARD_", Output, ".R")
    )
  } # end of outputs
}

# Generate mutate code that stamps group[n]_groupingId and group[n]_groupId
# onto df3_<analysis_id> for each ARD group column produced by the method.
# Works identically for XLSX- and JSON-sourced ARS files because both produce
# the same AnalysisGroupings tibble with id, group_id, group_condition_value,
# and dataDriven columns.
.generate_groupid_code <- function(analysis_id, groupids, n_group_cols,
                                   AG_dataDriven, analysis_groupings) {
  if (n_group_cols < 1L) return("")

  mutate_parts <- character(0)

  for (k in seq_len(n_group_cols)) {
    gid            <- groupids[k]
    grp_level_col  <- paste0("group", k, "_level")
    gid_col        <- paste0("group", k, "_groupingId")
    gpid_col       <- paste0("group", k, "_groupId")

    mutate_parts <- c(mutate_parts,
      paste0("      ", gid_col, " = '", gid, "'")
    )

    gpval_col <- paste0("group", k, "_groupValue")

    if (isTRUE(as.logical(AG_dataDriven[k]))) {
      mutate_parts <- c(mutate_parts,
        paste0("      ", gpid_col,  " = NA_character_"),
        paste0("      ", gpval_col, " = as.character(", grp_level_col, ")")
      )
    } else {
      grp_rows <- analysis_groupings[
        analysis_groupings$id == gid &
          !is.na(analysis_groupings$group_id) &
          nchar(as.character(analysis_groupings$group_id)) > 0, ]

      if (nrow(grp_rows) == 0L) {
        mutate_parts <- c(mutate_parts,
          paste0("      ", gpid_col, " = NA_character_")
        )
      } else {
        cond_vals <- gsub("'", "\\'", grp_rows$group_condition_value, fixed = TRUE)
        grp_ids   <- grp_rows$group_id
        cases <- paste(
          paste0("        as.character(", grp_level_col, ") == '",
                 cond_vals, "' ~ '", grp_ids, "'"),
          collapse = ",\n"
        )
        mutate_parts <- c(mutate_parts,
          paste0("      ", gpid_col, " = dplyr::case_when(\n",
                 cases, ",\n",
                 "        TRUE ~ NA_character_\n",
                 "      )")
        )
      }
    }
  }

  paste0(
    "if(nrow(df2_", analysis_id, ") != 0){\n",
    "df3_", analysis_id, " <- df3_", analysis_id, " |>\n",
    "  dplyr::mutate(\n",
    paste(mutate_parts, collapse = ",\n"),
    "\n  )\n}\n"
  )
}

# Determine how many group[n] columns a method produces in the ARD output.
#
# Checks which valueSource types are used by parameters whose placeholder
# tokens actually appear in the method's code template. Checking the template
# (not the parameter table and not the generated code) avoids two failure
# modes: (a) spurious parameter-table rows emitted by some tools even when
# the placeholder is absent from the template; (b) coupling to the exact
# string format of runtime-resolved values like by_vars or strata_vars.
.n_group_cols_from_template <- function(num_grp,
                                        method_id,
                                        analysis_method_code_template,
                                        analysis_method_code_parameters) {
  if (num_grp == 0L) return(0L)

  mid <- method_id

  template_code <- analysis_method_code_template |>
    dplyr::filter(
      method_id == mid,
      context %in% c("R", "R (siera)", "siera"),
      specifiedAs == "Code"
    ) |>
    dplyr::pull(templateCode) |>
    paste(collapse = "\n")

  params <- analysis_method_code_parameters |>
    dplyr::filter(
      method_id == mid,
      parameter_valueSource != ""
    )

  # Keep only parameters whose placeholder token is actually in the template.
  in_template <- vapply(
    params$parameter_name,
    function(p) grepl(p, template_code, fixed = TRUE),
    logical(1L)
  )
  active_sources <- params$parameter_valueSource[in_template]

  if (any(active_sources %in% c("by_vars", "strata_vars"))) {
    max(0L, num_grp - 1L)
  } else if (any(active_sources == "by_listc")) {
    num_grp
  } else {
    0L
  }
}
