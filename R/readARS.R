#' Ingest ARS (Analysis Results Standard) metadata, produce ARD (Analysis Results Dataset) code for each output
#'
#' Ingest ARS (Analysis Results Standard) metadata, and meta-programme R scripts
#' that could be run as-is to produce Analysis Results Datasets when ingesting ADaM
#' datasets
#'
#' @param ARS_path A file containing ARS metadata for a reporting event
#' @param output_path Path to store .R ARD scripts
#' @param adam_path Path to folder containing ADaM datasets, to be run in
#'  ARD program
#' @param spec_output The output ID for a specific output to be run from
#' the metadata
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
                    spec_output = "") {
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
    Lopo <- Lopo %>%
      dplyr::filter(listItem_outputId == spec_output)

    Lopa <- Lopa %>%
      dplyr::filter(listItem_outputId == spec_output)
  }

  # Prework and loops ----------------------------------------------------

  for (i in 1:nrow(Lopo)) {
    Output <- Lopo[i, ]$listItem_outputId
    OutputName <- Lopo[i, ]$listItem_name

    Anas <- Lopa %>% # get all analyses for current output
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
    analysis_set_code <- list()
    data_subset_code <- list()
    analysis_bind_targets <- character()

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

      # Analysis Grouping
      groupid1 <- Anas_s$groupingId1 # group ID (to be used in AG)
      resultsByGroup1 <- Anas_s$resultsByGroup1 # Y/N group by
      groupid2 <- Anas_s$groupingId2
      resultsByGroup2 <- Anas_s$resultsByGroup2
      groupid3 <- Anas_s$groupingId3
      resultsByGroup3 <- Anas_s$resultsByGroup3

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

      analysis_set_code[[Anas_j]] <- analysis_set_result$code
      AnSetDataSubsets <- analysis_set_result$data_subset
      # Apply Grouping ----------------------------

      # determine maximum groupings
      column_names <- colnames(Analyses)

      # Filter column names that match the pattern "Group"
      group_columns <- column_names[grep("^groupingId[0-9]+$", column_names)]

      # Extract the numeric part of the group columns and find the maximum
      group_numbers <- as.numeric(sub("groupingId", "", group_columns))
      max_group_number <- max(group_numbers)

      # consider denominator analysis
      NUM_analysisid <- Anas_s$referencedAnalysisOperations_analysisId1
      DEN_analysisid <- Anas_s$referencedAnalysisOperations_analysisId2

      AG_denom_id <- Analyses %>%
        dplyr::filter(id == DEN_analysisid) %>%
        dplyr::select(groupingId1) %>%
        unique() %>%
        as.character()

      AG_denom_temp1 <- AnalysisGroupings %>%
        dplyr::filter(id == AG_denom_id)

      AG_denom_var1 <- AG_denom_temp1 %>%
        dplyr::select(groupingVariable) %>%
        unique() %>%
        as.character()

      if (AG_denom_var1 %in% c(NA, "NA", "")) {
        cli::cli_alert("Metadata issue in AnalysisGroupings {groupid1}: AnalysisGrouping has missing groupingVariable")
      }

      if (max_group_number >= 1) {
        AG_temp1 <- AnalysisGroupings %>%
          dplyr::filter(id == groupid1)

        AG_var1 <- AG_temp1 %>%
          dplyr::select(groupingVariable) %>%
          unique() %>%
          as.character()

        AG_ds1 <- AG_temp1 %>%
          dplyr::select(groupingDataset) %>%
          unique() %>%
          as.character()

        AG_1_DataDriven <- AG_temp1 %>%
          dplyr::select(dataDriven) %>%
          unique() %>%
          as.character()

        # get the number of dplyr::group_by to perform in Grouping Apply
        if (resultsByGroup1 == TRUE && !is.na(resultsByGroup1)) {
          num_grp <- 1
          AG_max_dataDriven <- AG_1_DataDriven
        } else {
          num_grp <- 0
        }

        if (max_group_number >= 2) {
          AG_temp2 <- AnalysisGroupings %>%
            dplyr::filter(id == groupid2)

          AG_var2 <- AG_temp2 %>%
            dplyr::select(groupingVariable) %>%
            unique() %>%
            as.character()

          AG_ds2 <- AG_temp2 %>%
            dplyr::select(groupingDataset) %>%
            unique() %>%
            as.character()

          AG_2_DataDriven <- AG_temp2 %>%
            dplyr::select(dataDriven) %>%
            unique() %>%
            as.character()

          # get the number of dplyr::group_by to perform in Grouping Apply
          if (resultsByGroup1 == TRUE && !is.na(resultsByGroup1)) {
            num_grp <- 1
            AG_max_dataDriven <- AG_1_DataDriven
            if (resultsByGroup2 == TRUE && !is.na(resultsByGroup2)) {
              num_grp <- 2
              AG_max_dataDriven <- AG_2_DataDriven
            }
          } else {
            num_grp <- 0
          }

          if (max_group_number >= 3) {
            AG_temp3 <- AnalysisGroupings %>%
              dplyr::filter(id == groupid3)

            AG_var3 <- AG_temp3 %>%
              dplyr::select(groupingVariable) %>%
              unique() %>%
              as.character()

            AG_ds3 <- AG_temp3 %>%
              dplyr::select(groupingDataset) %>%
              unique() %>%
              as.character()

            AG_3_DataDriven <- AG_temp3 %>%
              dplyr::select(dataDriven) %>%
              unique() %>%
              as.character()

            # get the number of dplyr::group_by to perform in Grouping Apply
            if (resultsByGroup1 == TRUE && !is.na(resultsByGroup1)) {
              num_grp <- 1
              AG_max_dataDriven <- AG_1_DataDriven
              if (resultsByGroup2 == TRUE && !is.na(resultsByGroup2)) {
                num_grp <- 2
                AG_max_dataDriven <- AG_2_DataDriven
                if (resultsByGroup3 == TRUE && !is.na(resultsByGroup3)) {
                  num_grp <- 3
                  AG_max_dataDriven <- AG_3_DataDriven
                }
              }
            } else {
              num_grp <- 0
            }

            # get number of group_by for non-grouped operations
            if (!is.na(resultsByGroup1)) {
              num_grp_any <- 1
              if (!is.na(resultsByGroup2)) {
                num_grp_any <- 2
                if (!is.na(resultsByGroup3)) {
                  num_grp_any <- 3
                }
              }
            } else {
              num_grp_any <- 0
            }
          }
        }
      }

      if (num_grp == 1) {
        # cards part
        distinct_list <- paste0(AG_var1, ", ", ana_var)
        by_listc <- paste0("'", AG_var1, "'")
        by_list <- paste0(AG_var1)
        by_stmt <- paste0(", by = ", AG_var1)


        by_vars <- paste0(", variables = '", AG_var1, "'")
        strata_vars <- paste0(", variables = '", AG_var1, "'")
      } else if (num_grp == 2) {
        # cards part
        distinct_list <- paste0(AG_var1, ", ", AG_var2, ", ", ana_var)
        by_listc <- paste0("'", AG_var1, "', '", AG_var2, "'")
        by_list <- paste0(AG_var1, ", ", AG_var2)

        by_vars <- paste0(
          ", by = '",
          AG_var1,
          "' , variables = '",
          AG_var2, "'"
        )

        strata_vars <- paste0(
          ", strata = '",
          AG_var1,
          "' , variables = '",
          AG_var2, "'"
        )
      } else if (num_grp == 3) {
        # cards part
        distinct_list <- paste0(AG_var1, ", ", AG_var2, ", ", AG_var3, ", ", ana_var)
        by_listc <- paste0("'", AG_var1, "', '", AG_var2, "', '", AG_var3, "'")
        by_list <- paste0(AG_var1, ", ", AG_var2, ", ", AG_var3)

        by_vars <- paste0(
          ", by = c('",
          AG_var1,
          "', '",
          AG_var2,
          "') , variables = '",
          AG_var3, "'"
        )

        strata_vars <- paste0(
          ", strata = c('",
          AG_var1,
          "', '",
          AG_var2,
          "') , variables = '",
          AG_var3, "'"
        )
      } else { # no grouping being done
        distinct_list <- ana_var
        by_stmt <- ""
      }

      # Apply DataSubset -------------------------------------------------------------

      df2_sym <- rlang::sym(paste0("df2_", Anas_j))
      source_sym <- rlang::sym(AnSetDataSubsets)

      has_data_subsets <- !is.null(DataSubsets) && nrow(DataSubsets) > 0

      if (has_data_subsets && !is.na(subsetid)) {
        subsetrule <- DataSubsets |> dplyr::filter(id == subsetid)

        if (nrow(subsetrule) == 0) {
          cli::cli_warn("No DataSubset rules found for {subsetid}; defaulting to analysis set data")

          assignment_expr <- rlang::expr(!!df2_sym <- !!source_sym)

          data_subset_code[[Anas_j]] <- paste0(
            "# Apply Data Subset ---\n",
            .expr_to_code(assignment_expr),
            "\n"
          )
        } else {
          DSname <- subsetrule |>
            dplyr::pull(name) |>
            unique() |>
            as.character()

          DSname <- DSname[!is.na(DSname)]
          DSname <- DSname[seq_len(min(length(DSname), 1L))]
          DSname <- gsub("[\\r\\n]", " ", DSname)

          filter_exprs <- .build_subset_filter_expressions(subsetrule, file_ext)

          pipeline_rhs <- if (length(filter_exprs) > 0) {
            rlang::inject((!!source_sym) |> dplyr::filter(!!!filter_exprs))
          } else {
            source_sym
          }

          assignment_expr <- rlang::expr(!!df2_sym <- !!pipeline_rhs)

          ds_comment <- if (length(DSname) == 1 && !identical(DSname, "")) {
            glue::glue("# Data subset: {DSname}\n", .trim = FALSE)
          } else {
            ""
          }

          data_subset_code[[Anas_j]] <- paste0(
            "# Apply Data Subset ---\n",
            ds_comment,
            .expr_to_code(assignment_expr),
            "\n"
          )
        }
      } else {
        assignment_expr <- rlang::expr(!!df2_sym <- !!source_sym)

        data_subset_code[[Anas_j]] <- paste0(
          "# Apply Data Subset ---\n",
          .expr_to_code(assignment_expr),
          "\n"
        )
      }

      # Apply AnalysisMethod -------------------------------------------------------------

      method <- AnalysisMethods %>%
        dplyr::filter(id == methodid) %>% # refnew
        dplyr::select(name, description, label, id) %>%
        unique()

      operations <- AnalysisMethods %>%
        dplyr::filter(id == methodid) %>%
        dplyr::select(operation_id)

      operation <- operations$operation_id
      for (i in seq_len(nrow(operations))) {
        assign(
          paste0("operation_", i),
          operation[i]
          # ,
          # envir = .GlobalEnv
        )
      }

      methodname <- method$name
      methoddesc <- method$description
      methodlabel <- method$label
      methodid <- method$id

      # Code
      anmetcode <- AnalysisMethodCodeTemplate %>%
        dplyr::filter(
          method_id == methodid,
          context %in% c("R", "R (siera)", "siera"),
          specifiedAs == "Code"
        ) %>%
        dplyr::select(templateCode)

      # Parameters
      # to be replaced with Source values:
      anmetparam_s <- AnalysisMethodCodeParameters %>%
        dplyr::filter(
          method_id == methodid,
          parameter_valueSource != ""
        )

      # operations to transpose with
      operation_list <- AnalysisMethods %>%
        dplyr::filter(id == methodid) %>% # refnew
        dplyr::select(operation_id)

      operation_list_string <- paste(operation_list$operation_id,
        collapse = ", "
      )


      # refnew
      # intro part

      code_method_tmp_1 <- .render_template(
        "# Method ID:              {methodid}\n# Method name:            {methodname}\n# Method description:     {methoddesc}\n",
        methodid = methodid,
        methodname = methodname,
        methoddesc = methoddesc
      )

      # code part

      ## using for loop
      anmetcode_temp <- .render_template(
        "if (nrow(df2_{analysis_id}) != 0) {{\n{code}\n}}",
        analysis_id = Anas_j,
        code = anmetcode
      )

      for (i in seq_len(nrow(anmetparam_s))) {
        # Get the replacement value using get() based on the variable name in Column B

        rep <- rlang::env_get(
          env = environment(),
          nm = anmetparam_s$parameter_valueSource[i],
          default = NA,
          inherit = TRUE
        )
        # Replace the placeholder in VAR with the variable's value
        if (!is.na(rep)) {
          anmetcode_temp <- gsub(
            anmetparam_s$parameter_name[i],
            rep,
            anmetcode_temp
          )
        }
      }
      anmetcode_final <- gsub("methodidhere", methodid, anmetcode_temp)
      anmetcode_final <- gsub("analysisidhere", Anas_j, anmetcode_final)

      code_method_tmp_2 <- anmetcode_final

      # mutate part
      df3_sym <- rlang::sym(paste0("df3_", Anas_j))
      analysis_id_value <- as.character(Anas_j)
      method_id_value <- as.character(methodid)
      output_value <- as.character(Output)

      mutate_expr <- rlang::expr(
        if (nrow(!!df2_sym) != 0) {
          !!df3_sym <- !!df3_sym |>
            dplyr::mutate(
              AnalysisId = !!analysis_id_value,
              MethodId = !!method_id_value,
              OutputId = !!output_value
            )
        } else {
          !!df3_sym <- data.frame(
            AnalysisId = !!analysis_id_value,
            MethodId = !!method_id_value,
            OutputId = !!output_value
          )
        }
      )

      code_method_tmp_3 <- paste0(.expr_to_code(mutate_expr), "\n")

      code_method <- paste0(
        code_method_tmp_1, "\n",
        code_method_tmp_2,
        code_method_tmp_3
      )


      # code to combine it all --------------------------------------------------
      # dplyr::rename groups to append
      grouping_vars <- c(AG_var1, AG_var2, AG_var3)
      grouping_vars <- grouping_vars[seq_len(min(num_grp, length(grouping_vars)))]
      grouping_vars <- grouping_vars[!is.na(grouping_vars) & grouping_vars != ""]

      if (length(grouping_vars) > 0) {
        rename_expr <- rlang::expr(
          dplyr::rename(!!!rlang::set_names(rlang::syms(grouping_vars), paste0("Group", seq_along(grouping_vars))))
        )
        code_rename <- paste0(" |>\n", .expr_to_code(rename_expr, indent = 8L))
      } else {
        code_rename <- ""
      }

      method_code_block <- paste0(
        "#Apply Method --- \n",
        code_method
      )

      # Generate code for analysis ----------------------------------------------

      analysis_block <- paste0(
        "\n\n# Analysis ", Anas_j, "----\n#",
        ana_name,
        analysis_set_code[[Anas_j]],
        data_subset_code[[Anas_j]],
        method_code_block
      )

      run_code <- paste0(run_code, analysis_block)
      analysis_bind_targets <- c(analysis_bind_targets, paste0("df3_", Anas_j))
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
    combine_code <- ""
    if (length(analysis_bind_targets) > 0) {
      analysis_syms <- rlang::syms(unique(analysis_bind_targets))
      combine_expr <- rlang::expr(ARD <- dplyr::bind_rows(!!!analysis_syms))
      combine_code <- paste0(
        "\n\n# combine analyses to create ARD ----\n",
        .expr_to_code(combine_expr),
        "\n"
      )
    }

    final_code <- paste0(
      code_header,
      code_libraries,
      code_ADaM,
      run_code,
      combine_code
    )

    writeLines(
      final_code,
      file.path(output_path, paste0("ARD_", Output, ".R"))
    )
  } # end of outputs
}
