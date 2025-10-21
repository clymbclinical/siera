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

      assign(paste0("code_AnalysisSet_", Anas_j), analysis_set_result$code)
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
        cli::cli_alert("Metadata issue in AnalysisGroupings {groupid1}: AnalysisGrouping has missing groupingVariable")
      }

      if (max_group_number >= 1) {
        AG_temp1 <- AnalysisGroupings |>
          dplyr::filter(id == groupid1)

        AG_var1 <- AG_temp1 |>
          dplyr::select(groupingVariable) |>
          unique() |>
          as.character()

        AG_ds1 <- AG_temp1 |>
          dplyr::select(groupingDataset) |>
          unique() |>
          as.character()

        AG_1_DataDriven <- AG_temp1 |>
          dplyr::select(dataDriven) |>
          unique() |>
          as.character()

        # get the number of dplyr::group_by to perform in Grouping Apply
        if (resultsByGroup1 == TRUE && !is.na(resultsByGroup1)) {
          num_grp <- 1
          AG_max_dataDriven <- AG_1_DataDriven
        } else {
          num_grp <- 0
        }

        if (max_group_number >= 2) {
          AG_temp2 <- AnalysisGroupings |>
            dplyr::filter(id == groupid2)

          AG_var2 <- AG_temp2 |>
            dplyr::select(groupingVariable) |>
            unique() |>
            as.character()

          AG_ds2 <- AG_temp2 |>
            dplyr::select(groupingDataset) |>
            unique() |>
            as.character()

          AG_2_DataDriven <- AG_temp2 |>
            dplyr::select(dataDriven) |>
            unique() |>
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
            AG_temp3 <- AnalysisGroupings |>
              dplyr::filter(id == groupid3)

            AG_var3 <- AG_temp3 |>
              dplyr::select(groupingVariable) |>
              unique() |>
              as.character()

            AG_ds3 <- AG_temp3 |>
              dplyr::select(groupingDataset) |>
              unique() |>
              as.character()

            AG_3_DataDriven <- AG_temp3 |>
              dplyr::select(dataDriven) |>
              unique() |>
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
        by_stmt <- paste0(", by = ", AG_var1, ", ", AG_var2)

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
        by_stmt <- paste0(", by = ", AG_var1, ", ", AG_var2, ", ", AG_var3)

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
        cont_by_stmt <- ""
      }

      # Apply DataSubset -------------------------------------------------------------
      data_subset_result <- .generate_data_subset_code(
        data_subsets = DataSubsets,
        subset_id = subsetid,
        analysis_id = Anas_j,
        analysis_set_dataset = AnSetDataSubsets,
        file_ext = file_ext
      )

      assign(
        paste0("code_DataSubset_", Anas_j),
        data_subset_result$code
      )

      # Apply AnalysisMethod -------------------------------------------------------------

      analysis_method_result <- .generate_analysis_method_section(
        analysis_methods = AnalysisMethods,
        analysis_method_code_template = AnalysisMethodCodeTemplate,
        analysis_method_code_parameters = AnalysisMethodCodeParameters,
        method_id = methodid,
        analysis_id = Anas_j,
        output_id = Output,
        envir = environment()
      )

      code_method <- analysis_method_result$code

      method <- analysis_method_result$method
      methodname <- method$name
      methoddesc <- method$description
      methodlabel <- method$label
      methodid <- method$id

      # code to combine it all --------------------------------------------------
      # dplyr::rename groups to append
      if (num_grp == 1) { # if 1 analysis grouping
        func_rename1 <- function(groupvar1) {
          template <- " |>
        dplyr::rename(Group1 = groupvar1here)
"
          code <- gsub("groupvar1here", groupvar1, template)

          return(code)
        }

        code_rename <- func_rename1(AG_var1)
      } else if (num_grp == 2) { # if 2 analysis groupings
        func_rename2 <- function(groupvar1,
                                 groupvar2) {
          template <- " |>
        dplyr::rename(Group1 = groupvar1here,
               Group2 = groupvar2here)
"
          code <- gsub("groupvar1here", groupvar1, template)
          code <- gsub("groupvar2here", groupvar2, code)

          return(code)
        }
        code_rename <- func_rename2(
          AG_var1,
          AG_var2
        )
      } else if (num_grp == 3) { # if 3 analysis groupings
        func_rename3 <- function(groupvar1,
                                 groupvar2,
                                 groupvar3) {
          template <- " |>
        dplyr::rename(Group1 = groupvar1here,
               Group2 = groupvar2here,
               Group3 = groupvar3here)
"
          code <- gsub("groupvar1here", groupvar1, template)
          code <- gsub("groupvar2here", groupvar2, code)
          code <- gsub("groupvar3here", groupvar3, code)

          return(code)
        }

        code_rename <- func_rename3(
          AG_var1,
          AG_var2,
          AG_var3
        )
      } else {
        code_rename <- ""
      } # if no analysis grouping


      assign(
        paste0("code_AnalysisMethod_", Anas_j),
        paste0(
          "#Apply Method --- \n",
          code_method # ,
          # code_rename
        )
      )

      # Generate code for analysis ----------------------------------------------

      assign(
        paste0("code_", Anas_j),
        paste0(
          "\n\n# Analysis ", Anas_j, "----\n#",
          ana_name,
          get(paste0("code_AnalysisSet_", Anas_j)),
          # get(paste0("code_AnalysisGrouping_",Anas_j)),
          get(paste0("code_DataSubset_", Anas_j)),
          get(paste0("code_AnalysisMethod_", Anas_j))
        )
      )

      run_code <- paste0(
        run_code,
        get(paste0("code_", Anas_j))
      )

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
    assign(
      paste0("code_", Output),
      paste0(
        code_header,
        code_libraries,
        code_ADaM,
        run_code,
        "\n\n# combine analyses to create ARD ----\n",
        "ARD <- dplyr::bind_rows(",
        combine_analysis_code,
        ") "
      )
    )

    # script_code <- get(paste0("code_", Output))

    # styled_script_code <- styler::style_text(script_code)
    # # styled_script_code <- tryCatch(
    # #   styler::style_text(script_code),
    # #   error = function(err) {
    # #     warning(
    # #       paste(
    # #         "Unable to style generated ARD script for",
    # #         Output,
    # #         "- writing unstyled code.",
    # #         conditionMessage(err)
    # #       )
    # #     )
    # #     script_code
    # #   }
    # # )

    writeLines(
      get(paste0("code_", Output)),
      # styled_script_code,
      paste0(output_path, "/ARD_", Output, ".R")
    )
  } # end of outputs
}
