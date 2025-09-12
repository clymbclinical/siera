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
#' @param spec_analysis The analysis ID for a specific analysis to be run
#' from the metadata
#' @param example Default is FALSE.  If TRUE, example-based operations will
#'  be applied to CDISC example ARS. If FALSE, AnalysisMethodCodeTemplateCode
#'  will be expected as source of the Method (and Operations) code
#' @param shuffle Default is FALSE.  If TRUE, shuffle_ard() is applied to the
#'  binded final ARD, to prepare for the tfrmt use case of table generation
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
#' output_dir = tempdir()
#'
#' # folder containing ADaM datasets
#' adam_folder = tempdir()
#'
#' # run function, write to temp directory
#' readARS(ARS_path, output_dir, adam_folder)
#'

readARS <- function(ARS_path,
                     output_path = tempdir(),
                     adam_path = tempdir(),
                     spec_output = "",
                     spec_analysis = "",
                     example = FALSE,
                     shuffle = FALSE){

  func_libraries <- function(){

    if(example == FALSE){

      template <- "

# load libraries ----
library(dplyr)
library(readxl)
library(readr)
library(cards)
library(cardx)
library(broom)
library(parameters)
library(tidyr)
library(magrittr)
  "
    } else {

      template <- "

# load libraries ----
library(tidyverse)
library(readxl)
library(splitstackshape)
library(readr)
  "
    }
    code <- template
    return(code)
  }

  code_libraries <- func_libraries()


  # Read in ARS metadata ----------------------------------------------------

  # Get file extension (case-insensitive)
  file_ext <- tolower(tools::file_ext(ARS_path))

  if (!file_ext %in% c("json", "xlsx")) {
    cli::cli_warn(
      "Input ARS file must be JSON or xlsx; {.path {ARS_path}} was received"
    )
    return(invisible(NULL))
  }

  # Read in JSON metadata
  if (file_ext == "json") {
    json_from <- jsonlite::fromJSON(ARS_path)

    #otherListsOfContents (LOPO) --V1ized
    otherListsOfContents <- json_from$otherListsOfContents$contentsList$listItems[[1]]  # this is similar to xlsx
    Lopo <- otherListsOfContents |>
      dplyr::rename(listItem_outputId = outputId,
                    listItem_name = name,
                    listItem_order = order,
                    listItem_level = level)# list of all planned outputs to loop JSONized

    #mainListOfContents --V1ized
    mainListOfContents <- json_from$mainListOfContents$contentsList$listItems

    # loop through outputs to construct LOPA
    Lopa <- data.frame()
    for(a in 1:nrow(otherListsOfContents)){
      tmp_PO <- otherListsOfContents[a,]

      tmp_json_Lopa <-  # contains list with datasets with analysisIDs
        mainListOfContents$sublist$listItems[[a]]

      # gather anaIDs from anaysisID (Level 2)
      anaIds <- tmp_json_Lopa$analysisId |>
        tibble::as_tibble() |>
        dplyr::mutate(listItem_outputId = tmp_PO$outputId)

      if(nrow(anaIds) > 0){
        anaIds = anaIds |>
          dplyr::rename(listItem_analysisId = value) |>
          dplyr::filter(!is.na(listItem_analysisId))
      }

      # bind analysisIDs
      Lopa <- rbind(Lopa, anaIds)

      # gather level 3 AnaIDs
      if("sublist" %in% names(tmp_json_Lopa)){ # check if there are level 3's

        tmp_json_lopa_sub <- tmp_json_Lopa$sublist$listItems

        forend <- length(tmp_json_lopa_sub) # amount of analyses datasets in json_lopa
        subana_dset <- data.frame() # initialise dataframe to contain datasets
        for(b in 2:forend){   # always(?) 1st row is empty
          ana_ids <- tmp_json_lopa_sub[[b]]$analysisId |>
            tibble::as_tibble() |>
            dplyr::mutate(listItem_outputId = tmp_PO$outputId) |>
            dplyr::rename(listItem_analysisId = value)
          subana_dset <- rbind(subana_dset, ana_ids)
        }
        Lopa <- rbind(Lopa, subana_dset)
      }
    }

    #dataSubsets
    JSON_DataSubsets <- json_from$dataSubsets
    # level 1
    JSONDSL1 <- tibble::tibble(id = json_from$dataSubsets[["id"]],
                               name = json_from$dataSubsets[["name"]],
                               label = json_from$dataSubsets[["label"]],
                               order = json_from$dataSubsets[["order"]],
                               level = json_from$dataSubsets[["level"]],
                               condition_dataset = json_from[["dataSubsets"]][["condition"]][["dataset"]],
                               condition_variable = json_from[["dataSubsets"]][["condition"]][["variable"]],
                               condition_comparator = json_from[["dataSubsets"]][["condition"]][["comparator"]],
                               condition_value = json_from[["dataSubsets"]][["condition"]][["value"]],
                               compoundExpression_logicalOperator = json_from[["dataSubsets"]][["compoundExpression"]][["logicalOperator"]])

    # level 2
    # loop through level 1
    whereClauses <- JSON_DataSubsets[["compoundExpression"]][["whereClauses"]]
    JSONDSL2 <- data.frame()
    JSONDSL3 <- data.frame()
    for(c in 1:nrow(JSON_DataSubsets)){  # loop through level 1
      # for(c in 5:5){
      tmp_DSID <- JSON_DataSubsets[c, "id"]
      tmp_DSname <- JSON_DataSubsets[c, "name"]
      tmp_DSlabel <- JSON_DataSubsets[c, "label"]
      tmp_DS_c <- JSON_DataSubsets[c,]

      if(!is.null(whereClauses[[c]])){ # check for level 2 existence

        tmp_DS <- tibble::tibble(level =  whereClauses[[c]][["level"]],
                                 order = whereClauses[[c]][["order"]],
                                 condition_dataset = whereClauses[[c]][["condition"]][["dataset"]],
                                 condition_variable = whereClauses[[c]][["condition"]][["variable"]],
                                 condition_comparator = whereClauses[[c]][["condition"]][["comparator"]],
                                 condition_value = whereClauses[[c]][["condition"]][["value"]],
                                 compoundExpression_logicalOperator =  whereClauses[[c]]$compoundExpression$logicalOperator,
                                 id = tmp_DSID,
                                 name = tmp_DSname,
                                 label = tmp_DSlabel)
        JSONDSL2 = dplyr::bind_rows(JSONDSL2,tmp_DS)

        whereClausesL2 <- whereClauses[[c]][["compoundExpression"]][["whereClauses"]]
        for(d in 1:nrow(tmp_DS)){ # loop through level 2

          if (!is.null(whereClausesL2[[d]])) { # check for level 3 existence
            tmp_DSL2 <- tibble::tibble(level =  whereClausesL2[[d]][["level"]],
                                       order = whereClausesL2[[d]][["order"]],
                                       condition_dataset = whereClausesL2[[d]][["condition"]][["dataset"]],
                                       condition_variable = whereClausesL2[[d]][["condition"]][["variable"]],
                                       condition_comparator = whereClausesL2[[d]][["condition"]][["comparator"]],
                                       condition_value = whereClausesL2[[d]][["condition"]][["value"]],
                                       id = tmp_DSID,
                                       name = tmp_DSname,
                                       label = tmp_DSlabel)

            JSONDSL3 = dplyr::bind_rows(JSONDSL3,tmp_DSL2)
          }
        }
      }
    }

    DataSubsets <- dplyr::bind_rows(JSONDSL1, JSONDSL2, JSONDSL3) |>
      dplyr::arrange(id, level, order) # --JSONIZED! cHECK DIFFERENCE IN CONDITION_VALUE

    DataSubsets$condition_value[DataSubsets$condition_value == 'NULL'] = NA

    AnalysisSets <- tibble::tibble(id = json_from$analysisSets$id,
                                   label = json_from$analysisSets$label,
                                   name = json_from$analysisSets$name,
                                   level = json_from$analysisSets$level,
                                   order = json_from$analysisSets$order,
                                   condition_dataset = json_from$analysisSets$condition[["dataset"]],
                                   condition_variable = json_from$analysisSets$condition[["variable"]],
                                   condition_comparator = json_from$analysisSets$condition[["comparator"]],
                                   condition_value = json_from$analysisSets$condition[["value"]])

    # AG
    JSON_AnalysisGroupings <-  json_from$analysisGroupings

    JSON_AG_1 <- tibble::tibble(id = json_from$analysisGroupings$id,
                                name = json_from$analysisGroupings$name,
                                groupingDataset = json_from$analysisGroupings$groupingDataset,
                                groupingVariable = json_from$analysisGroupings$groupingVariable,
                                dataDriven = json_from$analysisGroupings$dataDriven)

    JSON_AG <- data.frame()
    for(e in 1: nrow(JSON_AG_1)){

      AG_ID <- JSON_AG_1[e, "id"] %>% as.character()
      AG_name <- JSON_AG_1[e, "name"] %>% as.character()
      AG_groupingVariable <- JSON_AG_1[e, "groupingVariable"] %>% as.character()
      AG_groupingDataset <- JSON_AG_1[e, "groupingDataset"] %>% as.character()
      AG_dataDriven <- JSON_AG_1[e, "dataDriven"] %>% as.character()

      tmp_AG <- tibble::tibble(group_id = JSON_AnalysisGroupings[["groups"]][[e]]$id,
                               group_name = JSON_AnalysisGroupings[["groups"]][[e]]$name,
                               group_level = JSON_AnalysisGroupings[["groups"]][[e]]$level,
                               group_order = JSON_AnalysisGroupings[["groups"]][[e]]$order,
                               group_condition_dataset = JSON_AnalysisGroupings[["groups"]][[e]]$condition[["dataset"]],
                               group_condition_variable = JSON_AnalysisGroupings[["groups"]][[e]]$condition[["variable"]],
                               group_condition_comparator = JSON_AnalysisGroupings[["groups"]][[e]]$condition[["comparator"]],
                               group_condition_value = JSON_AnalysisGroupings[["groups"]][[e]]$condition[["value"]],
                               id = AG_ID,
                               name = AG_name,
                               groupingVariable = AG_groupingVariable,
                               groupingDataset = AG_groupingDataset,
                               dataDriven = AG_dataDriven)

      JSON_AG <- dplyr::bind_rows(JSON_AG, tmp_AG)
    }


    AnalysisGroupings <- dplyr::bind_rows(JSON_AG) # JSONIZED!  (without grouping_type and Logical Operator)



    # Analyses
    JSON_AN <- json_from$analyses

    JSON_AnalysesL1 <-  tibble::tibble(id = JSON_AN$id,
                                       name = JSON_AN$name,
                                       label = JSON_AN$label,
                                       version = JSON_AN$version,
                                       categoryIds = JSON_AN$categoryIds,
                                       method_id = JSON_AN$methodId,
                                       analysisSetId = JSON_AN$analysisSetId,
                                       dataset = JSON_AN$dataset,
                                       variable = JSON_AN$variable,
                                       dataSubsetId = JSON_AN$dataSubsetId
    )

    # AN groupings
    AN_groupings <- data.frame()
    for(g in 1:nrow(JSON_AnalysesL1)){

      tmp_id <- JSON_AN[g,]$id %>% as.character()

      tmp <- JSON_AN[["orderedGroupings"]][[g]] %>%
        tidyr::pivot_wider(
          names_from = order,
          values_from = c(resultsByGroup, groupingId),
          names_glue = "{.value}{order}"
        ) %>%
        dplyr::mutate(id = tmp_id)

      AN_groupings <- dplyr::bind_rows(AN_groupings, tmp)
    }

    # AN refs
    AN_refs <- data.frame()
    for(h in 1:nrow(JSON_AnalysesL1)){

      tmp_id <- JSON_AN[h,]$id %>% as.character()

      if(!is.null(JSON_AN[["referencedAnalysisOperations"]][[h]])){
        tmp_ref <- JSON_AN[["referencedAnalysisOperations"]][[h]] %>%
          dplyr::mutate(order = dplyr::row_number()) %>%
          tidyr::pivot_wider(
            names_from = order,
            values_from = c(referencedOperationRelationshipId, analysisId),
            names_glue = "{'referencedAnalysisOperations_'}{.value}{order}"
          ) %>%
          dplyr::mutate(id = tmp_id)

        AN_refs <- dplyr::bind_rows(AN_refs, tmp_ref)
      }
    }
    colnames(AN_refs) <- gsub("Relationship", "", colnames(AN_refs))

    #merge Analyses
    Analyses <- merge(JSON_AnalysesL1,  #JSONIZED
                      AN_refs,
                      by = "id",
                      all.x = TRUE) %>%
      merge(AN_groupings,
            by = "id",
            all.x = TRUE) %>%
      dplyr::filter(!is.na(method_id),
                    method_id != "") # exclude if not methodid

    # JSON AM_L1
    JSONAML1 <- tibble::tibble(id = json_from$methods$id,
                               name = json_from$methods$name,
                               description = json_from$methods$description,
                               label = json_from$methods$label
    )

    # JSON AML2
    JSONAML2 <- data.frame()
    JSONAML3 <- data.frame()
    for(i in 1:nrow(JSONAML1)){

      tmp_l2 <- tibble::tibble(operation_id = json_from$methods$operations[[i]]$id, # operation info
                               operation_name = json_from$methods$operations[[i]]$name,
                               operation_resultPattern = json_from$methods$operations[[i]]$resultPattern,
                               operation_label = json_from$methods$operations[[i]]$label,
                               operation_order = json_from$methods$operations[[i]]$order,
                               id = JSONAML1[i,]$id %>% as.character()
      )
      JSONAML2 <- dplyr::bind_rows(JSONAML2, tmp_l2)


      # check for and add referenced...
      rOF <- json_from$methods$operations[[i]]$referencedOperationRelationships
      if(!is.null(rOF)) {

        lenrOF <- length(rOF)

        for(j in 1:lenrOF){

          if(!is.null(rOF[[j]])){

            tmp_l3 <- tibble::tibble(id =  rOF[[j]]$id,
                                     operationId = rOF[[j]]$operationId,
                                     description = rOF[[j]]$description,
                                     referencedOperationRole = rOF[[j]]$referencedOperationRole$controlledTerm)

            tmp_l3_fin <- tmp_l3 %>%
              dplyr::mutate(order = dplyr::row_number()) %>%
              tidyr::pivot_wider(
                names_from = order,
                values_from = c(id, operationId, description, referencedOperationRole),
                names_glue = "{'operation_referencedResultRelationships'}{order}{'_'}{.value}"
              ) %>%
              dplyr::mutate(operation_id = json_from[["methods"]][["operations"]][[i]][["id"]][[j]])

            JSONAML3 = dplyr::bind_rows(JSONAML3,tmp_l3_fin)
          }
        }
      }
    }

    # AM
    AnalysisMethods <- merge(JSONAML1,
                             JSONAML2,
                             by = "id",
                             all = TRUE) %>%
      merge(JSONAML3,
            by = "operation_id",
            all = TRUE)

    #AMC
    AnalysisMethodCodeTemplate <- tibble::tibble(method_id = json_from$methods$id,
                                                 context = json_from$methods$codeTemplate$context,
                                                 specifiedAs = "Code",
                                                 templateCode = json_from$methods$codeTemplate$code)


    AnalysisMethodCodeParameters <- data.frame()
    for(i in 1:nrow(JSONAML1)){
      id = JSONAML1[i,]$id %>% as.character()
      tmp_AMCP <- tibble::tibble(method_id = id,
                                 parameter_name = json_from$methods$codeTemplate$parameters[[i]]$name,
                                 parameter_description = json_from$methods$codeTemplate$parameters[[i]]$description,
                                 parameter_valueSource = json_from$methods$codeTemplate$parameters[[i]]$valueSource
      )

      AnalysisMethodCodeParameters <- dplyr::bind_rows(AnalysisMethodCodeParameters,
                                                       tmp_AMCP)
    }

  } else if (file_ext == "xlsx") {

    ARS_xlsx = ARS_path
    mainListOfContents <- read_excel(ARS_xlsx,
                                     sheet = 'MainListOfContents')
    otherListsOfContents <- read_excel(ARS_xlsx,
                                       sheet = 'OtherListsOfContents')
    DataSubsets <- read_excel(ARS_xlsx,
                              sheet = 'DataSubsets')
    AnalysisSets <- read_excel(ARS_xlsx,
                               sheet = 'AnalysisSets')
    AnalysisGroupings <- read_excel(ARS_xlsx,
                                    sheet = 'AnalysisGroupings')
    Analyses <- read_excel(ARS_xlsx,
                           sheet = 'Analyses') %>%
      dplyr::filter(!is.na(method_id)) # exclude if not methodid
    AnalysisMethods <- read_excel(ARS_xlsx,
                                  sheet = 'AnalysisMethods')
    AnalysisMethods <- read_excel(ARS_xlsx,
                                  sheet = 'AnalysisMethods')
    AnalysisMethodCodeTemplate <- read_excel(ARS_xlsx,
                                             sheet = 'AnalysisMethodCodeTemplate')
    AnalysisMethodCodeParameters <- read_excel(ARS_xlsx,
                                               sheet = 'AnalysisMethodCodeParameters')

    Lopo <- otherListsOfContents # list of all planned outputs to loop

    Lopa <- mainListOfContents %>%  # list of all planned analyses to loop
      tidyr::fill(listItem_outputId) %>%
      dplyr::filter(!is.na(listItem_analysisId)) %>%
      dplyr::select(listItem_analysisId, listItem_outputId)
  }

  # Handle specific outputs or analyses -------------

  # specific output
  if(spec_output != ""){
    Lopo <- Lopo %>%
      dplyr::filter(listItem_outputId == spec_output)

    Lopa <- Lopa %>%
      dplyr::filter(listItem_outputId == spec_output)
  }

  # specific analysis
  if(spec_analysis != ""){
    Lopa <- Lopa %>%
      dplyr::filter(listItem_analysisId == spec_analysis)

    output_ded = Lopa %>%
      dplyr::select(listItem_outputId) %>%
      unique() %>%
      as.character()

    Lopo <- Lopo %>%
      dplyr::filter(listItem_outputId == output_ded)
  }

  # Prework and loops ----------------------------------------------------

  for (i in 1:nrow(Lopo)) {
    Output = Lopo[i,]$listItem_outputId
    OutputName = Lopo[i,]$listItem_name

    Anas <- Lopa %>%    # get all analyses for current output
      dplyr::filter(listItem_outputId == Output,
                    listItem_analysisId %in% Analyses$id) # exclude if not methodid

    # Load ADaMs ----
    a1 <- ""

    # Combine unique datasets from both dataframes
    Analyses_IDs <- Analyses %>%
      dplyr::filter(id %in% Anas$listItem_analysisId)

    Analyses_ADaMs <- Analyses_IDs %>%
      dplyr::select(dataset) %>%
      unique()

    AnalysisSets_ADaMs <- AnalysisSets %>%
      dplyr::filter(id %in% Analyses_IDs$analysisSetId) %>%
      dplyr::select(condition_dataset) %>%
      dplyr::rename(dataset = condition_dataset) %>%
      unique()

    DataSubsets_ADaMs <- DataSubsets %>%
      dplyr::filter(id %in% Analyses_IDs$dataSubsetId,
                    !is.na(condition_dataset)) %>%
      dplyr::select(condition_dataset) %>%
      dplyr::rename(dataset = condition_dataset) %>%
      unique()

    unique_datasets <- rbind(Analyses_ADaMs
                             , AnalysisSets_ADaMs
                             , DataSubsets_ADaMs
    ) %>%
      unique()

    # Loop through the unique dataset list once
    for (ad in unique_datasets) {
      ad_path <- paste0("adampathhere/", ad, ".csv")  # Construct the file path
      a1 <- paste0(a1, ad, " <- readr::read_csv(\'", ad_path, "\',
                                      show_col_types = FALSE,
                                      progress = FALSE) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ tidyr::replace_na(.x, '')))\n")  # Append each line
    }
    a1 <- gsub("adampathhere", adam_path, a1, fixed = TRUE)
    code_ADaM_1 <- unique(paste(a1, sep = ""))
    code_ADaM <- paste0("\n# Load ADaM -------\n",
                        paste(code_ADaM_1, collapse = "\n"))

    run_code <- ""    # variable to contain generated code
    combine_analysis_code <- "" # variable containing code to combine analyses

    # Programme header ----
    timenow <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    func_header <- function(OutputId, Output_Name, date){
      template <- "
# Programme:    Generate code to produce ARD for outputidhere
# Output:       outputnamehere
# Date created: datehere

  "
      code <- gsub('outputidhere', OutputId, template)
      code <- gsub('outputnamehere', Output_Name, code)
      code <- gsub('datehere', date, code)
      return(code)
    }

    code_header <- func_header(Output, OutputName, timenow)

    # loop through individual analyses
    max_j = nrow(Anas)
    for (j in 1:max_j) {
      # for (j in 1:nrow(Anas)) {

      #Analysis
      Anas_j <- Anas[j, ]$listItem_analysisId  # AnalysisID from
      #PA to dplyr::filter AN -jsonized
      Anas_s <- Analyses |> # row from AN to get other IDs
        dplyr::filter(id == Anas_j)

      ana_adam <- Anas_s$dataset # ADaM used for this analysis (esp. to be used in ChiSq)
      ana_name = Anas_s$name
      # Analysis Set
      ana_setId <- Anas_s$analysisSetId # AS ID (to be used in AS)
      ana_var <- Anas_s$variable #AS variable (to be used in MT)

      # Analysis Grouping
      groupid1 <- Anas_s$groupingId1    #group ID (to be used in AG)
      resultsByGroup1 <- Anas_s$resultsByGroup1 # Y/N group by
      groupid2 <- Anas_s$groupingId2
      resultsByGroup2 <- Anas_s$resultsByGroup2
      groupid3 <- Anas_s$groupingId3
      resultsByGroup3 <- Anas_s$resultsByGroup3

      # Data Subset
      subsetid <- Anas_s$dataSubsetId # data subset ID (to be used in DS)
      if(subsetid %in% c("", "NA")) {
        subsetid = NA
      } else{
        subsetid = subsetid
      }

      # Method
      methodid <- Anas_s$method_id #

      # Apply Analysis Set -----

      if(j == 1){ # only apply AnalysisSet once per output
        temp_AnSet <- AnalysisSets |>  # get analysis set for this iteration
          dplyr::filter(id == ana_setId)

        cond_adam <- temp_AnSet |> # ADaM for this analysis set
          dplyr::select(condition_dataset) |>
          as.character()

        cond_var <- temp_AnSet |> # condition variable for this analysis set
          dplyr::select(condition_variable) |>
          as.character()

        cond_oper <- temp_AnSet |> # condition operator for this analysis set
          dplyr::select(condition_comparator) |>
          as.character()

        cond_val <- temp_AnSet |> # condition value for this analysis set
          dplyr::select(condition_value) |>
          unlist()

        anSetName <- temp_AnSet |> # condition value for this analysis set
          dplyr::select(name)|>
          as.character()

        if(cond_oper == "EQ") { # convert to R code
          oper <-  '=='
        } else if(cond_oper == "NE"){
          oper = '!='
        } else if(cond_oper == "GE"){
          oper = '>='
        } else if(cond_oper == "GT"){
          oper = '>'
        } else if(cond_oper == "LE"){
          oper = '<='
        } else if(cond_oper == "LT"){
          oper = '<'
        }


        if(is.na(cond_val)){
          cond_val = ""
        } else{
          if(!is.numeric(cond_val)){
            cond_val = paste0(cond_val)
          }
        }

        # code for conditional statement for anSet (Fisher's)
        # anset_cond_stm = paste0(cond_var, oper,cond_val)

        # select 2nd Analysis in Output for identifying ADaM
        Anas_2 <- Anas[2, ]$listItem_analysisId
        Anas_s2 <- Analyses %>% # row from AN to get other IDs
          dplyr::filter(id == Anas_2)

        ana_adam2 <- Anas_s2$dataset

        if(cond_adam == ana_adam2){    # if Analysis Set ADaM and Output ADaM are same

          func_AnalysisSet1 <- function(dataset, variable, oper, val, ASID, anSetName) {
            template <- "
# Apply Analysis Set ---
df_pop <- dplyr::filter(ADaM,
            var operator 'value')

"
            code <- gsub('ADaM', dataset, template)
            code <- gsub('var', variable, code)
            code <- gsub('operator', oper, code)
            code <- gsub('value', val, code)
            code <- gsub('analysisidhere', ASID, code)
            code <- gsub('Analysissetnamehere', anSetName, code)

            return(code)
          }

          assign(paste0("code_AnalysisSet_",Anas_j), func_AnalysisSet1(cond_adam,
                                                                       cond_var,
                                                                       oper,
                                                                       cond_val,
                                                                       Anas_j,
                                                                       anSetName))

        }
        else { # if analysis set ADaM and Analysis ADaMs are different

          # variable used in Analysis
          func_AnalysisSet2 <- function(dataset,
                                        variable,
                                        oper,
                                        val,
                                        #anavar,
                                        ASID,
                                        anaADaM,
                                        anSetName) {
            template <- "
# Apply Analysis Set ---
overlap <- intersect(names(ADaM), names(analysisADAMhere))
overlapfin <- setdiff(overlap, 'USUBJID')

df_pop <- dplyr::filter(ADaM,
            var operator 'value') |>
            merge(analysisADAMhere |> dplyr::select(-dplyr::all_of(overlapfin)),
                  by = 'USUBJID',
                  all = FALSE)
"
            code <- gsub('ADaM', dataset, template)
            code <- gsub('var', variable, code)
            code <- gsub('operator', oper, code)
            code <- gsub('value', val, code)
            #code <- gsub('anasetvrhere', anavar, code)
            code <- gsub('analysisidhere', ASID, code)
            code <- gsub('analysisADAMhere', anaADaM, code)
            code <- gsub('Analysissetnamehere', anSetName, code)

            return(code)
          }

          assign(paste0("code_AnalysisSet_",Anas_j),
                 func_AnalysisSet2(cond_adam,
                                   cond_var,
                                   oper,
                                   cond_val,
                                   #ana_var,
                                   Anas_j,
                                   ana_adam2,
                                   anSetName))
        }
      } else{
        assign(paste0("code_AnalysisSet_",Anas_j),"")
      }

      # Apply Grouping ----------------------------

      # determine maximum groupings
      column_names <- colnames(Analyses)

      # Filter column names that match the pattern "Group"
      group_columns <- column_names[grep("^groupingId[0-9]+$", column_names)]

      # Extract the numeric part of the group columns and find the maximum
      group_numbers <- as.numeric(sub("groupingId", "", group_columns))
      max_group_number <- max(group_numbers)

      # consider denominator analysis
      NUM_analysisid = Anas_s$referencedAnalysisOperations_analysisId1
      DEN_analysisid = Anas_s$referencedAnalysisOperations_analysisId2

      AG_denom_id = Analyses %>%
        dplyr::filter(id == DEN_analysisid) %>%
        dplyr::select(groupingId1) %>%
        unique %>%
        as.character()

      AG_denom_temp1 <- AnalysisGroupings %>%
        dplyr::filter(id == AG_denom_id)

      AG_denom_var1 <- AG_denom_temp1 %>%
        dplyr::select(groupingVariable) %>%
        unique() %>%
        as.character()

      if(AG_denom_var1 %in% c(NA, "NA", "")){
        cli::cli_alert("Metadata issue in AnalysisGroupings {groupid1}: AnalysisGrouping has missing groupingVariable")
      }

      if(max_group_number >=1) {
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

        #get the number of dplyr::group_by to perform in Grouping Apply
        if(resultsByGroup1 == TRUE && !is.na(resultsByGroup1)){
          num_grp <- 1
          AG_max_dataDriven <- AG_1_DataDriven
        } else num_grp = 0

        if(max_group_number >= 2){
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

          #get the number of dplyr::group_by to perform in Grouping Apply
          if(resultsByGroup1 == TRUE && !is.na(resultsByGroup1)){
            num_grp <- 1
            AG_max_dataDriven <- AG_1_DataDriven
            if(resultsByGroup2 == TRUE && !is.na(resultsByGroup2)) {
              num_grp <- 2
              AG_max_dataDriven <- AG_2_DataDriven
            }
          } else num_grp = 0

          if(max_group_number >= 3){

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

            #get the number of dplyr::group_by to perform in Grouping Apply
            if(resultsByGroup1 == TRUE && !is.na(resultsByGroup1)){
              num_grp <- 1
              AG_max_dataDriven <- AG_1_DataDriven
              if(resultsByGroup2 == TRUE && !is.na(resultsByGroup2)) {
                num_grp <- 2
                AG_max_dataDriven <- AG_2_DataDriven
                if(resultsByGroup3 == TRUE && !is.na(resultsByGroup3)) {
                  num_grp <- 3
                  AG_max_dataDriven <- AG_3_DataDriven
                }
              }
            } else num_grp = 0

            # get number of group_by for non-grouped operations
            if(!is.na(resultsByGroup1)){
              num_grp_any <- 1
              if(!is.na(resultsByGroup2)) {
                num_grp_any <- 2
                if(!is.na(resultsByGroup3)) {
                  num_grp_any <- 3
                }
              }
            } else num_grp_any = 0

          }
        }
      }

      if(example == FALSE){

        if(num_grp == 1){
          #cards part
          distinct_list <- paste0(AG_var1,", ",ana_var)
          by_listc <- paste0("'",AG_var1,"'")
          by_list <- paste0(AG_var1)

          by_vars =  paste0(", variables = '",AG_var1,"'")
          strata_vars =  paste0(", variables = '",AG_var1,"'")

        } else if(num_grp == 2){
          #cards part
          distinct_list <- paste0(AG_var1,", ",AG_var2,", ",ana_var)
          by_listc <- paste0("'",AG_var1,"', '",AG_var2,"'")
          by_list <- paste0(AG_var1,", ",AG_var2)

          by_vars =  paste0(", by = '",
                            AG_var1,
                            "' , variables = '"
                            ,AG_var2,"'")

          strata_vars =  paste0(", strata = '",
                                AG_var1,
                                "' , variables = '"
                                ,AG_var2,"'")
        } else if(num_grp == 3){

          #cards part
          distinct_list <- paste0(AG_var1,", ",AG_var2,", ",AG_var3,", ",ana_var)
          by_listc <- paste0("'",AG_var1,"', '",AG_var2,"', '",AG_var3,"'")
          by_list <- paste0(AG_var1,", ",AG_var2,", ",AG_var3)

          by_vars =  paste0(", by = c('",
                            AG_var1,
                            "', '",
                            AG_var2,
                            "') , variables = '"
                            ,AG_var3,"'")

          strata_vars =  paste0(", strata = c('",
                                AG_var1,
                                "', '",
                                AG_var2,
                                "') , variables = '"
                                ,AG_var3,"'")

        } else { # no grouping being done
        }

      } else if(example == TRUE){

        if(num_grp == 1){
          func_AnalysisGrouping1 <- function(var1, ASID) {

            template <- "

#Apply Analysis Grouping ---
df1_analysisidhere <- df_analysisidhere |>
          dplyr::group_by(var)

"
            code <- gsub('var', var1, template)
            code <- gsub('analysisidhere', ASID, code)
          }

          code_AnalysisGrouping_0 <- func_AnalysisGrouping1(AG_var1, Anas_j)

        } else if(num_grp == 2){
          func_AnalysisGrouping2 <- function(var1, var2, ASID) {

            template <- "

#Apply Analysis Grouping ---
df1_analysisidhere <- df_analysisidhere |>
          dplyr::group_by(var1, var2)

"

            code <- gsub('var1', var1, template)
            code <- gsub('var2', var2, code)
            code <- gsub('analysisidhere', ASID, code)

            return(code)
          }

          code_AnalysisGrouping_0 <- func_AnalysisGrouping2(AG_var1,
                                                            AG_var2,
                                                            Anas_j)
        } else if(num_grp == 3){
          func_AnalysisGrouping3 <- function(var1, var2, var3, ASID) {

            template <- "

#Apply Analysis Grouping ---
df1_analysisidhere <- df_analysisidhere |>
          dplyr::group_by(var1, var2, var3)

"
            code <- gsub('var1', var1, template)
            code <- gsub('var2', var2, code)
            code <- gsub('var3', var3, code)
            code <- gsub('analysisidhere', ASID, code)
            return(code)
          }

          code_AnalysisGrouping_0 <- func_AnalysisGrouping3(AG_var1,
                                                            AG_var2,
                                                            AG_var3,
                                                            Anas_j)
        } else {

          func_AnalysisGrouping4 <- function(ASID) {

            template <- "

#Apply Analysis Grouping ---

# (No grouping applicable for this analysis)
df1_analysisidhere <- df_analysisidhere
"

            code <- gsub('analysisidhere', ASID, template)
            return(code)
          }

          code_AnalysisGrouping_0 <- func_AnalysisGrouping4(Anas_j)
        }

        assign(paste0("code_AnalysisGrouping_",
                      Anas_j),
               code_AnalysisGrouping_0)
      }
      # Apply DataSubset -------------------------------------------------------------

      if(exists("DataSubsets")){ # if there is a data subset for the RE
        if(!is.na(subsetid)){ # if there is a data subset for this analysis
          subsetrule <- DataSubsets %>%
            dplyr::filter(id == subsetid)

          DSname <- subsetrule %>%
            dplyr::select(name) %>%
            unique() %>%
            as.character()

          # for Fisher's exact test to filter:
          # fishersrow = subsetrule %>%
          #   dplyr::filter(condition_dataset == AG_ds1)
          #
          # # assign the variables
          # fishervar = fishersrow$condition_variable
          #
          # fishervac = fishersrow$condition_comparator
          #
          # fisherval1 = fishersrow$condition_value
          #
          #
          # if(nrow(fishersrow) > 0) { # if we need a DS statement
          #
          #
          #   if(fishervac == "IN") {
          #     fisher_f_vac = "%in%"
          #
          #     fisher_f_val = paste0("'", trimws(unlist(strsplit(fisherval1, "\\|"))), "'", collapse = ",")
          #   }# define operator in R code
          #   else { # vac is EQ or NE
          #     if(fishervac == "EQ") fisher_f_vac = "==" # define operator in R code
          #     else fisher_f_vac = "!=" #
          #     fisher_f_val = paste0("'", fisherval1,"'")
          #   }
          #   # concatenate expression
          #   fisher_cond_stm = paste0(", ",
          #                            fishervar,
          #                            " ",
          #                            fisher_f_vac," "
          #                            , "c(",
          #                            fisher_f_val,")")
          # } else { # if the DS statement should be blank for fisher's
          #   fisher_cond_stm = ""
          #
          # }

          ### end Fisher's exact test to filter


          if(nrow(subsetrule) == 1){      # if there's only one row

            var = subsetrule$condition_variable
            val1 = stringr::str_trim(subsetrule$condition_value)
            vac = subsetrule$condition_comparator

            if(vac == "IN"){
              rvac = "%in%"

              # multiple values
              vals = strsplit(val1, ",\\s*")[[1]]
              is_num <- !is.na(suppressWarnings(as.numeric(vals)))

              if(is_num[1] == TRUE){ # numeric values
                vals_ = suppressWarnings(as.numeric(vals))
                val =  paste0("c(", paste0( vals_, collapse = ", "), ")")
              } else{
                val =  paste0("c(", paste0("'", vals, "'", collapse = ", "), ")")
              }

            }else{
              if(vac == "EQ") rvac = '=='
              if(vac == "NE") rvac = '!='
              if(vac == "GT") rvac = '>'
              if(vac == "GE") rvac = '>='
              if(vac == "LT") rvac = '<'
              if(vac == "LE") rvac = '<='

              is_num <- !is.na(suppressWarnings(as.numeric(val1)))
              if(is_num == TRUE){
                val = suppressWarnings(as.numeric(val1))
              } else{
                val =  paste0("'",val1,"'")
              }
            }

            rFilt_final <- paste0(var," ", rvac," ",val)

          } else  {# if there are more than one rows

            maxlev = max(subsetrule$level)
            if(maxlev <= 1){
              cli::cli_abort("Metadata issue in DataSubsets {subsetid}: DataSubset levels not incrementing")
            }

            for (m in 1:(maxlev - 1)){   #loop through levels
              # get logical operators

              log_oper = subsetrule %>%  # identify all rows for this level
                dplyr::filter(level == m,
                              !is.na(compoundExpression_logicalOperator)) %>%
                dplyr::select(compoundExpression_logicalOperator) %>%
                as.character()

              if(log_oper == "character(0)" ) log_oper = NA
              assign(paste('log_oper',m, sep=''), log_oper) # assign logical operator value
              #R code
              if(!is.na(log_oper)){
                if(log_oper == "AND") rlog_oper = '&'
                else if(log_oper == "OR") rlog_oper = '|'
                else rlog_oper = NA
              }

              lev = subsetrule %>%  # subset containing only first set of equations
                dplyr::filter(level == m+1,
                              is.na(compoundExpression_logicalOperator))

              rcode <- ""

              if(file_ext == "json"){

                for (n in 1:nrow(lev)) {

                  ord1_ <- lev[n, ] # one row at a time

                  # assign the variables
                  var = ord1_$condition_variable

                  vac = ord1_$condition_comparator

                  if(example == TRUE){
                    val1 = ord1_$condition_value %>%
                      unlist()
                    is_num <- !is.na(suppressWarnings(as.numeric(val1)))

                    if(vac == "IN") {
                      f_vac = "%in%"

                      # multiple values
                      if(is_num[1] == TRUE){ # numeric values
                        val1_ = suppressWarnings(as.numeric(val1))
                        val =  paste0("c(", paste0( val1_, collapse = ", "), ")")
                      } else{
                        val =  paste0("c(", paste0("'", val1, "'", collapse = ", "), ")")
                      }
                    }
                    else {
                      if(vac == "EQ") f_vac = '=='
                      if(vac == "NE") f_vac = '!='
                      if(vac == "GT") f_vac = '>'
                      if(vac == "GE") f_vac = '>='
                      if(vac == "LT") f_vac = '<'
                      if(vac == "LE") f_vac = '<='

                      # single value
                      if(is_num == TRUE){
                        val = suppressWarnings(as.numeric(val1))
                      } else{
                        val =  paste0("'",val1,"'")
                      }
                    }
                  } else{ # example not true
                    val1 = ord1_$condition_value

                    if(vac == "IN") {
                      f_vac = "%in%"

                      # multiple values
                      vals = strsplit(val1, ",\\s*")[[1]]
                      is_num <- !is.na(suppressWarnings(as.numeric(vals)))

                      if(is_num[1] == TRUE){ # numeric values
                        vals_ = suppressWarnings(as.numeric(vals))
                        val =  paste0("c(", paste0( vals_, collapse = ", "), ")")
                      } else{
                        val =  paste0("c(", paste0("'", vals, "'", collapse = ", "), ")")
                      }
                    }
                    else {
                      if(vac == "EQ") f_vac = '=='
                      if(vac == "NE") f_vac = '!='
                      if(vac == "GT") f_vac = '>'
                      if(vac == "GE") f_vac = '>='
                      if(vac == "LT") f_vac = '<'
                      if(vac == "LE") f_vac = '<='

                      # single value
                      is_num <- !is.na(suppressWarnings(as.numeric(val1)))
                      if(is_num == TRUE){
                        val = suppressWarnings(as.numeric(val1))
                      } else{
                        val =  paste0("'",val1,"'")
                      }
                    }
                  }



                  # concatenate expression
                  assign(paste("fexp", m,n, sep = "_"), paste0(var," ", f_vac," ", val))

                  if(n>1) assign('rcode', paste0(rcode, " LOGOP ",var," ", f_vac," ", val))
                  else assign('rcode', paste0(var," ", f_vac," ", val))

                } # end loop through rows
                # combine total dplyr::filter
              } else if(file_ext == "xlsx"){

                for (n in 1:nrow(lev)) {

                  ord1_ <- lev[n, ] # one row at a time

                  # assign the variables
                  var = ord1_$condition_variable

                  vac = ord1_$condition_comparator


                  val1 = ord1_$condition_value
                  val = gsub("\\|", ",", val1)

                  if(vac == "IN") {
                    f_vac = "%in%"

                    #multiple values
                    vals = strsplit(val1, ",\\s*")[[1]]
                    is_num <- !is.na(suppressWarnings(as.numeric(vals)))

                    if(is_num[1] == TRUE){ # numeric values
                      vals_ = suppressWarnings(as.numeric(vals))
                      f_val =  paste0("c(", paste0( vals_, collapse = ", "), ")")
                    } else{
                      f_val =  paste0("c(", paste0("'", vals, "'", collapse = ", "), ")")
                    }

                  } else { # vac is EQ or NE
                    if(vac == "EQ") f_vac = '=='
                    if(vac == "NE") f_vac = '!='
                    if(vac == "GT") f_vac = '>'
                    if(vac == "GE") f_vac = '>='
                    if(vac == "LT") f_vac = '<'
                    if(vac == "LE") f_vac = '<='

                    # single value
                    is_num <- !is.na(suppressWarnings(as.numeric(val1)))
                    if(is_num == TRUE){
                      f_val = suppressWarnings(as.numeric(val1))
                    } else{
                      f_val =  paste0("'",val1,"'")
                    }
                  }
                  # concatenate expression
                  assign(paste("fexp", m,n, sep = "_"), paste0(var," ", f_vac," ", f_val))

                  if(n>1) assign('rcode', paste0(rcode, " LOGOP ",var," ", f_vac," ", f_val))
                  else assign('rcode', paste0(var," ", f_vac," ", f_val))

                }# end loop through rows
              }
              # combine total dplyr::filter

              assign(paste("rFilt", m, sep = "_"),
                     gsub("LOGOP", rlog_oper, rcode))
            } # end loop through levels

            # combine all dplyr::filter values:
            if(exists('rFilt_2')){

              rFilt_final <- paste(rFilt_1, rFilt_2, sep = ", ")
              rm(rFilt_2) #clear it so it doesn't exist for future
            } else rFilt_final <- rFilt_1

          } # end case where there are more than one rows



          func_DataSubset1 <- function(filterVal, ASID, DSNAME) {
            if(example == FALSE){
              template <- "

# Apply Data Subset ---
# Data subset: dsnamehere
df2_analysisidhere <- df_pop |>
        dplyr::filter(dplyr::filtertext1)

"
            } else{
              template <- "

# Apply Data Subset ---
# Data subset: dsnamehere
df2_analysisidhere <- df_pop |>
        dplyr::filter(dplyr::filtertext1)

"
            }

            code <- gsub('dplyr::filtertext1', filterVal, template)
            code <- gsub('analysisidhere', ASID, code)
            code <- gsub('dsnamehere', DSNAME, code)

            return(code)
          }

          # code_DataSubset <- func_DataSubset(rFilt_final, Anas_j)
          assign(paste0("code_DataSubset_",Anas_j),
                 func_DataSubset1(rFilt_final,
                                  Anas_j,
                                  DSname)
          )

        } else { # there is no data subsetting for this analysis

          func_DataSubset2 <- function(ASID) {
            if(example == FALSE){
              template <- "

#Apply Data Subset ---
df2_analysisidhere <- df_pop

"
            } else{
              template <- "

#Apply Data Subset ---
df2_analysisidhere <- df_pop

"
            }

            code <- gsub('analysisidhere', ASID, template)
            return(code)
          } # end function

          assign(paste0("code_DataSubset_",Anas_j),
                 func_DataSubset2(Anas_j)
          )
        } # end case where no data subsetting
      } # end case where no data subsetting for the entire RE

      else { # no data subset for the RE


        func_DataSubset3 <- function(ASID) {

          if(example == FALSE){
            template <- "

#Apply Data Subset ---
df2_analysisidhere <- df_pop

"} else {
  template <- "

#Apply Data Subset ---
df2_analysisidhere <- df_pop

"
}

          code <- gsub('analysisidhere', ASID, template)
          return(code)
        } # end function

        assign(paste0("code_DataSubset_",Anas_j),
               func_DataSubset3(Anas_j)
        )
      }

      # Apply AnalysisMethod -------------------------------------------------------------
      if(example == FALSE){

        method <- AnalysisMethods %>%
          dplyr::filter(id == methodid) %>% # refnew
          dplyr::select(name, description, label, id) %>%
          unique()

        methodname = method$name
        methoddesc = method$description
        methodlabel = method$label
        methodid = method$id

        # Code
        anmetcode <- AnalysisMethodCodeTemplate %>%
          dplyr::filter(
            method_id == methodid,
            context %in% c("R", "R (siera)", "siera"),
            specifiedAs == "Code"
          ) %>%
          dplyr::pull(templateCode)
        anmetcode <- anmetcode[1]

        if (length(anmetcode) == 0 || is.na(anmetcode) || anmetcode == "") {
          warning(
            sprintf(
              "No template code found in AnalysisMethodCodeTemplate for method '%s' referenced by analysis '%s'",
              methodid, Anas_j
            )
          )
          anmetcode <- ""
        }

        # Parameters
        # to be replaced with Source values:
        anmetparam_s <- AnalysisMethodCodeParameters %>%
          dplyr::filter(method_id == methodid,
                        parameter_valueSource != "")

        # to be replaced with values:
        # anmetparam_v <- AnalysisMethodCodeParameters %>%
        #   dplyr::filter(method_id == methodid,
        #                 parameter_value != "")

        # transpose_yn = anmetparam_v %>%
        #   dplyr::filter(parameter_name == "transpose") %>%
        #   dplyr::select(parameter_value) %>%
        #   as.character()

        # operations to transpose with
        operation_list <- AnalysisMethods %>%
          dplyr::filter(id == methodid) %>% # refnew
          dplyr::select(operation_id)

        operation_list_string = paste(operation_list$operation_id,
                                      collapse = ", ")


        #refnew
        # intro part

        template <- "
# Method ID:              methodidhere
# Method name:            methodnamehere
# Method description:     methoddeschere
"

        code <- gsub('methodidhere', methodid, template)
        code <- gsub('methodnamehere', methodname, code)
        code_method_tmp_1 <- gsub('methoddeschere', methoddesc, code)

        # code part

        ## using for loop
        anmetcode_temp <- paste0("if(nrow(df2_analysisidhere) != 0) {
                              ",
                                 anmetcode,
                                 "}"
        )

        for (i in seq_len(nrow(anmetparam_s))) {
          # Get the replacement value using get() based on the variable name in Column B
          rep <- get(anmetparam_s$parameter_valueSource[i])
          # Replace the placeholder in VAR with the variable's value
          if(!is.na(rep)){
            anmetcode_temp <- gsub(anmetparam_s$parameter_name[i],
                                   rep,
                                   anmetcode_temp)
          }
        }
        anmetcode_final <- gsub('methodidhere', methodid, anmetcode_temp)
        anmetcode_final <- gsub('analysisidhere', Anas_j, anmetcode_final)

        # applying transpose code
        # if(transpose_yn == "Y"){
        #   code_method_tmp_2 = paste0(trimws(anmetcode_final %>%
        #                                       as.character()), " %>%
        #   pivot_longer(c(", operation_list_string, "),
        #   names_to = 'operation_id',
        #   values_to = 'res')")
        # } else {
        code_method_tmp_2 = anmetcode_final
        # }

        # mutate part

        template <-
          "
if(nrow(df2_analysisidhere) != 0){
df3_analysisidhere <- df3_analysisidhere |>
        dplyr::mutate(AnalysisId = 'analysisidhere',
               MethodId = 'methodidhere',
               OutputId = 'outputidhere')
} else {
    df3_analysisidhere = data.frame(AnalysisId = 'analysisidhere',
               MethodId = 'methodidhere',
               OutputId = 'outputidhere')
}
    "

      code <- gsub('methodidhere', methodid, template)
      code <- gsub('analysisidhere', Anas_j, code)
      code_method_tmp_3 <- gsub('outputidhere', Output, code)


      code_method = paste0(code_method_tmp_1, "\n",
                           code_method_tmp_2,
                           code_method_tmp_3)


      # code to combine it all --------------------------------------------------
      # dplyr::rename groups to append
      if(num_grp == 1){ # if 1 analysis grouping
        func_rename1 <- function(groupvar1) {
          template <- " %>%
        dplyr::rename(Group1 = groupvar1here)
"
          code <- gsub('groupvar1here', groupvar1, template)

          return(code)
        }

        code_rename = func_rename1(AG_var1)

      }
      else if(num_grp == 2){ # if 2 analysis groupings
        func_rename2 <- function(groupvar1,
                                 groupvar2) {
          template <- " |>
        dplyr::rename(Group1 = groupvar1here,
               Group2 = groupvar2here)
"
          code <- gsub('groupvar1here', groupvar1, template)
          code <- gsub('groupvar2here', groupvar2, code)

          return(code)
        }
        code_rename = func_rename2(AG_var1,
                                   AG_var2)

      }
      else if(num_grp == 3){ # if 3 analysis groupings
        func_rename3 <- function(groupvar1,
                                 groupvar2,
                                 groupvar3) {
          template <- " |>
        dplyr::rename(Group1 = groupvar1here,
               Group2 = groupvar2here,
               Group3 = groupvar3here)
"
          code <- gsub('groupvar1here', groupvar1, template)
          code <- gsub('groupvar2here', groupvar2, code)
          code <- gsub('groupvar3here', groupvar3, code)

          return(code)
        }

        code_rename = func_rename3(AG_var1,
                                   AG_var2,
                                   AG_var3)
      } else code_rename = "" # if no analysis grouping


      assign(paste0("code_AnalysisMethod_", Anas_j),
             paste0("#Apply Method --- \n",
                    code_method#,
                    #code_rename
             ))
      } else {
        method <- AnalysisMethods %>%
          dplyr::filter(id == methodid)

        code_Operation_0 = ""  # initialise code (to be appended)
        code_combine = "" #initialise code to combine datasets

        for(k in 1:nrow(method)){
          # for(k in 1:1){

          operation = method[k,] # one operation at a time
          oper_id <- operation$operation_id #current operation ID
          # oper_order <- operation$operation_order #current operation order
          oper_name <- operation$name #current operation name
          oper_desc <- operation$description #current operation description
          oper_pattern <- operation$operation_resultPattern # pattern


          # Mth01_CatVar_Count_ByGrp_1_n ------------------------

          if(oper_id == "Mth01_CatVar_Count_ByGrp_1_n"){

            func_OperationTmp1 <- function(operid,
                                           # operorder,
                                           opername,
                                           operdesc,
                                           analysisid,
                                           methodid,
                                           outputid,
                                           pattern) {
              template <- "
# Operation ID:           operationidhere
# Operation name:         operationnamehere
# Operation description:  operationdeschere

df3_analysisidhere_operationidhere <- df2_analysisidhere |>
        dplyr::summarise(res = n()) |>
        dplyr::mutate(AnalysisId = 'analysisidhere',
               MethodId = 'methodidhere',
               OperationId = 'operationidhere',
               OutputId = 'outputidhere',
               pattern = 'patternhere')

"
              code <- gsub('operationidhere', operid, template)
              code <- gsub('operationnamehere', opername, code)
              code <- gsub('operationdeschere', operdesc, code)
              code <- gsub('analysisidhere', analysisid, code)
              code <- gsub('methodidhere', methodid, code)
              code <- gsub('outputidhere', outputid, code)
              code <- gsub('patternhere', pattern, code)

              return(code)
            }

            code_Operation_tmp = func_OperationTmp1(oper_id,
                                                    oper_name,
                                                    oper_desc,
                                                    Anas_j,
                                                    methodid,
                                                    Output,
                                                    oper_pattern)

            #cat(code_Operation_tmp)
            # Mth02_ContVar_Summ_ByGrp_1_n ------------------
          } else if(operation$operation_id == "Mth02_ContVar_Summ_ByGrp_1_n"){

            func_OperationTmp2 <- function(operid,
                                           # operorder,
                                           opername,
                                           operdesc,
                                           analysisid,
                                           methodid,
                                           outputid,
                                           pattern) {
              template <- "
# Operation ID:           operationidhere
# Operation name:         operationnamehere
# Operation description:  operationdeschere

df3_analysisidhere_operationidhere <- df2_analysisidhere |>
        dplyr::summarise(res = n()) |>
        dplyr::mutate(AnalysisId = 'analysisidhere',
               MethodId = 'methodidhere',
               OperationId = 'operationidhere',
               OutputId = 'outputidhere',
               pattern = 'patternhere')

"
              code <- gsub('operationidhere', operid, template)
              code <- gsub('operationnamehere', opername, code)
              code <- gsub('operationdeschere', operdesc, code)
              code <- gsub('analysisidhere', analysisid, code)
              code <- gsub('methodidhere', methodid, code)
              code <- gsub('outputidhere', outputid, code)
              code <- gsub('patternhere', pattern, code)

              return(code)
            }

            code_Operation_tmp = func_OperationTmp2(oper_id,
                                                    # oper_order,
                                                    oper_name,
                                                    oper_desc,
                                                    Anas_j,
                                                    methodid,
                                                    Output,
                                                    oper_pattern)

            #cat(code_Operation_tmp)
            # Mth02_ContVar_Summ_ByGrp_2_Mean ------------------
          } else if(operation$operation_id == "Mth02_ContVar_Summ_ByGrp_2_Mean"){

            func_OperationTmp3 <- function(operid,
                                           # operorder,
                                           opername,
                                           operdesc,
                                           analysisid,
                                           methodid,
                                           outputid,
                                           analysisvar,
                                           pattern) {
              template <- "
# Operation ID:           operationidhere
# Operation name:         operationnamehere
# Operation description:  operationdeschere

df3_analysisidhere_operationidhere <- df2_analysisidhere |>
        dplyr::summarise(res = mean(ana_varhere)) |>
        dplyr::mutate(AnalysisId = 'analysisidhere',
               MethodId = 'methodidhere',
               OperationId = 'operationidhere',
               OutputId = 'outputidhere',
               pattern = 'patternhere')

"
              code <- gsub('operationidhere', operid, template)
              code <- gsub('operationnamehere', opername, code)
              code <- gsub('operationdeschere', operdesc, code)
              # code <- gsub('operorderhere', operorder, code)
              code <- gsub('analysisidhere', analysisid, code)
              code <- gsub('methodidhere', methodid, code)
              code <- gsub('outputidhere', outputid, code)
              code <- gsub('ana_varhere', analysisvar, code)
              code <- gsub('patternhere', pattern, code)


              return(code)
            }

            code_Operation_tmp = func_OperationTmp3(oper_id,
                                                    # oper_order,
                                                    oper_name,
                                                    oper_desc,
                                                    Anas_j,
                                                    methodid,
                                                    Output,
                                                    ana_var,
                                                    oper_pattern)

            #cat(code_Operation_tmp)
            # Mth02_ContVar_Summ_ByGrp_3_SD ------------------
          } else if(operation$operation_id == "Mth02_ContVar_Summ_ByGrp_3_SD"){

            func_OperationTmp4 <- function(operid,
                                           # operorder,
                                           opername,
                                           operdesc,
                                           analysisid,
                                           methodid,
                                           outputid,
                                           analysisvar,
                                           pattern) {
              template <- "
# Operation ID:           operationidhere
# Operation name:         operationnamehere
# Operation description:  operationdeschere

df3_analysisidhere_operationidhere <- df2_analysisidhere |>
        dplyr::summarise(res = sd(ana_varhere)) |>
        dplyr::mutate(AnalysisId = 'analysisidhere',
               MethodId = 'methodidhere',
               OperationId = 'operationidhere',
               OutputId = 'outputidhere',
               pattern = 'patternhere')

"
              code <- gsub('operationidhere', operid, template)
              code <- gsub('operationnamehere', opername, code)
              code <- gsub('operationdeschere', operdesc, code)
              # code <- gsub('operorderhere', operorder, code)
              code <- gsub('analysisidhere', analysisid, code)
              code <- gsub('methodidhere', methodid, code)
              code <- gsub('outputidhere', outputid, code)
              code <- gsub('ana_varhere', analysisvar, code)
              code <- gsub('patternhere', pattern, code)

              return(code)
            }

            code_Operation_tmp = func_OperationTmp4(oper_id,
                                                    # oper_order,
                                                    oper_name,
                                                    oper_desc,
                                                    Anas_j,
                                                    methodid,
                                                    Output,
                                                    ana_var,
                                                    oper_pattern)

            #cat(code_Operation_tmp)
            # Mth02_ContVar_Summ_ByGrp_4_Median ------------------
          } else if(operation$operation_id == "Mth02_ContVar_Summ_ByGrp_4_Median"){

            func_OperationTmp5 <- function(operid,
                                           # operorder,
                                           opername,
                                           operdesc,
                                           analysisid,
                                           methodid,
                                           outputid,
                                           analysisvar,
                                           pattern) {
              template <- "
# Operation ID:           operationidhere
# Operation name:         operationnamehere
# Operation description:  operationdeschere

df3_analysisidhere_operationidhere <- df2_analysisidhere |>
        dplyr::summarise(res = median(ana_varhere)) |>
        dplyr::mutate(AnalysisId = 'analysisidhere',
               MethodId = 'methodidhere',
               OperationId = 'operationidhere',
               OutputId = 'outputidhere',
               pattern = 'patternhere')

"
              code <- gsub('operationidhere', operid, template)
              code <- gsub('operationnamehere', opername, code)
              code <- gsub('operationdeschere', operdesc, code)
              # code <- gsub('operorderhere', operorder, code)
              code <- gsub('analysisidhere', analysisid, code)
              code <- gsub('methodidhere', methodid, code)
              code <- gsub('outputidhere', outputid, code)
              code <- gsub('ana_varhere', analysisvar, code)
              code <- gsub('patternhere', pattern, code)

              return(code)
            }

            code_Operation_tmp = func_OperationTmp5(oper_id,
                                                    # oper_order,
                                                    oper_name,
                                                    oper_desc,
                                                    Anas_j,
                                                    methodid,
                                                    Output,
                                                    ana_var,
                                                    oper_pattern)
            #cat(code_Operation_tmp)
            # Mth02_ContVar_Summ_ByGrp_5_Q1 ------------------
          } else if(operation$operation_id == "Mth02_ContVar_Summ_ByGrp_5_Q1"){
            func_OperationTmp6 <- function(operid,
                                           # operorder,
                                           opername,
                                           operdesc,
                                           analysisid,
                                           methodid,
                                           outputid,
                                           analysisvar,
                                           pattern) {
              template <- "
# Operation ID:           operationidhere
# Operation name:         operationnamehere
# Operation description:  operationdeschere

df3_analysisidhere_operationidhere <- df2_analysisidhere |>
        dplyr::summarise(res = quantile(ana_varhere, c(.25), na.rm = TRUE)) |>
        dplyr::mutate(AnalysisId = 'analysisidhere',
               MethodId = 'methodidhere',
               OperationId = 'operationidhere',
               OutputId = 'outputidhere',
               pattern = 'patternhere')

"
              code <- gsub('operationidhere', operid, template)
              code <- gsub('operationnamehere', opername, code)
              code <- gsub('operationdeschere', operdesc, code)
              # code <- gsub('operorderhere', operorder, code)
              code <- gsub('analysisidhere', analysisid, code)
              code <- gsub('methodidhere', methodid, code)
              code <- gsub('outputidhere', outputid, code)
              code <- gsub('ana_varhere', analysisvar, code)
              code <- gsub('patternhere', pattern, code)

              return(code)
            }

            code_Operation_tmp = func_OperationTmp6(oper_id,
                                                    # oper_order,
                                                    oper_name,
                                                    oper_desc,
                                                    Anas_j,
                                                    methodid,
                                                    Output,
                                                    ana_var,
                                                    oper_pattern)
            #cat(code_Operation_tmp)
            # Mth02_ContVar_Summ_ByGrp_6_Q3 ------------------
          } else if(operation$operation_id == "Mth02_ContVar_Summ_ByGrp_6_Q3"){
            func_OperationTmp7 <- function(operid,
                                           # operorder,
                                           opername,
                                           operdesc,
                                           analysisid,
                                           methodid,
                                           outputid,
                                           analysisvar,
                                           pattern) {
              template <- "
# Operation ID:           operationidhere
# Operation name:         operationnamehere
# Operation description:  operationdeschere

df3_analysisidhere_operationidhere <- df2_analysisidhere |>
        dplyr::summarise(res = quantile(ana_varhere, c(.75), na.rm = TRUE)) |>
        dplyr::mutate(AnalysisId = 'analysisidhere',
               MethodId = 'methodidhere',
               OperationId = 'operationidhere',
               OutputId = 'outputidhere',
               pattern = 'patternhere')

"
              code <- gsub('operationidhere', operid, template)
              code <- gsub('operationnamehere', opername, code)
              code <- gsub('operationdeschere', operdesc, code)
              # code <- gsub('operorderhere', operorder, code)
              code <- gsub('analysisidhere', analysisid, code)
              code <- gsub('methodidhere', methodid, code)
              code <- gsub('outputidhere', outputid, code)
              code <- gsub('ana_varhere', analysisvar, code)
              code <- gsub('patternhere', pattern, code)

              return(code)
            }

            code_Operation_tmp = func_OperationTmp7(oper_id,
                                                    # oper_order,
                                                    oper_name,
                                                    oper_desc,
                                                    Anas_j,
                                                    methodid,
                                                    Output,
                                                    ana_var,
                                                    oper_pattern)
            #cat(code_Operation_tmp)
            # Mth02_ContVar_Summ_ByGrp_7_Min ------------------
          } else if(operation$operation_id == "Mth02_ContVar_Summ_ByGrp_7_Min"){
            func_OperationTmp8 <- function(operid,
                                           # operorder,
                                           opername,
                                           operdesc,
                                           analysisid,
                                           methodid,
                                           outputid,
                                           analysisvar,
                                           pattern) {
              template <- "
# Operation ID:           operationidhere
# Operation name:         operationnamehere
# Operation description:  operationdeschere

df3_analysisidhere_operationidhere <- df2_analysisidhere |>
        dplyr::summarise(res = min(ana_varhere)) |>
        dplyr::mutate(AnalysisId = 'analysisidhere',
               MethodId = 'methodidhere',
               OperationId = 'operationidhere',
               OutputId = 'outputidhere',
               pattern = 'patternhere')

"
              code <- gsub('operationidhere', operid, template)
              code <- gsub('operationnamehere', opername, code)
              code <- gsub('operationdeschere', operdesc, code)
              # code <- gsub('operorderhere', operorder, code)
              code <- gsub('analysisidhere', analysisid, code)
              code <- gsub('methodidhere', methodid, code)
              code <- gsub('outputidhere', outputid, code)
              code <- gsub('ana_varhere', analysisvar, code)
              code <- gsub('patternhere', pattern, code)


              return(code)
            }

            code_Operation_tmp = func_OperationTmp8(oper_id,
                                                    # oper_order,
                                                    oper_name,
                                                    oper_desc,
                                                    Anas_j,
                                                    methodid,
                                                    Output,
                                                    ana_var,
                                                    oper_pattern)
            # #cat(code_Operation_tmp)
            # Mth02_ContVar_Summ_ByGrp_8_Max ------------------

          } else if(operation$operation_id == "Mth02_ContVar_Summ_ByGrp_8_Max"){
            func_OperationTmp9 <- function(operid,
                                           # operorder,
                                           opername,
                                           operdesc,
                                           analysisid,
                                           methodid,
                                           outputid,
                                           analysisvar,
                                           pattern) {
              template <- "
# Operation ID:           operationidhere
# Operation name:         operationnamehere
# Operation description:  operationdeschere

df3_analysisidhere_operationidhere <- df2_analysisidhere |>
        dplyr::summarise(res = max(ana_varhere)) |>
        dplyr::mutate(AnalysisId = 'analysisidhere',
               MethodId = 'methodidhere',
               OperationId = 'operationidhere',
               OutputId = 'outputidhere',
               pattern = 'patternhere')

"
              code <- gsub('operationidhere', operid, template)
              code <- gsub('operationnamehere', opername, code)
              code <- gsub('operationdeschere', operdesc, code)
              # code <- gsub('operorderhere', operorder, code)
              code <- gsub('analysisidhere', analysisid, code)
              code <- gsub('methodidhere', methodid, code)
              code <- gsub('outputidhere', outputid, code)
              code <- gsub('ana_varhere', analysisvar, code)
              code <- gsub('patternhere', pattern, code)


              return(code)
            }

            code_Operation_tmp = func_OperationTmp9(oper_id,
                                                    # oper_order,
                                                    oper_name,
                                                    oper_desc,
                                                    Anas_j,
                                                    methodid,
                                                    Output,
                                                    ana_var,
                                                    oper_pattern)
            # #cat(code_Operation_tmp)
            # Mth01_CatVar_Summ_ByGrp_1_n ------------------

          } else if(operation$operation_id == "Mth01_CatVar_Summ_ByGrp_1_n"){
            func_OperationTmp10 <- function(operid,
                                            # operorder,
                                            opername,
                                            operdesc,
                                            analysisid,
                                            methodid,
                                            outputid,
                                            analysisvar,
                                            pattern) {
              template <- "
# Operation ID:           operationidhere
# Operation name:         operationnamehere
# Operation description:  operationdeschere

df3_analysisidhere_operationidhere <- df2_analysisidhere |>
        dplyr::summarise(res = n_distinct(ana_varhere)) |>
        dplyr::mutate(AnalysisId = 'analysisidhere',
               MethodId = 'methodidhere',
               OperationId = 'operationidhere',
               OutputId = 'outputidhere',
               pattern = 'patternhere')

"
              code <- gsub('operationidhere', operid, template)
              code <- gsub('operationnamehere', opername, code)
              code <- gsub('operationdeschere', operdesc, code)
              # code <- gsub('operorderhere', operorder, code)
              code <- gsub('analysisidhere', analysisid, code)
              code <- gsub('methodidhere', methodid, code)
              code <- gsub('outputidhere', outputid, code)
              code <- gsub('ana_varhere', analysisvar, code)
              code <- gsub('patternhere', pattern, code)

              return(code)
            }

            code_Operation_tmp = func_OperationTmp10(oper_id,
                                                     # oper_order,
                                                     oper_name,
                                                     oper_desc,
                                                     Anas_j,
                                                     methodid,
                                                     Output,
                                                     ana_var,
                                                     oper_pattern)
            # Mth01_CatVar_Summ_ByGrp_2_pct ------------------

          } else if(operation$operation_id == "Mth01_CatVar_Summ_ByGrp_2_pct"){


            NUM_analysisid = Anas_s$referencedAnalysisOperations_analysisId1
            DEN_analysisid = Anas_s$referencedAnalysisOperations_analysisId2

            NUM_operationid = operation$operation_referencedResultRelationships1_operationId
            DEN_operationid = operation$operation_referencedResultRelationships2_operationId

            func_OperationTmp11 <- function(operid,
                                            # operorder,
                                            opername,
                                            operdesc,
                                            analysisid,
                                            methodid,
                                            outputid,
                                            analysisvar,
                                            num_analysisid,
                                            den_analysisid,
                                            num_operationid,
                                            den_operationid,
                                            pattern,
                                            groupvar1) {
              template <- "
# Operation ID:           operationidhere
# Operation name:         operationnamehere
# Operation description:  operationdeschere



df3_analysisidhere_operationidhere_num <- df3_num_analysisIDhere_num_operationIDhere |>
          dplyr::rename(NUM = res)

df3_analysisidhere_operationidhere_den <- df3_den_analysisIDhere_den_operationIDhere |>
          dplyr::rename(DEN = res)

df3_analysisidhere_operationidhere <- merge(df3_analysisidhere_operationidhere_num,
                                            df3_analysisidhere_operationidhere_den |>
                                                  dplyr::select(group1varhere, DEN),
                                            by = c('group1varhere')) |>
                                            dplyr::mutate(res = NUM / DEN * 100,
                                                   OperationId = 'operationidhere',
                                                   pattern = 'patternhere') |>
                                            dplyr::select(-NUM, -DEN)

"
              code <- gsub('operationidhere', operid, template)
              code <- gsub('operationnamehere', opername, code)
              code <- gsub('operationdeschere', operdesc, code)
              # code <- gsub('operorderhere', operorder, code)
              code <- gsub('analysisidhere', analysisid, code)
              code <- gsub('methodidhere', methodid, code)
              code <- gsub('outputidhere', outputid, code)
              code <- gsub('num_analysisIDhere', num_analysisid, code)
              code <- gsub('den_analysisIDhere', den_analysisid, code)
              code <- gsub('num_operationIDhere', num_operationid, code)
              code <- gsub('den_operationIDhere', den_operationid, code)
              code <- gsub('patternhere', pattern, code)
              code <- gsub('group1varhere', groupvar1, code)

              return(code)
            }

            code_Operation_tmp = func_OperationTmp11(oper_id,
                                                     # oper_order,
                                                     oper_name,
                                                     oper_desc,
                                                     Anas_j,
                                                     methodid,
                                                     Output,
                                                     ana_var,
                                                     NUM_analysisid,
                                                     DEN_analysisid,
                                                     NUM_operationid,
                                                     DEN_operationid,
                                                     oper_pattern,
                                                     AG_var1)

            # Mth04_ContVar_Comp_Anova_1_pval ----
          } else if(operation$operation_id == "Mth04_ContVar_Comp_Anova_1_pval"){

            func_OperationTmp12 <- function(operid,
                                            # operorder,
                                            opername,
                                            operdesc,
                                            analysisid,
                                            methodid,
                                            outputid,
                                            analysisvar,
                                            AGvar,
                                            pattern) {
              template <- "
# Operation ID:           operationidhere
# Operation name:         operationnamehere
# Operation description:  operationdeschere


fm <- stats::as.formula(paste('ana_varhere', '~', 'ana_groupvarhere'))
  model <- stats::lm(fm, data = df2_analysisidhere
  )

if (class(model) != 'lm') stop('Not an object of class lm ')
f <- summary(model)$fstatistic
p <- stats::pf(f[1],f[2],f[3],lower.tail=F)
attributes(p) <- NULL

df3_analysisidhere_operationidhere <- data.frame(res = p,
                  AnalysisId = 'analysisidhere',
                  MethodId = 'methodidhere',
                  OperationId = 'operationidhere',
                  OutputId = 'outputidhere',
                  pattern = 'patternhere')
"
              code <- gsub('operationidhere', operid, template)
              code <- gsub('operationnamehere', opername, code)
              code <- gsub('operationdeschere', operdesc, code)
              # code <- gsub('operorderhere', operorder, code)
              code <- gsub('analysisidhere', analysisid, code)
              code <- gsub('methodidhere', methodid, code)
              code <- gsub('outputidhere', outputid, code)
              code <- gsub('ana_varhere', analysisvar, code)
              code <- gsub('ana_groupvarhere', AGvar, code)
              code <- gsub('patternhere', pattern, code)

              return(code)
            }

            code_Operation_tmp = func_OperationTmp12(oper_id,
                                                     # oper_order,
                                                     oper_name,
                                                     oper_desc,
                                                     Anas_j,
                                                     methodid,
                                                     Output,
                                                     ana_var,
                                                     AG_var1,
                                                     oper_pattern)
            # Mth03_CatVar_Comp_PChiSq_1_pval ----
          } else if(operation$operation_id == "Mth03_CatVar_Comp_PChiSq_1_pval"){

            func_OperationTmp13 <- function(operid,
                                            # operorder,
                                            opername,
                                            operdesc,
                                            analysisid,
                                            methodid,
                                            outputid,
                                            analysisvar,
                                            AGvar1,
                                            AGvar2,
                                            ana_adam,
                                            pattern) {
              template <- "
# Operation ID:           operationidhere
# Operation name:         operationnamehere
# Operation description:  operationdeschere

tab <- table(adamhere[, c('ana_groupvar1here', 'ana_groupvar2here')])
p <- chisq.test(tab)$p.value

df3_analysisidhere_operationidhere <- data.frame(res = p,
                  AnalysisId = 'analysisidhere',
                  MethodId = 'methodidhere',
                  OperationId = 'operationidhere',
                  OutputId = 'outputidhere',
                  pattern = 'patternhere')

"
              code <- gsub('operationidhere', operid, template)
              code <- gsub('operationnamehere', opername, code)
              code <- gsub('operationdeschere', operdesc, code)
              # code <- gsub('operorderhere', operorder, code)
              code <- gsub('analysisidhere', analysisid, code)
              code <- gsub('methodidhere', methodid, code)
              code <- gsub('outputidhere', outputid, code)
              code <- gsub('ana_varhere', analysisvar, code)
              code <- gsub('ana_groupvar1here', AGvar1, code)
              code <- gsub('ana_groupvar2here', AGvar2, code)
              code <- gsub('adamhere', ana_adam, code)
              code <- gsub('patternhere', pattern, code)

              return(code)
            }

            code_Operation_tmp = func_OperationTmp13(oper_id,
                                                     # oper_order,
                                                     oper_name,
                                                     oper_desc,
                                                     Anas_j,
                                                     methodid,
                                                     Output,
                                                     ana_var,
                                                     AG_var1,
                                                     AG_var2,
                                                     ana_adam,
                                                     oper_pattern)

          } # operation loop ends

          code_Operation_0 = paste(code_Operation_0, # code contai
                                   code_Operation_tmp)

          if(k<nrow(method)) {
            code_combine = paste0(code_combine,
                                  "df3_",Anas_j, "_",oper_id, ", \n")
          } else {
            code_combine = paste0(code_combine,
                                  "df3_",Anas_j, "_",oper_id)
          }
        } # all operations end

        # code to combine it all --------------------------------------------------
        # dplyr::rename groups to append
        if(num_grp == 1){ # if 1 analysis grouping
          func_rename1 <- function(groupvar1) {
            template <- " |>
      dplyr::rename(Group1 = groupvar1here)
"
            code <- gsub('groupvar1here', groupvar1, template)

            return(code)
          }

          code_rename = func_rename1(AG_var1)

        }
        else if(num_grp == 2){ # if 2 analysis groupings
          func_rename2 <- function(groupvar1,
                                   groupvar2) {
            template <- " |>
      dplyr::rename(Group1 = groupvar1here,
             Group2 = groupvar2here)
"
            code <- gsub('groupvar1here', groupvar1, template)
            code <- gsub('groupvar2here', groupvar2, code)

            return(code)
          }
          code_rename = func_rename2(AG_var1,
                                     AG_var2)

        }
        else if(num_grp == 3){ # if 3 analysis groupings
          func_rename3 <- function(groupvar1,
                                   groupvar2,
                                   groupvar3) {
            template <- " |>
      dplyr::rename(Group1 = groupvar1here,
             Group2 = groupvar2here,
             Group3 = groupvar3here)
"
            code <- gsub('groupvar1here', groupvar1, template)
            code <- gsub('groupvar2here', groupvar2, code)
            code <- gsub('groupvar3here', groupvar3, code)

            return(code)
          }

          code_rename = func_rename3(AG_var1,
                                     AG_var2,
                                     AG_var3)
        } else code_rename = "" # if no analysis grouping


        assign(paste0("code_AnalysisMethod_", Anas_j),
               paste0("#Apply Operations within Method --- \n",
                      code_Operation_0,
                      "#Combine operation datasets: \n",
                      "df3_",Anas_j," <- dplyr::bind_rows(",
                      code_combine,
                      ")",
                      code_rename))
      }

    # Generate code for analysis ----------------------------------------------
    # if(j == 1){  #AnalysisSet only for first analysis
      if(example == FALSE){
        assign(paste0("code_",Anas_j),
               paste0("\n\n# Analysis ", Anas_j,"----\n#",
                      ana_name,
                      get(paste0("code_AnalysisSet_",Anas_j)),
                      #get(paste0("code_AnalysisGrouping_",Anas_j)),
                      get(paste0("code_DataSubset_",Anas_j)),
                      get(paste0("code_AnalysisMethod_",Anas_j))))
      } else {
        assign(paste0("code_",Anas_j),
               paste0("\n\n# Analysis ", Anas_j,"----\n#",
                      ana_name,
                      get(paste0("code_AnalysisSet_",Anas_j)),
                      get(paste0("code_AnalysisGrouping_",Anas_j)),
                      get(paste0("code_DataSubset_",Anas_j)),
                      get(paste0("code_AnalysisMethod_",Anas_j))))
      }
      # }


    run_code <- paste0(run_code,
                       get(paste0("code_",Anas_j)))

    if(j<max_j) {
      combine_analysis_code = paste0(combine_analysis_code,
                                     "df3_",Anas_j, ", \n")
    } else {
      combine_analysis_code = paste0(combine_analysis_code,
                                     "df3_",Anas_j)
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
  if(example == FALSE){

    if(shuffle == FALSE){
      assign(paste0("code_",Output),
             paste0(code_header,
                    code_libraries,
                    code_ADaM,
                    run_code,
                    "\n\n# combine analyses to create ARD ----\n",
                    "ARD <- dplyr::bind_rows(",
                    combine_analysis_code,
                    ") "
             )
      )
    } else {
      assign(paste0("code_",Output),
             paste0(code_header,
                    code_libraries,
                    code_ADaM,
                    run_code,
                    "\n\n# combine analyses to create ARD ----\n",
                    "ARD <- cards::bind_ard(",
                    combine_analysis_code,
                    ") %>%\n shuffle_ard() "
             )
      )
    }

  } else { # example = TRUE

    assign(paste0("code_",Output),
           paste0(code_header,
                  code_libraries,
                  code_ADaM,
                  run_code,
                  "\n\n# combine analyses to create ARD ----\n",
                  "df4 <- dplyr::bind_rows(",
                  combine_analysis_code,
                  ")\n\n #Apply pattern format:\n",
                  code_pattern
           )
    )
  }
  writeLines(get(paste0("code_",Output)),
             paste0(output_path,"/ARD_",Output,".R"))
  } # end of outputs
}
