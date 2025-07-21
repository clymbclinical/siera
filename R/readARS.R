#' Ingest ARS (Analysis Results Standard) metadata, produce ARD (Analysis Results Dataset) code for each output
#'
#' Ingest JSON or xlsx ARS (Analysis Results Standard) metadata, and meta-programme R scripts
#' that could be run as-is to produce Analysis Results Datasets when ingesting ADaM
#' datasets
#'
#' @param ARS_path A file (JSON or xlsx) containing ARS metadata for a reporting event
#' @param output_path Path to store .R ARD scripts
#' @param adam_path Path to folder containing ADaM datasets, to be run in ARD program
#'
#' @returns R programmes generating ARDs - one for each output specificied in the ARS metadata
#' @export
#'
#' @examples
#' # path to file containing ARS metadata
#' ARS_file <- ARS_example("ARS_V1_Common_Safety_Displays.json")
#'
#' # output path for R programs
#' output_dir = tempdir()
#'
#' # folder containing ADaM datasets
#' adam_folder = tempdir()
#'
#' # run function, write to temp directory
#' readARS(ARS_file, output_dir, adam_folder)
#'
readARS <- function(ARS_path, output_path = tempdir(), adam_path = tempdir()){
  # load libraries ----------------------------------------------------------

  func_libraries <- function(){
    template <- "

# load libraries ----
library(tidyverse)
library(readxl)
library(splitstackshape)
library(readr)
  "
    code <- template
    return(code)
  }

  code_libraries <- func_libraries()

  # Read in ARS (JSON) content ----------------------------------------------------
  json_from <- jsonlite::fromJSON(ARS_path)

  #otherListsOfContents (LOPO) --V1ized
  otherListsOfContents <- json_from$otherListsOfContents$contentsList$listItems[[1]]  # this is similar to xlsx
  Lopo <- otherListsOfContents # list of all planned outputs to loop JSONized

  #listOfPlannedAnalyses --V1ized
  mainListOfContents <- json_from$mainListOfContents$contentsList$listItems

  # loop through outputs to construct LOPA
  Lopa <- data.frame()
  for(a in 1:nrow(otherListsOfContents)){
    tmp_PO <- otherListsOfContents[a,]

    tmp_json_Lopa <-  # contains list with datasets with analysisIDs
      mainListOfContents$sublist$listItems[[a]]

    # gather anaIDs from anaysisID (Level 2)
    anaIds <- tmp_json_Lopa$analysisId %>%
      tibble::as_tibble() %>%
      dplyr::mutate(outputId = tmp_PO$outputId) %>%
      dplyr::rename(analysisId = value) %>%
      dplyr::filter(!is.na(analysisId))

    # bind analysisIDs
    Lopa <- rbind(Lopa, anaIds)

    # gather level 3 AnaIDs
    if("sublist" %in% names(tmp_json_Lopa)){ # check if there are level 3's

      tmp_json_lopa_sub <- tmp_json_Lopa$sublist$listItems

      forend <- length(tmp_json_lopa_sub) # amount of analyses datasets in json_lopa
      subana_dset <- data.frame() # initialise dataframe to contain datasets
      for(b in 2:forend){   # always(?) 1st row is empty
        ana_ids <- tmp_json_lopa_sub[[b]]$analysisId %>%
          tibble::as_tibble() %>%
          dplyr::mutate(outputId = tmp_PO$outputId) %>%
          dplyr::rename(analysisId = value)
        subana_dset <- rbind(subana_dset, ana_ids)
      }
      Lopa <- rbind(Lopa, subana_dset)
    }
  }

  #dataSubsets --- V1ized
  JSON_DataSubsets <- json_from$dataSubsets
  # level 1
  JSONDSL1 <- tibble::tibble(id = json_from$dataSubsets[["id"]],
                     name = json_from$dataSubsets[["name"]],
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
    tmp_DS_c <- JSON_DataSubsets[c,]

    if(!is.null(whereClauses[[c]])){ # check for level 2 existence

      # c = 6
      # scheck = whereClauses[[c]]
      tmp_DS <- tibble::tibble(level =  whereClauses[[c]][["level"]],
                       order = whereClauses[[c]][["order"]],
                       condition_dataset = whereClauses[[c]][["condition"]][["dataset"]],
                       condition_variable = whereClauses[[c]][["condition"]][["variable"]],
                       condition_comparator = whereClauses[[c]][["condition"]][["comparator"]],
                       condition_value = whereClauses[[c]][["condition"]][["value"]],
                       compoundExpression_logicalOperator =  whereClauses[[c]]$compoundExpression$logicalOperator,
                       id = tmp_DSID,
                       name = tmp_DSname)
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
                             name = tmp_DSname)

          JSONDSL3 = dplyr::bind_rows(JSONDSL3,tmp_DSL2)
        }
      }
    }
  }

  DataSubsets <- dplyr::bind_rows(JSONDSL1, JSONDSL2, JSONDSL3) %>%
    dplyr::arrange(id, level, order) # --JSONIZED! cHECK DIFFERENCE IN CONDITION_VALUE

  DataSubsets$condition_value[DataSubsets$condition_value == 'NULL'] = NA

  # DataSubsets_OG <- read_excel("ARSFILE.xlsx",
  #                              sheet = 'DataSubsets')

  # Analysis Sets -- V1ized
  # AnalysisSets <- read_excel("ARSFILE.xlsx",
  #                            sheet = 'AnalysisSets')   # jsonized

  AnalysisSets <- tibble::tibble(id = json_from$analysisSets$id,
                         label = json_from$analysisSets$label,
                         name = json_from$analysisSets$name,
                         level = json_from$analysisSets$level,
                         order = json_from$analysisSets$order,
                         condition_dataset = json_from$analysisSets$condition[["dataset"]],
                         condition_variable = json_from$analysisSets$condition[["variable"]],
                         condition_comparator = json_from$analysisSets$condition[["comparator"]],
                         condition_value = json_from$analysisSets$condition[["value"]])

  # Analysis Groupings
  # AnalysisGroupings <- read_excel("ARSFILE.xlsx",
  #                                 sheet = 'AnalysisGroupings')
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
          all.x = TRUE)


  # Analyses <- read_excel("ARSFILE.xlsx",
  #                        sheet = 'Analyses')

  #AM

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

  # JSONIZED!
  AnalysisMethods <- merge(JSONAML1,
                           JSONAML2,
                           by = "id",
                           all = TRUE) %>%
    merge(JSONAML3,
          by = "operation_id",
          all = TRUE)

  # Prework and loops ----------------------------------------------------


  # for (i in 1:1) {            # loop through outputs
  for (i in 1:nrow(Lopo)) {
    Output = Lopo[i,]$outputId
    OutputName = Lopo[i,]$name

    Anas <- Lopa %>%    # get all analyses for current output
      dplyr::filter(outputId == Output)

  # Load ADaMs ----
    a1 <- ""
    # Combine unique datasets from both dataframes
    Analyses_IDs <- Analyses %>%
      dplyr::filter(id %in% Anas$analysisId) %>%
      dplyr::select(dataset) %>%
      unique()

    AnalysisSets_Ids <- AnalysisSets %>%
      dplyr::filter(id %in% Anas$analysisId) %>%
      dplyr::select(condition_dataset) %>%
      dplyr::rename(dataset = condition_dataset) %>%
      unique()

    unique_datasets <- rbind(Analyses_IDs, AnalysisSets_Ids) %>%
      unique()
    # Loop through the unique dataset list once
    for (ad in unique_datasets) {
      ad_path <- paste0("adampathhere/", ad, ".csv")  # Construct the file path
      a1 <- paste0(a1, ad, " <- read_csv(\'", ad_path, "\')\n")  # Append each line
    }
    a1 <- gsub("adampathhere", adam_path, a1, fixed = TRUE)
    code_ADaM_1 <- unique(paste(a1, sep = ""))
    code_ADaM <- paste0("\n# Load ADaM -------\n",
                        paste(code_ADaM_1, collapse = "\n"))


    run_code <- ""    # variable to contain generated code
    combine_analysis_code <- "" # variable containing code to combine analyses

    # Programme header ----
    timenow <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    func_header <- function(OutputId, OutputName, date){
      template <- "
# Programme:    Generate code to produce ARD for outputidhere
# Output:       outputnamehere
# Date created: datehere


  "
      code <- gsub('outputidhere', OutputId, template)
      code <- gsub('outputnamehere', OutputName, code)
      code <- gsub('datehere', date, code)
      return(code)
    }

    code_header <- func_header(Output, OutputName, timenow)

    # loop through individual analyses
    max_j = nrow(Anas)
    for (j in 1:max_j) {
      # for (j in 1:nrow(Anas)) {

      #Analysis
      Anas_j <- Anas[j, ]$analysisId  # AnalysisID from PA to dplyr::filter AN -jsonized
      Anas_s <- Analyses %>% # row from AN to get other IDs
        dplyr::filter(id == Anas_j)

      ana_adam <- Anas_s$dataset # ADaM used for this analysis (esp. to be used in ChiSq)

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
      subsetid <- Anas_s$dataSubsetId # data subset ID (to be used in DS

      # Method
      methodid <- Anas_s$method_id # data subset ID (to be used in DS


      # Apply Analysis Set -----
      # ana_setId = "AnalysisSet_02_SAF"
      temp_AnSet <- AnalysisSets %>%  # get analysis set for this iteration
        dplyr::filter(id == ana_setId)

      cond_adam <- temp_AnSet %>% # ADaM for this analysis set
        dplyr::select(condition_dataset) %>%
        as.character()

      cond_var <- temp_AnSet %>% # condition variable for this analysis set
        dplyr::select(condition_variable) %>%
        as.character()

      cond_oper <- temp_AnSet %>% # condition operator for this analysis set
        dplyr::select(condition_comparator) %>%
        as.character()

      cond_val <- temp_AnSet %>% # condition value for this analysis set
        dplyr::select(condition_value) %>%
        unlist()

      anSetName <- temp_AnSet %>% # condition value for this analysis set
        dplyr::select(label)%>%
        as.character()

      if(cond_oper == "EQ") { # convert to R code
        oper <-  '=='
      }

      if(cond_adam == ana_adam){    # if Analysis Set ADaM and Analysis ADaM are same

        func_AnalysisSet1 <- function(dataset, variable, oper, val, ASID, anSetName) {
          template <- "
# Apply Analysis Set ---
# Analysis set :  Analysissetnamehere
df_analysisidhere <- dplyr::filter(ADaM,
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
                                       # anavar,
                                        ASID,
                                        anaADaM,
                                        anSetName) {
            template <- "
# Apply Analysis Set ---
# Analysis set :  Analysissetnamehere
df_analysisidhere <- dplyr::filter(ADaM,
            var operator 'value') %>%
            #dplyr::select(anasetvrhere) %>%
            merge(analysisADAMhere,
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
                                   ana_adam,
                                   anSetName))
        }


      # Apply Grouping ----------------------------------------------------------------


      # determine maximum groupings
      column_names <- colnames(Analyses)

      # Filter column names that match the pattern "Group"
      group_columns <- column_names[grep("^groupingId[0-9]+$", column_names)]

      # Extract the numeric part of the group columns and find the maximum
      group_numbers <- as.numeric(sub("groupingId", "", group_columns))
      max_group_number <- max(group_numbers)


      if(max_group_number >=1) {
        AG_temp1 <- AnalysisGroupings %>%
          dplyr::filter(id == groupid1)

        AG_var1 <- AG_temp1 %>%
          dplyr::select(groupingVariable) %>%
          unique() %>%
          as.character()

        #get the number of dplyr::group_by to perform in Grouping Apply
        if(resultsByGroup1 == TRUE && !is.na(resultsByGroup1)){
          num_grp <- 1
        } else num_grp = 0

        if(max_group_number >= 2){
          AG_temp2 <- AnalysisGroupings %>%
            dplyr::filter(id == groupid2)

          AG_var2 <- AG_temp2 %>%
            dplyr::select(groupingVariable) %>%
            unique() %>%
            as.character()

          #get the number of dplyr::group_by to perform in Grouping Apply
          if(resultsByGroup1 == TRUE && !is.na(resultsByGroup1)){
            num_grp <- 1
            if(resultsByGroup2 == TRUE && !is.na(resultsByGroup2)) {
              num_grp <- 2
            }
          } else num_grp = 0

          if(max_group_number >= 3){

            AG_temp3 <- AnalysisGroupings %>%
              dplyr::filter(id == groupid3)

            AG_var3 <- AG_temp3 %>%
              dplyr::select(groupingVariable) %>%
              unique() %>%
              as.character()

            #get the number of dplyr::group_by to perform in Grouping Apply
            if(resultsByGroup1 == TRUE && !is.na(resultsByGroup1)){
              num_grp <- 1
              if(resultsByGroup2 == TRUE && !is.na(resultsByGroup2)) {
                num_grp <- 2
                if(resultsByGroup3 == TRUE && !is.na(resultsByGroup3)) {
                  num_grp <- 3
                }
              }
            } else num_grp = 0

          }
        }
      }

      if(num_grp == 1){
        func_AnalysisGrouping1 <- function(var1, ASID) {

          template <- "

#Apply Analysis Grouping ---
df1_analysisidhere <- df_analysisidhere %>%
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
df1_analysisidhere <- df_analysisidhere %>%
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
df1_analysisidhere <- df_analysisidhere %>%
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
      # cat(code_AnalysisGrouping_0)

      # Apply DataSubset -------------------------------------------------------------

      if(exists("DataSubsets")){ # if there is a data subset for the RE
        if(!is.na(subsetid)){ # if there is a data subset for this analysis
          subsetrule <- DataSubsets %>%
            dplyr::filter(id == subsetid)

          DSname <- subsetrule %>%
            dplyr::select(name) %>%
            unique() %>%
            as.character()

          if(nrow(subsetrule) == 1){      # if there's only one row

            #dset?
            var = subsetrule$condition_variable
            val1 = stringr::str_trim(subsetrule$condition_value)
            vac = subsetrule$condition_comparator

            # R code
            if(vac == "EQ") rvac = '=='
            if(vac == "NE") rvac = '!='
            if(vac == "GT") rvac = '>'
            if(vac == "GE") rvac = '>='
            if(vac == "LT") rvac = '<'
            if(vac == "LE") rvac = '<='
            rFilt_final <- paste0(var," ", rvac," ", "'",val1,"'")

          } else  {                       # if there are more than one rows

            for (m in 1:(max(subsetrule$level) - 1)){   #loop through levels
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

              for (n in 1:nrow(lev)) {

                ord1_ <- lev[n, ] # one row at a time

                # assign the variables
                var = ord1_$condition_variable

                vac = ord1_$condition_comparator

                val1 = ord1_$condition_value

                if(vac == "IN") {
                  f_vac = "%in%"
                }# define operator in R code
                else { # vac is EQ or NE
                  if(vac == "EQ") f_vac = "==" # define operator in R code
                  else f_vac = "!=" #
                }
                # concatenate expression
                assign(paste("fexp", m,n, sep = "_"), paste0(var," ", f_vac," ", "'",val1,"'"))

                if(n>1) assign('rcode', paste0(rcode, " LOGOP ",var," ", f_vac," ", "'",val1,"'"))
                else assign('rcode', paste0(var," ", f_vac," ", "'",val1,"'"))

              } # end loop through rows
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
            template <- "

# Apply Data Subset ---
# Data subset: dsnamehere
df2_analysisidhere <- df1_analysisidhere %>%
        dplyr::filter(dplyr::filtertext1)

"
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
          # cat(code_DataSubset)
          # eval(parse(text=code_DataSubset))


        } else { # there is no data subsetting for this analysis

          func_DataSubset2 <- function(ASID) {
            template <- "

#Apply Data Subset ---
df2_analysisidhere <- df1_analysisidhere

"
            code <- gsub('analysisidhere', ASID, template)
            return(code)
          } # end function

          # code_DataSubset <- func_DataSubset(rFilt_final, Anas_j)
          assign(paste0("code_DataSubset_",Anas_j),
                 func_DataSubset2(Anas_j)
          )
        } # end case where no data subsetting
      } # end case where no data subsetting for the entire RE

      else { # no data subset for the RE

        func_DataSubset3 <- function(ASID) {
          template <- "

#Apply Data Subset ---
df2_analysisidhere <- df1_analysisidhere

"
          code <- gsub('analysisidhere', ASID, template)
          return(code)
        } # end function

        # code_DataSubset <- func_DataSubset(rFilt_final, Anas_j)
        assign(paste0("code_DataSubset_",Anas_j),
               func_DataSubset3(Anas_j)
        )
      }
      # Apply AnalysisMethod -------------------------------------------------------------
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

df3_analysisidhere_operationidhere <- df2_analysisidhere %>%
        dplyr::summarise(res = n()) %>%
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

df3_analysisidhere_operationidhere <- df2_analysisidhere %>%
        dplyr::summarise(res = n()) %>%
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

df3_analysisidhere_operationidhere <- df2_analysisidhere %>%
        dplyr::summarise(res = mean(ana_varhere)) %>%
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

df3_analysisidhere_operationidhere <- df2_analysisidhere %>%
        dplyr::summarise(res = sd(ana_varhere)) %>%
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

df3_analysisidhere_operationidhere <- df2_analysisidhere %>%
        dplyr::summarise(res = median(ana_varhere)) %>%
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

df3_analysisidhere_operationidhere <- df2_analysisidhere %>%
        dplyr::summarise(res = quantile(ana_varhere, c(.25), na.rm = TRUE)) %>%
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

df3_analysisidhere_operationidhere <- df2_analysisidhere %>%
        dplyr::summarise(res = quantile(ana_varhere, c(.75), na.rm = TRUE)) %>%
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

df3_analysisidhere_operationidhere <- df2_analysisidhere %>%
        dplyr::summarise(res = min(ana_varhere)) %>%
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

df3_analysisidhere_operationidhere <- df2_analysisidhere %>%
        dplyr::summarise(res = max(ana_varhere)) %>%
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

df3_analysisidhere_operationidhere <- df2_analysisidhere %>%
        dplyr::summarise(res = n_distinct(ana_varhere)) %>%
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



df3_analysisidhere_operationidhere_num <- df3_num_analysisIDhere_num_operationIDhere %>%
          dplyr::rename(NUM = res)

df3_analysisidhere_operationidhere_den <- df3_den_analysisIDhere_den_operationIDhere %>%
          dplyr::rename(DEN = res)

df3_analysisidhere_operationidhere <- merge(df3_analysisidhere_operationidhere_num,
                                            df3_analysisidhere_operationidhere_den %>%
                                                  dplyr::select(group1varhere, DEN),
                                            by = c('group1varhere')) %>%
                                            dplyr::mutate(res = NUM / DEN * 100,
                                                   OperationId = 'operationidhere',
                                                   pattern = 'patternhere') %>%
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
          template <- " %>%
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
          template <- " %>%
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


      assign(paste0("code_Operation_", Anas_j),
             paste0("#Apply Operations --- \n",
                    code_Operation_0,
                    "#Combine operation datasets: \n",
                    "df3_",Anas_j," <- dplyr::bind_rows(",
                    code_combine,
                    ")",
                    code_rename))

      # Generate code for analysis ----------------------------------------------

      assign(paste0("code_",Anas_j),
             paste0("\n\n# Analysis ", Anas_j,"----",
                    get(paste0("code_AnalysisSet_",Anas_j)),
                    get(paste0("code_AnalysisGrouping_",Anas_j)),
                    get(paste0("code_DataSubset_",Anas_j)),
                    get(paste0("code_Operation_",Anas_j))))

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


    code_pattern <- paste0('ARD_',
                           gsub('-', '_', Output),
                           "<- df4 %>%
      dplyr::mutate(dec = ifelse(grepl('X.X',
                                df4$pattern, ),
                          stringr::str_count(substr(df4$pattern,
                                          str_locate(df4$pattern,
                                                    'X.X')[, 1]+2,
                                          nchar(df4$pattern)), 'X'),
                          0)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(rnd = round(res, dec)) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(disp = ifelse(grepl('\\\\(N=', df4$pattern),
                           paste0('(N=', rnd, ')'),
                           ifelse(grepl('\\\\(', df4$pattern),
                                  paste0('(', rnd, ')'),
                                  as.character(rnd)))) %>%
                         dplyr::select(-rnd, -dec)")


    # add all code, combine analyses ARDs and apply pattern
    assign(paste0("code_",Output),
           paste0(code_header,
                  code_libraries,
                  code_ADaM,
                  run_code,
                  "\n\n# combine analyses to create ARD ----\n",
                  "df4 <- dplyr::bind_rows(",
                  combine_analysis_code,
                  ")\n\n #Apply pattern format:\n",
                  code_pattern)
    )

    writeLines(get(paste0("code_",Output)),
              paste0(output_path,"/ARD_",Output,".R"))


  } # end of outputs
}
