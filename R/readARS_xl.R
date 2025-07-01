#' Ingest ARS (Analysis Results Standard) metadata, produce ARD (Analysis Results Dataset) code for each output
#'
#' Ingest ARS (Analysis Results Standard) metadata, and meta-programme R scripts
#' that could be run as-is to produce Analysis Results Datasets when ingesting ADaM
#' datasets
#'
#' @param ARS_path A file containing ARS metadata for a reporting event
#' @param output_path Path to store .R ARD scripts
#' @param adam_path Path to folder containing ADaM datasets, to be run in ARD program
#' @param spec_output The output ID for a specific output to be run from the metadata
#' @param spec_analysis The analysis ID for a specific analysis to be run from the metadata
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
#' readARS_xl(ARS_path, output_dir, adam_folder)
#'
#'
#'

readARS_xl <- function(ARS_path,
                       output_path = tempdir(),
                       adam_path = tempdir(),
                       spec_output = "",
                       spec_analysis = ""){
  # load libraries ----------------------------------------------------------

  func_libraries <- function(){
    template <- "

# load libraries ----
library(dplyr)
library(readxl)
library(splitstackshape)
library(readr)
library(cards)
library(cardx)
library(broom)
library(parameters)
library(tidyr)
  "
    code <- template
    return(code)
  }

  code_libraries <- func_libraries()

  # Read in ARS xlsx content ----------------------------------------------------

  ARS_xlsx = ARS_path

  ListOfPlannedAnalyses <- read_excel(ARS_xlsx,
                                      sheet = 'MainListOfContents')
  ListOfPlannedOutputs <- read_excel(ARS_xlsx,
                                     sheet = 'OtherListsOfContents')
  DataSubsets <- read_excel(ARS_xlsx,
                            sheet = 'DataSubsets')
  AnalysisSets <- read_excel(ARS_xlsx,
                             sheet = 'AnalysisSets')
  AnalysisGroupings <- read_excel(ARS_xlsx,
                                  sheet = 'AnalysisGroupings')
  Analyses <- read_excel(ARS_xlsx,
                         sheet = 'Analyses')
  AnalysisMethods <- read_excel(ARS_xlsx,
                                sheet = 'AnalysisMethods')
  AnalysisMethods <- read_excel(ARS_xlsx,
                                sheet = 'AnalysisMethods')
  AnalysisMethodCodeTemplate <- read_excel(ARS_xlsx,
                                           sheet = 'AnalysisMethodCodeTemplate')
  AnalysisMethodCodeParameters <- read_excel(ARS_xlsx,
                                             sheet = 'AnalysisMethodCodeParameters')

  # specific output
  if(spec_output == ""){
    Lopo <- ListOfPlannedOutputs # list of all planned outputs to loop

    Lopa <- ListOfPlannedAnalyses %>%  # list of all planned analyses to loop
      tidyr::fill(listItem_outputId) %>%
      dplyr::filter(!is.na(listItem_analysisId)) %>%
      dplyr::select(listItem_analysisId, listItem_outputId)

  } else{
    Lopo <- ListOfPlannedOutputs %>%
      dplyr::filter(listItem_outputId == spec_output)

    Lopa <- ListOfPlannedAnalyses %>%  # list of all planned analyses to loop
      tidyr::fill(listItem_outputId) %>%
      dplyr::filter(!is.na(listItem_analysisId)) %>%
      dplyr::select(listItem_analysisId, listItem_outputId) %>%
      dplyr::filter(listItem_outputId == spec_output)
  }

  # specific analysis
  if(spec_analysis != ""){
    Lopa <- ListOfPlannedAnalyses %>%  # list of all planned analyses to loop
      tidyr::fill(listItem_outputId) %>%
      dplyr::filter(!is.na(listItem_analysisId)) %>%
      dplyr::select(listItem_analysisId, listItem_outputId) %>%
      dplyr::filter(listItem_analysisId == spec_analysis)

    output_ded = Lopa %>%
      dplyr::select(listItem_outputId) %>%
      unique() %>%
      as.character()

    Lopo <- ListOfPlannedOutputs %>%
      dplyr::filter(listItem_outputId == output_ded)
  }

  # Prework and loops ----------------------------------------------------

  max_i = nrow(Lopo)
  # for (i in 1:1) {            # loop through outputs
  for (i in 1:max_i) {
    Output = Lopo[i,]$listItem_outputId
    OutputName = Lopo[i,]$listItem_name

    Anas <- Lopa %>%    # get all analyses for current output
      dplyr::filter(listItem_outputId == Output)

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
      Anas_j <- Anas[j, ]$listItem_analysisId  # AnalysisID from
      #PA to dplyr::filter AN -jsonized
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

      # code for conditional statement for anSet
      anset_cond_stm = paste0(cond_var, oper,"'",cond_val,"'")

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
                                      #anavar,
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
          fishersrow = subsetrule %>%
            dplyr::filter(condition_dataset == AG_ds1)

          # assign the variables
          fishervar = fishersrow$condition_variable

          fishervac = fishersrow$condition_comparator

          fisherval1 = fishersrow$condition_value


          if(nrow(fishersrow) > 0) { # if we need a DS statement


            if(fishervac == "IN") {
              fisher_f_vac = "%in%"

              fisher_f_val = paste0("'", trimws(unlist(strsplit(fisherval1, "\\|"))), "'", collapse = ",")
            }# define operator in R code
            else { # vac is EQ or NE
              if(fishervac == "EQ") fisher_f_vac = "==" # define operator in R code
              else fisher_f_vac = "!=" #
              fisher_f_val = paste0("'", fisherval1,"'")
            }
            # concatenate expression
            fisher_cond_stm = paste0(", ",
                                     fishervar,
                                     " ",
                                     fisher_f_vac," "
                                     , "c(",
                                     fisher_f_val,")")
          } else { # if the DS statement should be blank for fisher's
            fisher_cond_stm = ""

          }

          ### end Fisher's exact test to filter


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

                  f_val = paste0("'", trimws(unlist(strsplit(val1, "\\|"))), "'", collapse = ",")
                }# define operator in R code
                else { # vac is EQ or NE
                  if(vac == "EQ") f_vac = "==" # define operator in R code
                  else f_vac = "!=" #
                  f_val = paste0("'", val1,"'")
                }
                # concatenate expression
                assign(paste("fexp", m,n, sep = "_"), paste0(var," ", f_vac," ", "'",f_val,"'"))

                if(n>1) assign('rcode', paste0(rcode, " LOGOP ",var," ", f_vac," ", "c(",f_val,")"))
                else assign('rcode', paste0(var," ", f_vac," ", "c(",f_val,")"))

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
df2_analysisidhere <- df_analysisidhere %>%
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
df2_analysisidhere <- df_analysisidhere

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
df2_analysisidhere <- df_analysisidhere

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
        dplyr::filter(id == methodid) %>% # refnew
        dplyr::select(name, description, label, id) %>%
        unique()

      methodname = method$name
      methoddesc = method$description
      methodlabel = method$label
      methodid = method$id


      # Code
      anmetcode <- AnalysisMethodCodeTemplate %>%
        dplyr::filter(method_id == methodid,
                      context == "R",
                      specifiedAs == "Code") %>%
        dplyr::select(templateCode)

      # Parameters
      # to be replaced with Source values:
      anmetparam_s <- AnalysisMethodCodeParameters %>%
        dplyr::filter(method_id == methodid,
                      parameter_valueSource != "")

      # to be replaced with values:
      anmetparam_v <- AnalysisMethodCodeParameters %>%
        dplyr::filter(method_id == methodid,
                      parameter_value != "")

      transpose_yn = anmetparam_v %>%
        dplyr::filter(parameter_name == "transpose") %>%
        dplyr::select(parameter_value) %>%
        as.character()

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
        anmetcode_temp <- gsub(anmetparam_s$parameter_name[i],
                               rep,
                               anmetcode_temp)
      }
      anmetcode_final <- gsub('methodidhere', methodid, anmetcode_temp)
      anmetcode_final <- gsub('analysisidhere', Anas_j, anmetcode_final)

      # applying transpose code
      if(transpose_yn == "Y"){
        code_method_tmp_2 = paste0(trimws(anmetcode_final %>%
                                            as.character()), " %>%
        pivot_longer(c(", operation_list_string, "),
        names_to = 'operation_id',
        values_to = 'res')")
      } else {
        code_method_tmp_2 = anmetcode_final
      }

      # mutate part

      template <-
        "
if(nrow(df2_analysisidhere) != 0){
df3_analysisidhere <- df3_analysisidhere %>%
        dplyr::mutate(AnalsysisId = 'analysisidhere',
               MethodId = 'methodidhere',
               OutputId = 'outputidhere') %>%
        dplyr::mutate(across(where(is.list), ~ map_chr(., ~ {
    if (is.null(.x)) {
      NA_character_
    } else {
      tryCatch(toString(.x), error = function(e) NA_character_)
    }
  })),
              stat = as.numeric(stat))
} else {
    df3_analysisidhere = data.frame(AnalsysisId = 'analysisidhere',
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


    assign(paste0("code_AnalysisMethod_", Anas_j),
           paste0("#Apply Methods --- \n",
                  code_method#,
                  #code_rename
           ))

    # Generate code for analysis ----------------------------------------------

    assign(paste0("code_",Anas_j),
           paste0("\n\n# Analysis ", Anas_j,"----",
                  get(paste0("code_AnalysisSet_",Anas_j)),
                  #get(paste0("code_AnalysisGrouping_",Anas_j)),
                  get(paste0("code_DataSubset_",Anas_j)),
                  get(paste0("code_AnalysisMethod_",Anas_j))))

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
                ")\n\n #Apply pattern format:\n"#,
                #code_pattern
         )
  )

  writeLines(get(paste0("code_",Output)),
             paste0(output_path,"/ARD_",Output,".R"))


  } # end of outputs
}
