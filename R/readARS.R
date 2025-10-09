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
                     spec_output = ""){

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
  if(spec_output != ""){
    Lopo <- Lopo %>%
      dplyr::filter(listItem_outputId == spec_output)

    Lopa <- Lopa %>%
      dplyr::filter(listItem_outputId == spec_output)
  }

  # Prework and loops ----------------------------------------------------

  for (i in 1:nrow(Lopo)) {
    Output = Lopo[i,]$listItem_outputId
    OutputName = Lopo[i,]$listItem_name

    Anas <- Lopa %>%    # get all analyses for current output
      dplyr::filter(listItem_outputId == Output,
                    listItem_analysisId %in% Analyses$id) # exclude if not methodid

    # Load ADaMs ----
    code_ADaM <- .generate_adam_loading_code(
      Anas = Anas,
      Analyses = Analyses,
      AnalysisSets = AnalysisSets,
      DataSubsets = DataSubsets,
      adam_path = adam_path
    )

    run_code <- ""    # variable to contain generated code
    combine_analysis_code <- "" # variable containing code to combine analyses

    # Programme header ----
    timenow <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    code_header <- .generate_program_header(Output, OutputName, timenow)

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

        # select 2nd Analysis in Output for identifying ADaM
        Anas_2 <- Anas[3, ]$listItem_analysisId
        Anas_s2 <- Analyses %>% # row from AN to get other IDs
          dplyr::filter(id == Anas_2)

        ana_adam2 <- Anas_s2$dataset

        # Anas_s2 <- Analyses %>% # row from AN to get other IDs
        #   dplyr::filter(id %in% Analyses_IDs)

        if(cond_adam == ana_adam2){    # if Analysis Set ADaM and Output ADaM are same

          func_AnalysisSet1 <- function(dataset, variable, oper, val, ASID, anSetName) {
            template <- "
# Apply Analysis Set ---
df_pop <- dplyr::filter(ADaM,
            var operator 'value')

df_poptot <- df_pop

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

df_poptot = dplyr::filter(ADaM,
            var operator 'value')
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

        # text to be used in DataSubsets:
        AnSetDataSubsets = "df_poptot"
      } else{ # AnalysisSet code for > 1st Analyses
        assign(paste0("code_AnalysisSet_",Anas_j),"")

        # text to be used in DataSubsets:
        AnSetDataSubsets = "df_pop"
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

        if(num_grp == 1){
          #cards part
          distinct_list <- paste0(AG_var1,", ",ana_var)
          by_listc <- paste0("'",AG_var1,"'")
          by_list <- paste0(AG_var1)
          by_stmt = paste0(", by = ",AG_var1)


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
          distinct_list <- ana_var
          by_stmt = ""
        }

      # Apply DataSubset -------------------------------------------------------------

      if(exists("DataSubsets")){ # if there is a data subset for the RE
        if(!is.na(subsetid)){ # if there is a data subset for this analysis
          subsetrule <- DataSubsets %>%
            dplyr::filter(id == subsetid)

          DSname <- subsetrule %>%
            dplyr::select(name) %>%
            unique() %>%
            as.character() %>%
            gsub("[\r\n]", " ", .)

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
              # handle blank value
              NEblankval <- FALSE
              val1_chr <- suppressWarnings(as.character(val1))[1]

              if (!is.null(vac) && !is.na(vac) && vac == "NE" &&
                  !is.null(val1_chr) && (identical(val1_chr, "") ||
                                         identical(val1_chr, "NA") ||
                                         is.na(val1_chr))) {
                NEblankval <- TRUE
              }
            }

            rFilt_final <- paste0(var," ", rvac," ",val)
            # handle blank value scenario:
            if(exists('NEblankval')){
              if(NEblankval == TRUE){
                rFilt_final = paste0("!is.na(", var, ") & ",var, "!= ''")
              } else rFilt_final = rFilt_final
            }

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
                      if(is_num == TRUE){  # value is numeric
                        val = suppressWarnings(as.numeric(val1))
                      } else{ # value is character
                          val =  paste0("'",val1,"'")
                      }
                      # handle blank value
                      NEblankval <- FALSE
                      # normalize to length-1 character if possible
                      val1_chr <- suppressWarnings(as.character(val1))[1]

                      if (!is.null(vac) && !is.na(vac) && vac == "NE" &&
                          !is.null(val1_chr) && (identical(val1_chr, "") ||
                                                 identical(val1_chr, "NA") ||
                                                 is.na(val1_chr))) {
                        NEblankval <- TRUE
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
                    # handle blank value
                    NEblankval <- FALSE
                    # normalize to length-1 character if possible
                    val1_chr <- suppressWarnings(as.character(val1))[1]

                    if (!is.null(vac) && !is.na(vac) && vac == "NE" &&
                        !is.null(val1_chr) && (identical(val1_chr, "") ||
                                               identical(val1_chr, "NA") ||
                                               is.na(val1_chr))) {
                      NEblankval <- TRUE
                    }
                  }
                  # concatenate expression
                  assign(paste("fexp", m,n, sep = "_"), paste0(var," ", f_vac," ", f_val))

                  if(n>1) assign('rcode', paste0(rcode, " LOGOP ",var," ", f_vac," ", f_val))
                  else assign('rcode', paste0(var," ", f_vac," ", f_val))

                }# end loop through rows
              } # end xlsx case

              # handle blank value scenario:
              if(exists('NEblankval')){
                if(NEblankval == TRUE){
                rcode = paste0("!is.na(", var, ") & ",var, "!= ''")
                } else rcode = rcode
              }

              assign(paste("rFilt", m, sep = "_"),
                     gsub("LOGOP", rlog_oper, rcode))
            } # end loop through levels

            # combine all dplyr::filter values:
            if(exists('rFilt_2')){

              rFilt_final <- paste(rFilt_1, rFilt_2, sep = ", ")
              rm(rFilt_2) #clear it so it doesn't exist for future
            } else rFilt_final <- rFilt_1

          } # end case where there are more than one rows

          func_DataSubset1 <- function(filterVal, ASID, DSNAME, Ansetds) {

              template <- "
# Apply Data Subset ---
# Data subset: dsnamehere
df2_analysisidhere <- ansetdshere |>
        dplyr::filter(dplyr::filtertext1)
"
            code <- gsub('dplyr::filtertext1', filterVal, template)
            code <- gsub('analysisidhere', ASID, code)
            code <- gsub('dsnamehere', DSNAME, code)
            code <- gsub('ansetdshere', Ansetds, code)

            return(code)
          }

          # code_DataSubset <- func_DataSubset(rFilt_final, Anas_j)
          assign(paste0("code_DataSubset_",Anas_j),
                 func_DataSubset1(rFilt_final,
                                  Anas_j,
                                  DSname,
                                  AnSetDataSubsets)
          )

        } else { # there is no data subsetting for this analysis

          func_DataSubset2 <- function(ASID, Ansetds) {
              template <- "

#Apply Data Subset ---
df2_analysisidhere <- ansetdshere

"

            code <- gsub('analysisidhere', ASID, template)
            code <- gsub('ansetdshere', Ansetds, code)
            return(code)
          } # end function

          assign(paste0("code_DataSubset_",Anas_j),
                 func_DataSubset2(Anas_j,
                                  AnSetDataSubsets)
          )
        } # end case where no data subsetting
      } # end case where no data subsetting for the entire RE

      else { # no data subset for the RE


        func_DataSubset3 <- function(ASID,
                                     Ansetds) {
            template <- "

#Apply Data Subset ---
df2_analysisidhere <- ansetdshere

"

          code <- gsub('analysisidhere', ASID, template)
          code <- gsub('ansetdshere', Ansetds, code)
          return(code)
        } # end function

        assign(paste0("code_DataSubset_",Anas_j),
               func_DataSubset3(Anas_j,
                                AnSetDataSubsets)
        )
      }

      # Apply AnalysisMethod -------------------------------------------------------------

        method <- AnalysisMethods %>%
          dplyr::filter(id == methodid) %>% # refnew
          dplyr::select(name, description, label, id) %>%
          unique()

      operations = AnalysisMethods %>%
        dplyr::filter(id == methodid) %>%
        dplyr::select(operation_id)

      operation = operations$operation_id
      for(i in seq_len(nrow(operations))){
        assign(paste0("operation_", i),
               operation[i]
               # ,
               # envir = .GlobalEnv
               )
      }

        methodname = method$name
        methoddesc = method$description
        methodlabel = method$label
        methodid = method$id

        # Code
        anmetcode <- AnalysisMethodCodeTemplate %>%
          dplyr::filter(method_id == methodid,
                        context %in% c("R", "R (siera)", "siera"),
                        specifiedAs == "Code") %>%
          dplyr::select(templateCode)

        # Parameters
        # to be replaced with Source values:
        anmetparam_s <- AnalysisMethodCodeParameters %>%
          dplyr::filter(method_id == methodid,
                        parameter_valueSource != "")

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

        code_method_tmp_2 = anmetcode_final

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

    # Generate code for analysis ----------------------------------------------

        assign(paste0("code_",Anas_j),
               paste0("\n\n# Analysis ", Anas_j,"----\n#",
                      ana_name,
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

  writeLines(get(paste0("code_",Output)),
             paste0(output_path,"/ARD_",Output,".R"))
  } # end of outputs
}
