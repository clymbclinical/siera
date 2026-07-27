
# Programme:    Generate code to produce ARD for Out14-3-1-1
# Output:       Overall Summary of Treatment-Emergent Adverse Events
# Date created: 2026-07-27 09:10:09

  # load libraries ----
    library(dplyr)
    library(readxl)
    library(readr)
    library(cards)
    library(cardx)
    library(broom)
    library(parameters)
    library(tidyr)
  
# Load ADaM -------
ADSL <- readr::read_csv(siera::ARS_example("ADSL.csv"),
                                      show_col_types = FALSE,
                                      progress = FALSE) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ tidyr::replace_na(.x, '')))
ADAE <- readr::read_csv(siera::ARS_example("ADAE.csv"),
                                      show_col_types = FALSE,
                                      progress = FALSE) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ tidyr::replace_na(.x, '')))


# Analysis An01_05_SAF_Summ_ByTrt----
#Summary of Subjects by Treatment
# Apply Analysis Set ---
overlap <- intersect(names(ADSL), names(ADAE))
overlapfin <- setdiff(overlap, 'USUBJID')
df_pop <- dplyr::filter(ADSL,
            SAFFL == 'Y') |>
            merge(ADAE |> dplyr::select(-dplyr::all_of(overlapfin)),
                  by = 'USUBJID',
                  all = FALSE)
df_poptot = dplyr::filter(ADSL,
            SAFFL == 'Y')

#Apply Data Subset ---
df2_An01_05_SAF_Summ_ByTrt <- df_poptot

#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth01_CatVar_Count_ByGrp
# Method name:            Count by group for a categorical variable
# Method description:     Count across groups for a categorical variable, based on subject occurrence

if(nrow(df2_An01_05_SAF_Summ_ByTrt) != 0) {
                              in_data = df2_An01_05_SAF_Summ_ByTrt |>
    dplyr::select(USUBJID, TRT01A) |>
    unique()
df3_An01_05_SAF_Summ_ByTrt <-
  cards::ard_tabulate(
    data = in_data
    , variables = 'TRT01A'
  ) |>
dplyr::filter(stat_name == 'n') |>
 dplyr::mutate(operationid = 'Mth01_CatVar_Count_ByGrp_1_n')}
if(nrow(df2_An01_05_SAF_Summ_ByTrt) != 0){
df3_An01_05_SAF_Summ_ByTrt <- df3_An01_05_SAF_Summ_ByTrt |>
        dplyr::mutate(AnalysisId = 'An01_05_SAF_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Count_ByGrp',
               OutputId = 'Out14-3-1-1')
} else {
    df3_An01_05_SAF_Summ_ByTrt = data.frame(AnalysisId = 'An01_05_SAF_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Count_ByGrp',
               OutputId = 'Out14-3-1-1')
}
    df3_An01_05_SAF_Summ_ByTrt <- df3_An01_05_SAF_Summ_ByTrt |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# Analysis An07_01_TEAE_Summ_ByTrt----
#Summary of Subjects with At Least One TEAE, by Treatment# Apply Data Subset ---
# Data subset: Treatment-Emergent Adverse Events
df2_An07_01_TEAE_Summ_ByTrt <- df_pop |>
        dplyr::filter(TRTEMFL == 'Y')
#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth01_CatVar_Summ_ByGrp
# Method name:            Summary by group of a categorical variable
# Method description:     Descriptive summary statistics across groups for a categorical variable, based on subject occurrence

if(nrow(df2_An07_01_TEAE_Summ_ByTrt) != 0) {
                              denom_dataset = df2_An01_05_SAF_Summ_ByTrt |>
  dplyr::select(TRT01A)

in_data = df2_An07_01_TEAE_Summ_ByTrt |>
    dplyr::distinct(TRT01A, USUBJID) |>
    dplyr::mutate(dummy = 'dummyvar')

dataDriven = FALSE
if(dataDriven == TRUE){
df3_An07_01_TEAE_Summ_ByTrt <-
  cards::ard_tabulate(
    data = in_data,
    strata = c('TRT01A'),
    variables = 'dummy',
    denominator = denom_dataset
  ) } else {
df3_An07_01_TEAE_Summ_ByTrt <-
 cards::ard_tabulate(
    data = in_data,
    by = c('TRT01A'),
    variables = 'dummy',
    denominator = denom_dataset
  ) }
df3_An07_01_TEAE_Summ_ByTrt <- df3_An07_01_TEAE_Summ_ByTrt|>
dplyr::filter(stat_name %in% c('n', 'p')) |>
dplyr::mutate(operationid = dplyr::case_when(stat_name == 'n' ~ 'Mth01_CatVar_Summ_ByGrp_1_n',
                                                              stat_name == 'p' ~ 'Mth01_CatVar_Summ_ByGrp_2_pct'))}
if(nrow(df2_An07_01_TEAE_Summ_ByTrt) != 0){
df3_An07_01_TEAE_Summ_ByTrt <- df3_An07_01_TEAE_Summ_ByTrt |>
        dplyr::mutate(AnalysisId = 'An07_01_TEAE_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OutputId = 'Out14-3-1-1')
} else {
    df3_An07_01_TEAE_Summ_ByTrt = data.frame(AnalysisId = 'An07_01_TEAE_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OutputId = 'Out14-3-1-1')
}
    if(nrow(df2_An07_01_TEAE_Summ_ByTrt) != 0){
df3_An07_01_TEAE_Summ_ByTrt <- df3_An07_01_TEAE_Summ_ByTrt |>
  dplyr::mutate(
      group1_groupingId = 'AnlsGrouping_01_Trt',
      group1_groupId = dplyr::case_when(
        as.character(group1_level) == 'Placebo' ~ 'AnlsGrouping_01_Trt_1',
        as.character(group1_level) == 'Xanomeline Low Dose' ~ 'AnlsGrouping_01_Trt_2',
        as.character(group1_level) == 'Xanomeline High Dose' ~ 'AnlsGrouping_01_Trt_3',
        TRUE ~ NA_character_
      )
  )
}
df3_An07_01_TEAE_Summ_ByTrt <- df3_An07_01_TEAE_Summ_ByTrt |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# Analysis An07_02_RelTEAE_Summ_ByTrt----
#Summary of Subjects with At Least One Related TEAE, by Treatment# Apply Data Subset ---
# Data subset: Related Treatment-Emergent Adverse Events
df2_An07_02_RelTEAE_Summ_ByTrt <- df_pop |>
        dplyr::filter(TRTEMFL == 'Y' & AEREL %in% c('POSSIBLE ', 'PROBABLE'))
#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth01_CatVar_Summ_ByGrp
# Method name:            Summary by group of a categorical variable
# Method description:     Descriptive summary statistics across groups for a categorical variable, based on subject occurrence

if(nrow(df2_An07_02_RelTEAE_Summ_ByTrt) != 0) {
                              denom_dataset = df2_An01_05_SAF_Summ_ByTrt |>
  dplyr::select(TRT01A)

in_data = df2_An07_02_RelTEAE_Summ_ByTrt |>
    dplyr::distinct(TRT01A, USUBJID) |>
    dplyr::mutate(dummy = 'dummyvar')

dataDriven = FALSE
if(dataDriven == TRUE){
df3_An07_02_RelTEAE_Summ_ByTrt <-
  cards::ard_tabulate(
    data = in_data,
    strata = c('TRT01A'),
    variables = 'dummy',
    denominator = denom_dataset
  ) } else {
df3_An07_02_RelTEAE_Summ_ByTrt <-
 cards::ard_tabulate(
    data = in_data,
    by = c('TRT01A'),
    variables = 'dummy',
    denominator = denom_dataset
  ) }
df3_An07_02_RelTEAE_Summ_ByTrt <- df3_An07_02_RelTEAE_Summ_ByTrt|>
dplyr::filter(stat_name %in% c('n', 'p')) |>
dplyr::mutate(operationid = dplyr::case_when(stat_name == 'n' ~ 'Mth01_CatVar_Summ_ByGrp_1_n',
                                                              stat_name == 'p' ~ 'Mth01_CatVar_Summ_ByGrp_2_pct'))}
if(nrow(df2_An07_02_RelTEAE_Summ_ByTrt) != 0){
df3_An07_02_RelTEAE_Summ_ByTrt <- df3_An07_02_RelTEAE_Summ_ByTrt |>
        dplyr::mutate(AnalysisId = 'An07_02_RelTEAE_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OutputId = 'Out14-3-1-1')
} else {
    df3_An07_02_RelTEAE_Summ_ByTrt = data.frame(AnalysisId = 'An07_02_RelTEAE_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OutputId = 'Out14-3-1-1')
}
    if(nrow(df2_An07_02_RelTEAE_Summ_ByTrt) != 0){
df3_An07_02_RelTEAE_Summ_ByTrt <- df3_An07_02_RelTEAE_Summ_ByTrt |>
  dplyr::mutate(
      group1_groupingId = 'AnlsGrouping_01_Trt',
      group1_groupId = dplyr::case_when(
        as.character(group1_level) == 'Placebo' ~ 'AnlsGrouping_01_Trt_1',
        as.character(group1_level) == 'Xanomeline Low Dose' ~ 'AnlsGrouping_01_Trt_2',
        as.character(group1_level) == 'Xanomeline High Dose' ~ 'AnlsGrouping_01_Trt_3',
        TRUE ~ NA_character_
      )
  )
}
df3_An07_02_RelTEAE_Summ_ByTrt <- df3_An07_02_RelTEAE_Summ_ByTrt |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# Analysis An07_03_SerTEAE_Summ_ByTrt----
#Summary of Subjects with At Least One Serious TEAE, by Treatment# Apply Data Subset ---
# Data subset: Serious Treatment-Emergent Adverse Events
df2_An07_03_SerTEAE_Summ_ByTrt <- df_pop |>
        dplyr::filter(TRTEMFL == 'Y' & AESER == 'Y')
#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth01_CatVar_Summ_ByGrp
# Method name:            Summary by group of a categorical variable
# Method description:     Descriptive summary statistics across groups for a categorical variable, based on subject occurrence

if(nrow(df2_An07_03_SerTEAE_Summ_ByTrt) != 0) {
                              denom_dataset = df2_An01_05_SAF_Summ_ByTrt |>
  dplyr::select(TRT01A)

in_data = df2_An07_03_SerTEAE_Summ_ByTrt |>
    dplyr::distinct(TRT01A, USUBJID) |>
    dplyr::mutate(dummy = 'dummyvar')

dataDriven = FALSE
if(dataDriven == TRUE){
df3_An07_03_SerTEAE_Summ_ByTrt <-
  cards::ard_tabulate(
    data = in_data,
    strata = c('TRT01A'),
    variables = 'dummy',
    denominator = denom_dataset
  ) } else {
df3_An07_03_SerTEAE_Summ_ByTrt <-
 cards::ard_tabulate(
    data = in_data,
    by = c('TRT01A'),
    variables = 'dummy',
    denominator = denom_dataset
  ) }
df3_An07_03_SerTEAE_Summ_ByTrt <- df3_An07_03_SerTEAE_Summ_ByTrt|>
dplyr::filter(stat_name %in% c('n', 'p')) |>
dplyr::mutate(operationid = dplyr::case_when(stat_name == 'n' ~ 'Mth01_CatVar_Summ_ByGrp_1_n',
                                                              stat_name == 'p' ~ 'Mth01_CatVar_Summ_ByGrp_2_pct'))}
if(nrow(df2_An07_03_SerTEAE_Summ_ByTrt) != 0){
df3_An07_03_SerTEAE_Summ_ByTrt <- df3_An07_03_SerTEAE_Summ_ByTrt |>
        dplyr::mutate(AnalysisId = 'An07_03_SerTEAE_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OutputId = 'Out14-3-1-1')
} else {
    df3_An07_03_SerTEAE_Summ_ByTrt = data.frame(AnalysisId = 'An07_03_SerTEAE_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OutputId = 'Out14-3-1-1')
}
    if(nrow(df2_An07_03_SerTEAE_Summ_ByTrt) != 0){
df3_An07_03_SerTEAE_Summ_ByTrt <- df3_An07_03_SerTEAE_Summ_ByTrt |>
  dplyr::mutate(
      group1_groupingId = 'AnlsGrouping_01_Trt',
      group1_groupId = dplyr::case_when(
        as.character(group1_level) == 'Placebo' ~ 'AnlsGrouping_01_Trt_1',
        as.character(group1_level) == 'Xanomeline Low Dose' ~ 'AnlsGrouping_01_Trt_2',
        as.character(group1_level) == 'Xanomeline High Dose' ~ 'AnlsGrouping_01_Trt_3',
        TRUE ~ NA_character_
      )
  )
}
df3_An07_03_SerTEAE_Summ_ByTrt <- df3_An07_03_SerTEAE_Summ_ByTrt |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# Analysis An07_04_RelSerTEAE_Summ_ByTrt----
#Summary of Subjects with At Least One Related Serious TEAE, by Treatment# Apply Data Subset ---
# Data subset: Related Serious Treatment-Emergent Adverse Events
df2_An07_04_RelSerTEAE_Summ_ByTrt <- df_pop |>
        dplyr::filter(TRTEMFL == 'Y' & AEREL %in% c('POSSIBLE ', 'PROBABLE') & AESER == 'Y')
#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth01_CatVar_Summ_ByGrp
# Method name:            Summary by group of a categorical variable
# Method description:     Descriptive summary statistics across groups for a categorical variable, based on subject occurrence

if(nrow(df2_An07_04_RelSerTEAE_Summ_ByTrt) != 0) {
                              denom_dataset = df2_An01_05_SAF_Summ_ByTrt |>
  dplyr::select(TRT01A)

in_data = df2_An07_04_RelSerTEAE_Summ_ByTrt |>
    dplyr::distinct(TRT01A, USUBJID) |>
    dplyr::mutate(dummy = 'dummyvar')

dataDriven = FALSE
if(dataDriven == TRUE){
df3_An07_04_RelSerTEAE_Summ_ByTrt <-
  cards::ard_tabulate(
    data = in_data,
    strata = c('TRT01A'),
    variables = 'dummy',
    denominator = denom_dataset
  ) } else {
df3_An07_04_RelSerTEAE_Summ_ByTrt <-
 cards::ard_tabulate(
    data = in_data,
    by = c('TRT01A'),
    variables = 'dummy',
    denominator = denom_dataset
  ) }
df3_An07_04_RelSerTEAE_Summ_ByTrt <- df3_An07_04_RelSerTEAE_Summ_ByTrt|>
dplyr::filter(stat_name %in% c('n', 'p')) |>
dplyr::mutate(operationid = dplyr::case_when(stat_name == 'n' ~ 'Mth01_CatVar_Summ_ByGrp_1_n',
                                                              stat_name == 'p' ~ 'Mth01_CatVar_Summ_ByGrp_2_pct'))}
if(nrow(df2_An07_04_RelSerTEAE_Summ_ByTrt) != 0){
df3_An07_04_RelSerTEAE_Summ_ByTrt <- df3_An07_04_RelSerTEAE_Summ_ByTrt |>
        dplyr::mutate(AnalysisId = 'An07_04_RelSerTEAE_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OutputId = 'Out14-3-1-1')
} else {
    df3_An07_04_RelSerTEAE_Summ_ByTrt = data.frame(AnalysisId = 'An07_04_RelSerTEAE_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OutputId = 'Out14-3-1-1')
}
    if(nrow(df2_An07_04_RelSerTEAE_Summ_ByTrt) != 0){
df3_An07_04_RelSerTEAE_Summ_ByTrt <- df3_An07_04_RelSerTEAE_Summ_ByTrt |>
  dplyr::mutate(
      group1_groupingId = 'AnlsGrouping_01_Trt',
      group1_groupId = dplyr::case_when(
        as.character(group1_level) == 'Placebo' ~ 'AnlsGrouping_01_Trt_1',
        as.character(group1_level) == 'Xanomeline Low Dose' ~ 'AnlsGrouping_01_Trt_2',
        as.character(group1_level) == 'Xanomeline High Dose' ~ 'AnlsGrouping_01_Trt_3',
        TRUE ~ NA_character_
      )
  )
}
df3_An07_04_RelSerTEAE_Summ_ByTrt <- df3_An07_04_RelSerTEAE_Summ_ByTrt |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# Analysis An07_05_TEAELd2Dth_Summ_ByTrt----
#Summary of Subjects with At Least One TEAE Leading to Death, by Treatment# Apply Data Subset ---
# Data subset: Treatment-Emergent Adverse Events Leading to Death
df2_An07_05_TEAELd2Dth_Summ_ByTrt <- df_pop |>
        dplyr::filter(TRTEMFL == 'Y' & AESDTH == 'Y')
#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth01_CatVar_Summ_ByGrp
# Method name:            Summary by group of a categorical variable
# Method description:     Descriptive summary statistics across groups for a categorical variable, based on subject occurrence

if(nrow(df2_An07_05_TEAELd2Dth_Summ_ByTrt) != 0) {
                              denom_dataset = df2_An01_05_SAF_Summ_ByTrt |>
  dplyr::select(TRT01A)

in_data = df2_An07_05_TEAELd2Dth_Summ_ByTrt |>
    dplyr::distinct(TRT01A, USUBJID) |>
    dplyr::mutate(dummy = 'dummyvar')

dataDriven = FALSE
if(dataDriven == TRUE){
df3_An07_05_TEAELd2Dth_Summ_ByTrt <-
  cards::ard_tabulate(
    data = in_data,
    strata = c('TRT01A'),
    variables = 'dummy',
    denominator = denom_dataset
  ) } else {
df3_An07_05_TEAELd2Dth_Summ_ByTrt <-
 cards::ard_tabulate(
    data = in_data,
    by = c('TRT01A'),
    variables = 'dummy',
    denominator = denom_dataset
  ) }
df3_An07_05_TEAELd2Dth_Summ_ByTrt <- df3_An07_05_TEAELd2Dth_Summ_ByTrt|>
dplyr::filter(stat_name %in% c('n', 'p')) |>
dplyr::mutate(operationid = dplyr::case_when(stat_name == 'n' ~ 'Mth01_CatVar_Summ_ByGrp_1_n',
                                                              stat_name == 'p' ~ 'Mth01_CatVar_Summ_ByGrp_2_pct'))}
if(nrow(df2_An07_05_TEAELd2Dth_Summ_ByTrt) != 0){
df3_An07_05_TEAELd2Dth_Summ_ByTrt <- df3_An07_05_TEAELd2Dth_Summ_ByTrt |>
        dplyr::mutate(AnalysisId = 'An07_05_TEAELd2Dth_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OutputId = 'Out14-3-1-1')
} else {
    df3_An07_05_TEAELd2Dth_Summ_ByTrt = data.frame(AnalysisId = 'An07_05_TEAELd2Dth_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OutputId = 'Out14-3-1-1')
}
    if(nrow(df2_An07_05_TEAELd2Dth_Summ_ByTrt) != 0){
df3_An07_05_TEAELd2Dth_Summ_ByTrt <- df3_An07_05_TEAELd2Dth_Summ_ByTrt |>
  dplyr::mutate(
      group1_groupingId = 'AnlsGrouping_01_Trt',
      group1_groupId = dplyr::case_when(
        as.character(group1_level) == 'Placebo' ~ 'AnlsGrouping_01_Trt_1',
        as.character(group1_level) == 'Xanomeline Low Dose' ~ 'AnlsGrouping_01_Trt_2',
        as.character(group1_level) == 'Xanomeline High Dose' ~ 'AnlsGrouping_01_Trt_3',
        TRUE ~ NA_character_
      )
  )
}
df3_An07_05_TEAELd2Dth_Summ_ByTrt <- df3_An07_05_TEAELd2Dth_Summ_ByTrt |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# Analysis An07_06_RelTEAELd2Dth_Summ_ByTrt----
#Summary of Subjects with At Least One Related TEAE Leading to Death, by Treatment# Apply Data Subset ---
# Data subset: Related Treatment-Emergent Adverse Events Leading to Death
df2_An07_06_RelTEAELd2Dth_Summ_ByTrt <- df_pop |>
        dplyr::filter(TRTEMFL == 'Y' & AESDTH == 'Y', AEREL == 'POSSIBLE' | AEREL == 'PROBABLE')
#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth01_CatVar_Summ_ByGrp
# Method name:            Summary by group of a categorical variable
# Method description:     Descriptive summary statistics across groups for a categorical variable, based on subject occurrence

if(nrow(df2_An07_06_RelTEAELd2Dth_Summ_ByTrt) != 0) {
                              denom_dataset = df2_An01_05_SAF_Summ_ByTrt |>
  dplyr::select(TRT01A)

in_data = df2_An07_06_RelTEAELd2Dth_Summ_ByTrt |>
    dplyr::distinct(TRT01A, USUBJID) |>
    dplyr::mutate(dummy = 'dummyvar')

dataDriven = FALSE
if(dataDriven == TRUE){
df3_An07_06_RelTEAELd2Dth_Summ_ByTrt <-
  cards::ard_tabulate(
    data = in_data,
    strata = c('TRT01A'),
    variables = 'dummy',
    denominator = denom_dataset
  ) } else {
df3_An07_06_RelTEAELd2Dth_Summ_ByTrt <-
 cards::ard_tabulate(
    data = in_data,
    by = c('TRT01A'),
    variables = 'dummy',
    denominator = denom_dataset
  ) }
df3_An07_06_RelTEAELd2Dth_Summ_ByTrt <- df3_An07_06_RelTEAELd2Dth_Summ_ByTrt|>
dplyr::filter(stat_name %in% c('n', 'p')) |>
dplyr::mutate(operationid = dplyr::case_when(stat_name == 'n' ~ 'Mth01_CatVar_Summ_ByGrp_1_n',
                                                              stat_name == 'p' ~ 'Mth01_CatVar_Summ_ByGrp_2_pct'))}
if(nrow(df2_An07_06_RelTEAELd2Dth_Summ_ByTrt) != 0){
df3_An07_06_RelTEAELd2Dth_Summ_ByTrt <- df3_An07_06_RelTEAELd2Dth_Summ_ByTrt |>
        dplyr::mutate(AnalysisId = 'An07_06_RelTEAELd2Dth_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OutputId = 'Out14-3-1-1')
} else {
    df3_An07_06_RelTEAELd2Dth_Summ_ByTrt = data.frame(AnalysisId = 'An07_06_RelTEAELd2Dth_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OutputId = 'Out14-3-1-1')
}
    if(nrow(df2_An07_06_RelTEAELd2Dth_Summ_ByTrt) != 0){
df3_An07_06_RelTEAELd2Dth_Summ_ByTrt <- df3_An07_06_RelTEAELd2Dth_Summ_ByTrt |>
  dplyr::mutate(
      group1_groupingId = 'AnlsGrouping_01_Trt',
      group1_groupId = dplyr::case_when(
        as.character(group1_level) == 'Placebo' ~ 'AnlsGrouping_01_Trt_1',
        as.character(group1_level) == 'Xanomeline Low Dose' ~ 'AnlsGrouping_01_Trt_2',
        as.character(group1_level) == 'Xanomeline High Dose' ~ 'AnlsGrouping_01_Trt_3',
        TRUE ~ NA_character_
      )
  )
}
df3_An07_06_RelTEAELd2Dth_Summ_ByTrt <- df3_An07_06_RelTEAELd2Dth_Summ_ByTrt |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# Analysis An07_07_TEAELd2DoseMod_Summ_ByTrt----
#Summary of Subjects with At Least One TEAE Leading to Dose Modification, by Treatment# Apply Data Subset ---
# Data subset: Treatment-Emergent Adverse Events Leading to Dose Modification
df2_An07_07_TEAELd2DoseMod_Summ_ByTrt <- df_pop |>
        dplyr::filter(TRTEMFL == 'Y' & AEACN %in% c('DOSE REDUCED ', 'DRUG INTERRUPTED'))
#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth01_CatVar_Summ_ByGrp
# Method name:            Summary by group of a categorical variable
# Method description:     Descriptive summary statistics across groups for a categorical variable, based on subject occurrence

if(nrow(df2_An07_07_TEAELd2DoseMod_Summ_ByTrt) != 0) {
                              denom_dataset = df2_An01_05_SAF_Summ_ByTrt |>
  dplyr::select(TRT01A)

in_data = df2_An07_07_TEAELd2DoseMod_Summ_ByTrt |>
    dplyr::distinct(TRT01A, USUBJID) |>
    dplyr::mutate(dummy = 'dummyvar')

dataDriven = FALSE
if(dataDriven == TRUE){
df3_An07_07_TEAELd2DoseMod_Summ_ByTrt <-
  cards::ard_tabulate(
    data = in_data,
    strata = c('TRT01A'),
    variables = 'dummy',
    denominator = denom_dataset
  ) } else {
df3_An07_07_TEAELd2DoseMod_Summ_ByTrt <-
 cards::ard_tabulate(
    data = in_data,
    by = c('TRT01A'),
    variables = 'dummy',
    denominator = denom_dataset
  ) }
df3_An07_07_TEAELd2DoseMod_Summ_ByTrt <- df3_An07_07_TEAELd2DoseMod_Summ_ByTrt|>
dplyr::filter(stat_name %in% c('n', 'p')) |>
dplyr::mutate(operationid = dplyr::case_when(stat_name == 'n' ~ 'Mth01_CatVar_Summ_ByGrp_1_n',
                                                              stat_name == 'p' ~ 'Mth01_CatVar_Summ_ByGrp_2_pct'))}
if(nrow(df2_An07_07_TEAELd2DoseMod_Summ_ByTrt) != 0){
df3_An07_07_TEAELd2DoseMod_Summ_ByTrt <- df3_An07_07_TEAELd2DoseMod_Summ_ByTrt |>
        dplyr::mutate(AnalysisId = 'An07_07_TEAELd2DoseMod_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OutputId = 'Out14-3-1-1')
} else {
    df3_An07_07_TEAELd2DoseMod_Summ_ByTrt = data.frame(AnalysisId = 'An07_07_TEAELd2DoseMod_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OutputId = 'Out14-3-1-1')
}
    if(nrow(df2_An07_07_TEAELd2DoseMod_Summ_ByTrt) != 0){
df3_An07_07_TEAELd2DoseMod_Summ_ByTrt <- df3_An07_07_TEAELd2DoseMod_Summ_ByTrt |>
  dplyr::mutate(
      group1_groupingId = 'AnlsGrouping_01_Trt',
      group1_groupId = dplyr::case_when(
        as.character(group1_level) == 'Placebo' ~ 'AnlsGrouping_01_Trt_1',
        as.character(group1_level) == 'Xanomeline Low Dose' ~ 'AnlsGrouping_01_Trt_2',
        as.character(group1_level) == 'Xanomeline High Dose' ~ 'AnlsGrouping_01_Trt_3',
        TRUE ~ NA_character_
      )
  )
}
df3_An07_07_TEAELd2DoseMod_Summ_ByTrt <- df3_An07_07_TEAELd2DoseMod_Summ_ByTrt |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# Analysis An07_08_TEAELd2TrtDsc_Summ_ByTrt----
#Summary of Subjects with At Least One TEAE Leading to Treatment Discontinuation, by Treatment# Apply Data Subset ---
# Data subset: Treatment-Emergent Adverse Events Leading to Treatment Discontinuation
df2_An07_08_TEAELd2TrtDsc_Summ_ByTrt <- df_pop |>
        dplyr::filter(TRTEMFL == 'Y' & AEACN == 'DRUG WITHDRAWN')
#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth01_CatVar_Summ_ByGrp
# Method name:            Summary by group of a categorical variable
# Method description:     Descriptive summary statistics across groups for a categorical variable, based on subject occurrence

if(nrow(df2_An07_08_TEAELd2TrtDsc_Summ_ByTrt) != 0) {
                              denom_dataset = df2_An01_05_SAF_Summ_ByTrt |>
  dplyr::select(TRT01A)

in_data = df2_An07_08_TEAELd2TrtDsc_Summ_ByTrt |>
    dplyr::distinct(TRT01A, USUBJID) |>
    dplyr::mutate(dummy = 'dummyvar')

dataDriven = FALSE
if(dataDriven == TRUE){
df3_An07_08_TEAELd2TrtDsc_Summ_ByTrt <-
  cards::ard_tabulate(
    data = in_data,
    strata = c('TRT01A'),
    variables = 'dummy',
    denominator = denom_dataset
  ) } else {
df3_An07_08_TEAELd2TrtDsc_Summ_ByTrt <-
 cards::ard_tabulate(
    data = in_data,
    by = c('TRT01A'),
    variables = 'dummy',
    denominator = denom_dataset
  ) }
df3_An07_08_TEAELd2TrtDsc_Summ_ByTrt <- df3_An07_08_TEAELd2TrtDsc_Summ_ByTrt|>
dplyr::filter(stat_name %in% c('n', 'p')) |>
dplyr::mutate(operationid = dplyr::case_when(stat_name == 'n' ~ 'Mth01_CatVar_Summ_ByGrp_1_n',
                                                              stat_name == 'p' ~ 'Mth01_CatVar_Summ_ByGrp_2_pct'))}
if(nrow(df2_An07_08_TEAELd2TrtDsc_Summ_ByTrt) != 0){
df3_An07_08_TEAELd2TrtDsc_Summ_ByTrt <- df3_An07_08_TEAELd2TrtDsc_Summ_ByTrt |>
        dplyr::mutate(AnalysisId = 'An07_08_TEAELd2TrtDsc_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OutputId = 'Out14-3-1-1')
} else {
    df3_An07_08_TEAELd2TrtDsc_Summ_ByTrt = data.frame(AnalysisId = 'An07_08_TEAELd2TrtDsc_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OutputId = 'Out14-3-1-1')
}
    if(nrow(df2_An07_08_TEAELd2TrtDsc_Summ_ByTrt) != 0){
df3_An07_08_TEAELd2TrtDsc_Summ_ByTrt <- df3_An07_08_TEAELd2TrtDsc_Summ_ByTrt |>
  dplyr::mutate(
      group1_groupingId = 'AnlsGrouping_01_Trt',
      group1_groupId = dplyr::case_when(
        as.character(group1_level) == 'Placebo' ~ 'AnlsGrouping_01_Trt_1',
        as.character(group1_level) == 'Xanomeline Low Dose' ~ 'AnlsGrouping_01_Trt_2',
        as.character(group1_level) == 'Xanomeline High Dose' ~ 'AnlsGrouping_01_Trt_3',
        TRUE ~ NA_character_
      )
  )
}
df3_An07_08_TEAELd2TrtDsc_Summ_ByTrt <- df3_An07_08_TEAELd2TrtDsc_Summ_ByTrt |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# combine analyses to create ARD ----
ARD <- dplyr::bind_rows(df3_An01_05_SAF_Summ_ByTrt, 
df3_An07_01_TEAE_Summ_ByTrt, 
df3_An07_02_RelTEAE_Summ_ByTrt, 
df3_An07_03_SerTEAE_Summ_ByTrt, 
df3_An07_04_RelSerTEAE_Summ_ByTrt, 
df3_An07_05_TEAELd2Dth_Summ_ByTrt, 
df3_An07_06_RelTEAELd2Dth_Summ_ByTrt, 
df3_An07_07_TEAELd2DoseMod_Summ_ByTrt, 
df3_An07_08_TEAELd2TrtDsc_Summ_ByTrt) 

# Format results per ARS resultPattern (res / pattern / disp) ----
.format_ars_result <- function (value, pattern) 
{
    if (is.null(pattern) || length(pattern) != 1L || is.na(pattern) || 
        !nzchar(pattern)) {
        return(NA_character_)
    }
    if (!is.numeric(value)) {
        value <- suppressWarnings(as.numeric(value))
    }
    if (is.null(value) || length(value) != 1L || is.na(value) || 
        !is.finite(value)) {
        return(NA_character_)
    }
    dec <- 0L
    dec_match <- regmatches(pattern, regexpr("\\.X+", pattern))
    if (length(dec_match) == 1L) {
        dec <- nchar(dec_match) - 1L
    }
    scale <- 10^dec
    rounded <- sign(value) * trunc(abs(value) * scale + 0.5 + 
        sqrt(.Machine$double.eps))/scale
    num <- formatC(rounded, format = "f", digits = dec)
    x_pos <- gregexpr("X", pattern, fixed = TRUE)[[1L]]
    if (x_pos[1L] != -1L) {
        prefix <- substr(pattern, 1L, x_pos[1L] - 1L)
        suffix <- substr(pattern, x_pos[length(x_pos)] + 1L, 
            nchar(pattern))
        return(paste0(prefix, num, suffix))
    }
    if (endsWith(pattern, ")")) {
        return(paste0(substr(pattern, 1L, nchar(pattern) - 1L), 
            num, ")"))
    }
    paste0(pattern, num)
}

.op_patterns <- data.frame(
  operationid = c(
    "Mth01_CatVar_Count_ByGrp_1_n",
    "Mth01_CatVar_Summ_ByGrp_1_n",
    "Mth01_CatVar_Summ_ByGrp_2_pct",
    "Mth02_ContVar_Summ_ByGrp_1_n",
    "Mth02_ContVar_Summ_ByGrp_2_Mean",
    "Mth02_ContVar_Summ_ByGrp_3_SD",
    "Mth02_ContVar_Summ_ByGrp_4_Median",
    "Mth02_ContVar_Summ_ByGrp_5_Q1",
    "Mth02_ContVar_Summ_ByGrp_6_Q3",
    "Mth02_ContVar_Summ_ByGrp_7_Min",
    "Mth02_ContVar_Summ_ByGrp_8_Max",
    "Mth03_CatVar_Comp_PChiSq_1_pval",
    "Mth04_ContVar_Comp_Anova_1_pval"
  ),
  pattern = c(
    "(N=XX)",
    "XXX",
    "( XX.X)",
    "XX",
    "XX.X",
    "(XX.XX)",
    "XX.X",
    "XX.X",
    "XX.X",
    "XX",
    "XX",
    "X.XXXX",
    "X.XXXX"
  ),
  stringsAsFactors = FALSE
)

if ("stat" %in% names(ARD)) {
  ARD$res <- if (is.list(ARD$stat)) {
    vapply(ARD$stat, function(v)
      if (is.null(v) || length(v) == 0) NA_real_
      else suppressWarnings(as.numeric(v[[1]])),
      numeric(1))
  } else {
    suppressWarnings(as.numeric(ARD$stat))
  }
} else {
  ARD$res <- NA_real_
}
if ("operationid" %in% names(ARD)) {
  ARD <- dplyr::left_join(ARD, .op_patterns, by = "operationid")
} else {
  ARD$pattern <- NA_character_
}

# {cards} proportions (stat_name == 'p') are 0-1; patterns expect percent.
.fmt_val <- ARD$res
if ("stat_name" %in% names(ARD)) {
  .is_prop <- !is.na(ARD$stat_name) & ARD$stat_name == "p"
  .fmt_val[.is_prop] <- ARD$res[.is_prop] * 100
}
ARD$disp <- vapply(seq_len(nrow(ARD)), function(.i)
  .format_ars_result(.fmt_val[.i], ARD$pattern[.i]), character(1))

# Drop {cards}' internal format-function list-columns (not printable).
ARD <- dplyr::select(ARD, -dplyr::any_of(c("fmt_fun", "fmt_fn")))

