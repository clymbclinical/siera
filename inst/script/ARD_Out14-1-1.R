
# Programme:    Generate code to produce ARD for Out14-1-1
# Output:       Summary of Demographics
# Date created: 2026-07-22 09:04:59

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


# Analysis An01_05_SAF_Summ_ByTrt----
#Summary of Subjects by Treatment
# Apply Analysis Set ---
df_pop <- dplyr::filter(ADSL,
            SAFFL == 'Y')
df_poptot <- df_pop

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
               OutputId = 'Out14-1-1')
} else {
    df3_An01_05_SAF_Summ_ByTrt = data.frame(AnalysisId = 'An01_05_SAF_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Count_ByGrp',
               OutputId = 'Out14-1-1')
}
    df3_An01_05_SAF_Summ_ByTrt <- df3_An01_05_SAF_Summ_ByTrt |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# Analysis An03_01_Age_Summ_ByTrt----
#Summary of Age by Treatment
#Apply Data Subset ---
df2_An03_01_Age_Summ_ByTrt <- df_pop

#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth02_ContVar_Summ_ByGrp
# Method name:            Summary by group of a continuous variable
# Method description:     Descriptive summary statistics across groups for a continuous variable

if(nrow(df2_An03_01_Age_Summ_ByTrt) != 0) {
                              df3_An03_01_Age_Summ_ByTrt <-
  cards::ard_summary(
    data = df2_An03_01_Age_Summ_ByTrt,
    by = c('TRT01A'),
    variables = AGE
  ) |>
dplyr::mutate(operationid = dplyr::case_when(stat_name == 'N' ~ 'Mth02_ContVar_Summ_ByGrp_1_n',
                                                                     stat_name == 'mean' ~ 'Mth02_ContVar_Summ_ByGrp_2_Mean',
                                                                     stat_name == 'sd' ~ 'Mth02_ContVar_Summ_ByGrp_3_SD',
                                                                     stat_name == 'median' ~ 'Mth02_ContVar_Summ_ByGrp_4_Median',
                                                                     stat_name == 'p25' ~ 'Mth02_ContVar_Summ_ByGrp_5_Q1',
                                                                     stat_name == 'p75' ~ 'Mth02_ContVar_Summ_ByGrp_6_Q3',
                                                                     stat_name == 'min' ~ 'Mth02_ContVar_Summ_ByGrp_7_Min',
                                                                     stat_name == 'max' ~ 'Mth02_ContVar_Summ_ByGrp_8_Max'))}
if(nrow(df2_An03_01_Age_Summ_ByTrt) != 0){
df3_An03_01_Age_Summ_ByTrt <- df3_An03_01_Age_Summ_ByTrt |>
        dplyr::mutate(AnalysisId = 'An03_01_Age_Summ_ByTrt',
               MethodId = 'Mth02_ContVar_Summ_ByGrp',
               OutputId = 'Out14-1-1')
} else {
    df3_An03_01_Age_Summ_ByTrt = data.frame(AnalysisId = 'An03_01_Age_Summ_ByTrt',
               MethodId = 'Mth02_ContVar_Summ_ByGrp',
               OutputId = 'Out14-1-1')
}
    if(nrow(df2_An03_01_Age_Summ_ByTrt) != 0){
df3_An03_01_Age_Summ_ByTrt <- df3_An03_01_Age_Summ_ByTrt |>
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
df3_An03_01_Age_Summ_ByTrt <- df3_An03_01_Age_Summ_ByTrt |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# Analysis An03_01_Age_Comp_ByTrt----
#Comparison of Age by Treatment
#Apply Data Subset ---
df2_An03_01_Age_Comp_ByTrt <- df_pop

#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth04_ContVar_Comp_Anova
# Method name:            Analysis of variance group comparison for a continuous variable
# Method description:     Comparison of groups by analysis of variance (ANOVA) for a continuous variable

if(nrow(df2_An03_01_Age_Comp_ByTrt) != 0) {
                              df3_An03_01_Age_Comp_ByTrt <- 
    cardx::ard_stats_aov(AGE ~ TRT01A, data = df2_An03_01_Age_Comp_ByTrt) |>
dplyr::filter(stat_name == 'p.value') |>
dplyr::mutate(operationid = 'Mth04_ContVar_Comp_Anova_1_pval')}
if(nrow(df2_An03_01_Age_Comp_ByTrt) != 0){
df3_An03_01_Age_Comp_ByTrt <- df3_An03_01_Age_Comp_ByTrt |>
        dplyr::mutate(AnalysisId = 'An03_01_Age_Comp_ByTrt',
               MethodId = 'Mth04_ContVar_Comp_Anova',
               OutputId = 'Out14-1-1')
} else {
    df3_An03_01_Age_Comp_ByTrt = data.frame(AnalysisId = 'An03_01_Age_Comp_ByTrt',
               MethodId = 'Mth04_ContVar_Comp_Anova',
               OutputId = 'Out14-1-1')
}
    df3_An03_01_Age_Comp_ByTrt <- df3_An03_01_Age_Comp_ByTrt |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# Analysis An03_02_AgeGrp_Summ_ByTrt----
#Summary of Subjects by Treatment and Age Group
#Apply Data Subset ---
df2_An03_02_AgeGrp_Summ_ByTrt <- df_pop

#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth01_CatVar_Summ_ByGrp
# Method name:            Summary by group of a categorical variable
# Method description:     Descriptive summary statistics across groups for a categorical variable, based on subject occurrence

if(nrow(df2_An03_02_AgeGrp_Summ_ByTrt) != 0) {
                              denom_dataset = df2_An01_05_SAF_Summ_ByTrt |>
  dplyr::select(TRT01A)

in_data = df2_An03_02_AgeGrp_Summ_ByTrt |>
    dplyr::distinct(TRT01A, AGEGR1, USUBJID) |>
    dplyr::mutate(dummy = 'dummyvar')

dataDriven = FALSE
if(dataDriven == TRUE){
df3_An03_02_AgeGrp_Summ_ByTrt <-
  cards::ard_tabulate(
    data = in_data,
    strata = c('TRT01A', 'AGEGR1'),
    variables = 'dummy',
    denominator = denom_dataset
  ) } else {
df3_An03_02_AgeGrp_Summ_ByTrt <-
 cards::ard_tabulate(
    data = in_data,
    by = c('TRT01A', 'AGEGR1'),
    variables = 'dummy',
    denominator = denom_dataset
  ) }
df3_An03_02_AgeGrp_Summ_ByTrt <- df3_An03_02_AgeGrp_Summ_ByTrt|>
dplyr::filter(stat_name %in% c('n', 'p')) |>
dplyr::mutate(operationid = dplyr::case_when(stat_name == 'n' ~ 'Mth01_CatVar_Summ_ByGrp_1_n',
                                                              stat_name == 'p' ~ 'Mth01_CatVar_Summ_ByGrp_2_pct'))}
if(nrow(df2_An03_02_AgeGrp_Summ_ByTrt) != 0){
df3_An03_02_AgeGrp_Summ_ByTrt <- df3_An03_02_AgeGrp_Summ_ByTrt |>
        dplyr::mutate(AnalysisId = 'An03_02_AgeGrp_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OutputId = 'Out14-1-1')
} else {
    df3_An03_02_AgeGrp_Summ_ByTrt = data.frame(AnalysisId = 'An03_02_AgeGrp_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OutputId = 'Out14-1-1')
}
    if(nrow(df2_An03_02_AgeGrp_Summ_ByTrt) != 0){
df3_An03_02_AgeGrp_Summ_ByTrt <- df3_An03_02_AgeGrp_Summ_ByTrt |>
  dplyr::mutate(
      group1_groupingId = 'AnlsGrouping_01_Trt',
      group1_groupId = dplyr::case_when(
        as.character(group1_level) == 'Placebo' ~ 'AnlsGrouping_01_Trt_1',
        as.character(group1_level) == 'Xanomeline Low Dose' ~ 'AnlsGrouping_01_Trt_2',
        as.character(group1_level) == 'Xanomeline High Dose' ~ 'AnlsGrouping_01_Trt_3',
        TRUE ~ NA_character_
      ),
      group2_groupingId = 'AnlsGrouping_03_AgeGp',
      group2_groupId = dplyr::case_when(
        as.character(group2_level) == '<65' ~ 'AnlsGrouping_03_AgeGp_1',
        as.character(group2_level) == '65-80 | >80' ~ 'AnlsGrouping_03_AgeGp_2',
        TRUE ~ NA_character_
      )
  )
}
df3_An03_02_AgeGrp_Summ_ByTrt <- df3_An03_02_AgeGrp_Summ_ByTrt |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# Analysis An03_02_AgeGrp_Comp_ByTrt----
#Comparison of Age Group by Treatment
#Apply Data Subset ---
df2_An03_02_AgeGrp_Comp_ByTrt <- df_pop

#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth03_CatVar_Comp_PChiSq
# Method name:            Pearson's chi-square test group comparison for a categorical variable
# Method description:     Comparison of groups by Pearson's chi-square test for a categorical variable

if(nrow(df2_An03_02_AgeGrp_Comp_ByTrt) != 0) {
                              df3_An03_02_AgeGrp_Comp_ByTrt <- 
    cardx::ard_stats_chisq_test(by = TRT01A, data = df2_An03_02_AgeGrp_Comp_ByTrt, variables = AGEGR1)|>
dplyr::filter(stat_name == 'p.value') |>
dplyr::mutate(operationid = 'Mth03_CatVar_Comp_PChiSq_1_pval')}
if(nrow(df2_An03_02_AgeGrp_Comp_ByTrt) != 0){
df3_An03_02_AgeGrp_Comp_ByTrt <- df3_An03_02_AgeGrp_Comp_ByTrt |>
        dplyr::mutate(AnalysisId = 'An03_02_AgeGrp_Comp_ByTrt',
               MethodId = 'Mth03_CatVar_Comp_PChiSq',
               OutputId = 'Out14-1-1')
} else {
    df3_An03_02_AgeGrp_Comp_ByTrt = data.frame(AnalysisId = 'An03_02_AgeGrp_Comp_ByTrt',
               MethodId = 'Mth03_CatVar_Comp_PChiSq',
               OutputId = 'Out14-1-1')
}
    df3_An03_02_AgeGrp_Comp_ByTrt <- df3_An03_02_AgeGrp_Comp_ByTrt |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# Analysis An03_03_Sex_Summ_ByTrt----
#Summary of Subjects by Treatment and Sex
#Apply Data Subset ---
df2_An03_03_Sex_Summ_ByTrt <- df_pop

#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth01_CatVar_Summ_ByGrp
# Method name:            Summary by group of a categorical variable
# Method description:     Descriptive summary statistics across groups for a categorical variable, based on subject occurrence

if(nrow(df2_An03_03_Sex_Summ_ByTrt) != 0) {
                              denom_dataset = df2_An01_05_SAF_Summ_ByTrt |>
  dplyr::select(TRT01A)

in_data = df2_An03_03_Sex_Summ_ByTrt |>
    dplyr::distinct(TRT01A, SEX, USUBJID) |>
    dplyr::mutate(dummy = 'dummyvar')

dataDriven = FALSE
if(dataDriven == TRUE){
df3_An03_03_Sex_Summ_ByTrt <-
  cards::ard_tabulate(
    data = in_data,
    strata = c('TRT01A', 'SEX'),
    variables = 'dummy',
    denominator = denom_dataset
  ) } else {
df3_An03_03_Sex_Summ_ByTrt <-
 cards::ard_tabulate(
    data = in_data,
    by = c('TRT01A', 'SEX'),
    variables = 'dummy',
    denominator = denom_dataset
  ) }
df3_An03_03_Sex_Summ_ByTrt <- df3_An03_03_Sex_Summ_ByTrt|>
dplyr::filter(stat_name %in% c('n', 'p')) |>
dplyr::mutate(operationid = dplyr::case_when(stat_name == 'n' ~ 'Mth01_CatVar_Summ_ByGrp_1_n',
                                                              stat_name == 'p' ~ 'Mth01_CatVar_Summ_ByGrp_2_pct'))}
if(nrow(df2_An03_03_Sex_Summ_ByTrt) != 0){
df3_An03_03_Sex_Summ_ByTrt <- df3_An03_03_Sex_Summ_ByTrt |>
        dplyr::mutate(AnalysisId = 'An03_03_Sex_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OutputId = 'Out14-1-1')
} else {
    df3_An03_03_Sex_Summ_ByTrt = data.frame(AnalysisId = 'An03_03_Sex_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OutputId = 'Out14-1-1')
}
    if(nrow(df2_An03_03_Sex_Summ_ByTrt) != 0){
df3_An03_03_Sex_Summ_ByTrt <- df3_An03_03_Sex_Summ_ByTrt |>
  dplyr::mutate(
      group1_groupingId = 'AnlsGrouping_01_Trt',
      group1_groupId = dplyr::case_when(
        as.character(group1_level) == 'Placebo' ~ 'AnlsGrouping_01_Trt_1',
        as.character(group1_level) == 'Xanomeline Low Dose' ~ 'AnlsGrouping_01_Trt_2',
        as.character(group1_level) == 'Xanomeline High Dose' ~ 'AnlsGrouping_01_Trt_3',
        TRUE ~ NA_character_
      ),
      group2_groupingId = 'AnlsGrouping_02_Sex',
      group2_groupId = dplyr::case_when(
        as.character(group2_level) == 'M' ~ 'AnlsGrouping_02_Sex_1',
        as.character(group2_level) == 'F' ~ 'AnlsGrouping_02_Sex_2',
        TRUE ~ NA_character_
      )
  )
}
df3_An03_03_Sex_Summ_ByTrt <- df3_An03_03_Sex_Summ_ByTrt |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# Analysis An03_03_Sex_Comp_ByTrt----
#Comparison of Sex by Treatment
#Apply Data Subset ---
df2_An03_03_Sex_Comp_ByTrt <- df_pop

#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth03_CatVar_Comp_PChiSq
# Method name:            Pearson's chi-square test group comparison for a categorical variable
# Method description:     Comparison of groups by Pearson's chi-square test for a categorical variable

if(nrow(df2_An03_03_Sex_Comp_ByTrt) != 0) {
                              df3_An03_03_Sex_Comp_ByTrt <- 
    cardx::ard_stats_chisq_test(by = TRT01A, data = df2_An03_03_Sex_Comp_ByTrt, variables = SEX)|>
dplyr::filter(stat_name == 'p.value') |>
dplyr::mutate(operationid = 'Mth03_CatVar_Comp_PChiSq_1_pval')}
if(nrow(df2_An03_03_Sex_Comp_ByTrt) != 0){
df3_An03_03_Sex_Comp_ByTrt <- df3_An03_03_Sex_Comp_ByTrt |>
        dplyr::mutate(AnalysisId = 'An03_03_Sex_Comp_ByTrt',
               MethodId = 'Mth03_CatVar_Comp_PChiSq',
               OutputId = 'Out14-1-1')
} else {
    df3_An03_03_Sex_Comp_ByTrt = data.frame(AnalysisId = 'An03_03_Sex_Comp_ByTrt',
               MethodId = 'Mth03_CatVar_Comp_PChiSq',
               OutputId = 'Out14-1-1')
}
    df3_An03_03_Sex_Comp_ByTrt <- df3_An03_03_Sex_Comp_ByTrt |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# Analysis An03_04_Ethnic_Summ_ByTrt----
#Summary of Subjects by Treatment and Ethnicity
#Apply Data Subset ---
df2_An03_04_Ethnic_Summ_ByTrt <- df_pop

#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth01_CatVar_Summ_ByGrp
# Method name:            Summary by group of a categorical variable
# Method description:     Descriptive summary statistics across groups for a categorical variable, based on subject occurrence

if(nrow(df2_An03_04_Ethnic_Summ_ByTrt) != 0) {
                              denom_dataset = df2_An01_05_SAF_Summ_ByTrt |>
  dplyr::select(TRT01A)

in_data = df2_An03_04_Ethnic_Summ_ByTrt |>
    dplyr::distinct(TRT01A, ETHNIC, USUBJID) |>
    dplyr::mutate(dummy = 'dummyvar')

dataDriven = FALSE
if(dataDriven == TRUE){
df3_An03_04_Ethnic_Summ_ByTrt <-
  cards::ard_tabulate(
    data = in_data,
    strata = c('TRT01A', 'ETHNIC'),
    variables = 'dummy',
    denominator = denom_dataset
  ) } else {
df3_An03_04_Ethnic_Summ_ByTrt <-
 cards::ard_tabulate(
    data = in_data,
    by = c('TRT01A', 'ETHNIC'),
    variables = 'dummy',
    denominator = denom_dataset
  ) }
df3_An03_04_Ethnic_Summ_ByTrt <- df3_An03_04_Ethnic_Summ_ByTrt|>
dplyr::filter(stat_name %in% c('n', 'p')) |>
dplyr::mutate(operationid = dplyr::case_when(stat_name == 'n' ~ 'Mth01_CatVar_Summ_ByGrp_1_n',
                                                              stat_name == 'p' ~ 'Mth01_CatVar_Summ_ByGrp_2_pct'))}
if(nrow(df2_An03_04_Ethnic_Summ_ByTrt) != 0){
df3_An03_04_Ethnic_Summ_ByTrt <- df3_An03_04_Ethnic_Summ_ByTrt |>
        dplyr::mutate(AnalysisId = 'An03_04_Ethnic_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OutputId = 'Out14-1-1')
} else {
    df3_An03_04_Ethnic_Summ_ByTrt = data.frame(AnalysisId = 'An03_04_Ethnic_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OutputId = 'Out14-1-1')
}
    if(nrow(df2_An03_04_Ethnic_Summ_ByTrt) != 0){
df3_An03_04_Ethnic_Summ_ByTrt <- df3_An03_04_Ethnic_Summ_ByTrt |>
  dplyr::mutate(
      group1_groupingId = 'AnlsGrouping_01_Trt',
      group1_groupId = dplyr::case_when(
        as.character(group1_level) == 'Placebo' ~ 'AnlsGrouping_01_Trt_1',
        as.character(group1_level) == 'Xanomeline Low Dose' ~ 'AnlsGrouping_01_Trt_2',
        as.character(group1_level) == 'Xanomeline High Dose' ~ 'AnlsGrouping_01_Trt_3',
        TRUE ~ NA_character_
      ),
      group2_groupingId = 'AnlsGrouping_05_Ethnic',
      group2_groupId = dplyr::case_when(
        as.character(group2_level) == 'HISPANIC OR LATINO' ~ 'AnlsGrouping_05_Ethnic_1',
        as.character(group2_level) == 'NOT HISPANIC OR LATINO' ~ 'AnlsGrouping_05_Ethnic_2',
        TRUE ~ NA_character_
      )
  )
}
df3_An03_04_Ethnic_Summ_ByTrt <- df3_An03_04_Ethnic_Summ_ByTrt |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# Analysis An03_04_Ethnic_Comp_ByTrt----
#Comparison of Ethnicity by Treatment
#Apply Data Subset ---
df2_An03_04_Ethnic_Comp_ByTrt <- df_pop

#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth03_CatVar_Comp_PChiSq
# Method name:            Pearson's chi-square test group comparison for a categorical variable
# Method description:     Comparison of groups by Pearson's chi-square test for a categorical variable

if(nrow(df2_An03_04_Ethnic_Comp_ByTrt) != 0) {
                              df3_An03_04_Ethnic_Comp_ByTrt <- 
    cardx::ard_stats_chisq_test(by = TRT01A, data = df2_An03_04_Ethnic_Comp_ByTrt, variables = ETHNIC)|>
dplyr::filter(stat_name == 'p.value') |>
dplyr::mutate(operationid = 'Mth03_CatVar_Comp_PChiSq_1_pval')}
if(nrow(df2_An03_04_Ethnic_Comp_ByTrt) != 0){
df3_An03_04_Ethnic_Comp_ByTrt <- df3_An03_04_Ethnic_Comp_ByTrt |>
        dplyr::mutate(AnalysisId = 'An03_04_Ethnic_Comp_ByTrt',
               MethodId = 'Mth03_CatVar_Comp_PChiSq',
               OutputId = 'Out14-1-1')
} else {
    df3_An03_04_Ethnic_Comp_ByTrt = data.frame(AnalysisId = 'An03_04_Ethnic_Comp_ByTrt',
               MethodId = 'Mth03_CatVar_Comp_PChiSq',
               OutputId = 'Out14-1-1')
}
    df3_An03_04_Ethnic_Comp_ByTrt <- df3_An03_04_Ethnic_Comp_ByTrt |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# Analysis An03_05_Race_Summ_ByTrt----
#Summary of Subjects by Treatment and Race
#Apply Data Subset ---
df2_An03_05_Race_Summ_ByTrt <- df_pop

#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth01_CatVar_Summ_ByGrp
# Method name:            Summary by group of a categorical variable
# Method description:     Descriptive summary statistics across groups for a categorical variable, based on subject occurrence

if(nrow(df2_An03_05_Race_Summ_ByTrt) != 0) {
                              denom_dataset = df2_An01_05_SAF_Summ_ByTrt |>
  dplyr::select(TRT01A)

in_data = df2_An03_05_Race_Summ_ByTrt |>
    dplyr::distinct(TRT01A, RACE, USUBJID) |>
    dplyr::mutate(dummy = 'dummyvar')

dataDriven = FALSE
if(dataDriven == TRUE){
df3_An03_05_Race_Summ_ByTrt <-
  cards::ard_tabulate(
    data = in_data,
    strata = c('TRT01A', 'RACE'),
    variables = 'dummy',
    denominator = denom_dataset
  ) } else {
df3_An03_05_Race_Summ_ByTrt <-
 cards::ard_tabulate(
    data = in_data,
    by = c('TRT01A', 'RACE'),
    variables = 'dummy',
    denominator = denom_dataset
  ) }
df3_An03_05_Race_Summ_ByTrt <- df3_An03_05_Race_Summ_ByTrt|>
dplyr::filter(stat_name %in% c('n', 'p')) |>
dplyr::mutate(operationid = dplyr::case_when(stat_name == 'n' ~ 'Mth01_CatVar_Summ_ByGrp_1_n',
                                                              stat_name == 'p' ~ 'Mth01_CatVar_Summ_ByGrp_2_pct'))}
if(nrow(df2_An03_05_Race_Summ_ByTrt) != 0){
df3_An03_05_Race_Summ_ByTrt <- df3_An03_05_Race_Summ_ByTrt |>
        dplyr::mutate(AnalysisId = 'An03_05_Race_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OutputId = 'Out14-1-1')
} else {
    df3_An03_05_Race_Summ_ByTrt = data.frame(AnalysisId = 'An03_05_Race_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OutputId = 'Out14-1-1')
}
    if(nrow(df2_An03_05_Race_Summ_ByTrt) != 0){
df3_An03_05_Race_Summ_ByTrt <- df3_An03_05_Race_Summ_ByTrt |>
  dplyr::mutate(
      group1_groupingId = 'AnlsGrouping_01_Trt',
      group1_groupId = dplyr::case_when(
        as.character(group1_level) == 'Placebo' ~ 'AnlsGrouping_01_Trt_1',
        as.character(group1_level) == 'Xanomeline Low Dose' ~ 'AnlsGrouping_01_Trt_2',
        as.character(group1_level) == 'Xanomeline High Dose' ~ 'AnlsGrouping_01_Trt_3',
        TRUE ~ NA_character_
      ),
      group2_groupingId = 'AnlsGrouping_04_Race',
      group2_groupId = dplyr::case_when(
        as.character(group2_level) == 'AMERICAN INDIAN OR ALASKA NATIVE' ~ 'AnlsGrouping_04_Race_1',
        as.character(group2_level) == 'ASIAN' ~ 'AnlsGrouping_04_Race_2',
        as.character(group2_level) == 'BLACK OR AFRICAN AMERICAN' ~ 'AnlsGrouping_04_Race_3',
        as.character(group2_level) == 'NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER' ~ 'AnlsGrouping_04_Race_4',
        as.character(group2_level) == 'WHITE' ~ 'AnlsGrouping_04_Race_5',
        as.character(group2_level) == 'MULTIPLE' ~ 'AnlsGrouping_04_Race_6',
        as.character(group2_level) == 'NOT REPORTED' ~ 'AnlsGrouping_04_Race_7',
        as.character(group2_level) == 'UNKNOWN' ~ 'AnlsGrouping_04_Race_8',
        as.character(group2_level) == 'OTHER' ~ 'AnlsGrouping_04_Race_9',
        TRUE ~ NA_character_
      )
  )
}
df3_An03_05_Race_Summ_ByTrt <- df3_An03_05_Race_Summ_ByTrt |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# Analysis An03_05_Race_Comp_ByTrt----
#Comparison of Race by Treatment
#Apply Data Subset ---
df2_An03_05_Race_Comp_ByTrt <- df_pop

#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth03_CatVar_Comp_PChiSq
# Method name:            Pearson's chi-square test group comparison for a categorical variable
# Method description:     Comparison of groups by Pearson's chi-square test for a categorical variable

if(nrow(df2_An03_05_Race_Comp_ByTrt) != 0) {
                              df3_An03_05_Race_Comp_ByTrt <- 
    cardx::ard_stats_chisq_test(by = TRT01A, data = df2_An03_05_Race_Comp_ByTrt, variables = RACE)|>
dplyr::filter(stat_name == 'p.value') |>
dplyr::mutate(operationid = 'Mth03_CatVar_Comp_PChiSq_1_pval')}
if(nrow(df2_An03_05_Race_Comp_ByTrt) != 0){
df3_An03_05_Race_Comp_ByTrt <- df3_An03_05_Race_Comp_ByTrt |>
        dplyr::mutate(AnalysisId = 'An03_05_Race_Comp_ByTrt',
               MethodId = 'Mth03_CatVar_Comp_PChiSq',
               OutputId = 'Out14-1-1')
} else {
    df3_An03_05_Race_Comp_ByTrt = data.frame(AnalysisId = 'An03_05_Race_Comp_ByTrt',
               MethodId = 'Mth03_CatVar_Comp_PChiSq',
               OutputId = 'Out14-1-1')
}
    df3_An03_05_Race_Comp_ByTrt <- df3_An03_05_Race_Comp_ByTrt |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# Analysis An03_06_Height_Summ_ByTrt----
#Summary of Height by Treatment
#Apply Data Subset ---
df2_An03_06_Height_Summ_ByTrt <- df_pop

#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth02_ContVar_Summ_ByGrp
# Method name:            Summary by group of a continuous variable
# Method description:     Descriptive summary statistics across groups for a continuous variable

if(nrow(df2_An03_06_Height_Summ_ByTrt) != 0) {
                              df3_An03_06_Height_Summ_ByTrt <-
  cards::ard_summary(
    data = df2_An03_06_Height_Summ_ByTrt,
    by = c('TRT01A'),
    variables = HEIGHTBL
  ) |>
dplyr::mutate(operationid = dplyr::case_when(stat_name == 'N' ~ 'Mth02_ContVar_Summ_ByGrp_1_n',
                                                                     stat_name == 'mean' ~ 'Mth02_ContVar_Summ_ByGrp_2_Mean',
                                                                     stat_name == 'sd' ~ 'Mth02_ContVar_Summ_ByGrp_3_SD',
                                                                     stat_name == 'median' ~ 'Mth02_ContVar_Summ_ByGrp_4_Median',
                                                                     stat_name == 'p25' ~ 'Mth02_ContVar_Summ_ByGrp_5_Q1',
                                                                     stat_name == 'p75' ~ 'Mth02_ContVar_Summ_ByGrp_6_Q3',
                                                                     stat_name == 'min' ~ 'Mth02_ContVar_Summ_ByGrp_7_Min',
                                                                     stat_name == 'max' ~ 'Mth02_ContVar_Summ_ByGrp_8_Max'))}
if(nrow(df2_An03_06_Height_Summ_ByTrt) != 0){
df3_An03_06_Height_Summ_ByTrt <- df3_An03_06_Height_Summ_ByTrt |>
        dplyr::mutate(AnalysisId = 'An03_06_Height_Summ_ByTrt',
               MethodId = 'Mth02_ContVar_Summ_ByGrp',
               OutputId = 'Out14-1-1')
} else {
    df3_An03_06_Height_Summ_ByTrt = data.frame(AnalysisId = 'An03_06_Height_Summ_ByTrt',
               MethodId = 'Mth02_ContVar_Summ_ByGrp',
               OutputId = 'Out14-1-1')
}
    if(nrow(df2_An03_06_Height_Summ_ByTrt) != 0){
df3_An03_06_Height_Summ_ByTrt <- df3_An03_06_Height_Summ_ByTrt |>
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
df3_An03_06_Height_Summ_ByTrt <- df3_An03_06_Height_Summ_ByTrt |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# Analysis An03_06_Height_Comp_ByTrt----
#Comparison of Height by Treatment
#Apply Data Subset ---
df2_An03_06_Height_Comp_ByTrt <- df_pop

#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth04_ContVar_Comp_Anova
# Method name:            Analysis of variance group comparison for a continuous variable
# Method description:     Comparison of groups by analysis of variance (ANOVA) for a continuous variable

if(nrow(df2_An03_06_Height_Comp_ByTrt) != 0) {
                              df3_An03_06_Height_Comp_ByTrt <- 
    cardx::ard_stats_aov(HEIGHTBL ~ TRT01A, data = df2_An03_06_Height_Comp_ByTrt) |>
dplyr::filter(stat_name == 'p.value') |>
dplyr::mutate(operationid = 'Mth04_ContVar_Comp_Anova_1_pval')}
if(nrow(df2_An03_06_Height_Comp_ByTrt) != 0){
df3_An03_06_Height_Comp_ByTrt <- df3_An03_06_Height_Comp_ByTrt |>
        dplyr::mutate(AnalysisId = 'An03_06_Height_Comp_ByTrt',
               MethodId = 'Mth04_ContVar_Comp_Anova',
               OutputId = 'Out14-1-1')
} else {
    df3_An03_06_Height_Comp_ByTrt = data.frame(AnalysisId = 'An03_06_Height_Comp_ByTrt',
               MethodId = 'Mth04_ContVar_Comp_Anova',
               OutputId = 'Out14-1-1')
}
    df3_An03_06_Height_Comp_ByTrt <- df3_An03_06_Height_Comp_ByTrt |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# combine analyses to create ARD ----
ARD <- dplyr::bind_rows(df3_An01_05_SAF_Summ_ByTrt, 
df3_An03_01_Age_Summ_ByTrt, 
df3_An03_01_Age_Comp_ByTrt, 
df3_An03_02_AgeGrp_Summ_ByTrt, 
df3_An03_02_AgeGrp_Comp_ByTrt, 
df3_An03_03_Sex_Summ_ByTrt, 
df3_An03_03_Sex_Comp_ByTrt, 
df3_An03_04_Ethnic_Summ_ByTrt, 
df3_An03_04_Ethnic_Comp_ByTrt, 
df3_An03_05_Race_Summ_ByTrt, 
df3_An03_05_Race_Comp_ByTrt, 
df3_An03_06_Height_Summ_ByTrt, 
df3_An03_06_Height_Comp_ByTrt) 

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
    num <- formatC(round(value, dec), format = "f", digits = dec)
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

