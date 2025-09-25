
# Programme:    Generate code to produce ARD for Out14-1-1
# Output:       Summary of Demographics
# Date created: 2025-09-23 17:34:43



# load libraries ----
library(dplyr)
library(readxl)
library(readr)
library(cards)
library(cardx)
library(siera)
library(parameters)
library(tidyr)
library(magrittr)

# Load ADaM -------
# ADSL <- readr::read_csv('C:/Users/mbosm/OneDrive - Clymb Clinical/Documents/siera/inst/extdata/ADSL.csv',
#                                       show_col_types = FALSE,
#                                       progress = FALSE) |>
#   dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ tidyr::replace_na(.x, '')))

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

# Method ID:              Mth01_CatVar_Count_ByGrp
# Method name:            Count by group for a categorical variable
# Method description:     Count across groups for a categorical variable, based on subject occurrence

if(nrow(df2_An01_05_SAF_Summ_ByTrt) != 0) {
                              in_data = df2_An01_05_SAF_Summ_ByTrt |>

    dplyr::select(USUBJID, TRT01A) |>

    unique()

df3_An01_05_SAF_Summ_ByTrt <-

  cards::ard_categorical(

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


# Analysis An03_01_Age_Summ_ByTrt----
#Summary of Age by Treatment

#Apply Data Subset ---
df2_An03_01_Age_Summ_ByTrt <- df_pop

#Apply Method ---

# Method ID:              Mth02_ContVar_Summ_ByGrp
# Method name:            Summary by group of a continuous variable
# Method description:     Descriptive summary statistics across groups for a continuous variable

if(nrow(df2_An03_01_Age_Summ_ByTrt) != 0) {
                              df3_An03_01_Age_Summ_ByTrt <-

  cards::ard_continuous(

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


# Analysis An03_01_Age_Comp_ByTrt----
#Comparison of Age by Treatment

#Apply Data Subset ---
df2_An03_01_Age_Comp_ByTrt <- df_pop

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


# Analysis An03_02_AgeGrp_Summ_ByTrt----
#Summary of Subjects by Treatment and Age Group

#Apply Data Subset ---
df2_An03_02_AgeGrp_Summ_ByTrt <- df_pop

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

  cards::ard_categorical(

    data = in_data,

    strata = c('TRT01A', 'AGEGR1'),

    variables = 'dummy',

    denominator = denom_dataset

  ) } else {

df3_An03_02_AgeGrp_Summ_ByTrt <-

 cards::ard_categorical(

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


# Analysis An03_02_AgeGrp_Comp_ByTrt----
#Comparison of Age Group by Treatment

#Apply Data Subset ---
df2_An03_02_AgeGrp_Comp_ByTrt <- df_pop

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


# Analysis An03_03_Sex_Summ_ByTrt----
#Summary of Subjects by Treatment and Sex

#Apply Data Subset ---
df2_An03_03_Sex_Summ_ByTrt <- df_pop

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

  cards::ard_categorical(

    data = in_data,

    strata = c('TRT01A', 'SEX'),

    variables = 'dummy',

    denominator = denom_dataset

  ) } else {

df3_An03_03_Sex_Summ_ByTrt <-

 cards::ard_categorical(

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


# Analysis An03_03_Sex_Comp_ByTrt----
#Comparison of Sex by Treatment

#Apply Data Subset ---
df2_An03_03_Sex_Comp_ByTrt <- df_pop

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


# Analysis An03_04_Ethnic_Summ_ByTrt----
#Summary of Subjects by Treatment and Ethnicity

#Apply Data Subset ---
df2_An03_04_Ethnic_Summ_ByTrt <- df_pop

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

  cards::ard_categorical(

    data = in_data,

    strata = c('TRT01A', 'ETHNIC'),

    variables = 'dummy',

    denominator = denom_dataset

  ) } else {

df3_An03_04_Ethnic_Summ_ByTrt <-

 cards::ard_categorical(

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


# Analysis An03_04_Ethnic_Comp_ByTrt----
#Comparison of Ethnicity by Treatment

#Apply Data Subset ---
df2_An03_04_Ethnic_Comp_ByTrt <- df_pop

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


# Analysis An03_05_Race_Summ_ByTrt----
#Summary of Subjects by Treatment and Race

#Apply Data Subset ---
df2_An03_05_Race_Summ_ByTrt <- df_pop

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

  cards::ard_categorical(

    data = in_data,

    strata = c('TRT01A', 'RACE'),

    variables = 'dummy',

    denominator = denom_dataset

  ) } else {

df3_An03_05_Race_Summ_ByTrt <-

 cards::ard_categorical(

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


# Analysis An03_05_Race_Comp_ByTrt----
#Comparison of Race by Treatment

#Apply Data Subset ---
df2_An03_05_Race_Comp_ByTrt <- df_pop

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


# Analysis An03_06_Height_Summ_ByTrt----
#Summary of Height by Treatment

#Apply Data Subset ---
df2_An03_06_Height_Summ_ByTrt <- df_pop

#Apply Method ---

# Method ID:              Mth02_ContVar_Summ_ByGrp
# Method name:            Summary by group of a continuous variable
# Method description:     Descriptive summary statistics across groups for a continuous variable

if(nrow(df2_An03_06_Height_Summ_ByTrt) != 0) {
                              df3_An03_06_Height_Summ_ByTrt <-

  cards::ard_continuous(

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


# Analysis An03_06_Height_Comp_ByTrt----
#Comparison of Height by Treatment

#Apply Data Subset ---
df2_An03_06_Height_Comp_ByTrt <- df_pop

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
