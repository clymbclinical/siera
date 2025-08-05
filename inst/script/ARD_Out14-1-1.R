
# Programme:    Generate code to produce ARD for Out14-1-1
# Output:       Summary of Demographics
# Date created: 2025-07-09 12:02:16



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
ADSL <- read_csv(ARS_example("ADSL.csv"))

# Analysis An01_05_SAF_Summ_ByTrt----
# Apply Analysis Set ---
# Analysis set :  Safety Population
df_An01_05_SAF_Summ_ByTrt <- dplyr::filter(ADSL,
            SAFFL == 'Y')

#Apply Data Subset ---
df2_An01_05_SAF_Summ_ByTrt <- df_An01_05_SAF_Summ_ByTrt

#Apply Methods ---

# Method ID:              Mth01_CatVar_Count_ByGrp
# Method name:            Count by group for a categorical variable
# Method description:     Count across groups for a categorical variable, based on subject occurrence

if(nrow(df2_An01_05_SAF_Summ_ByTrt) != 0) {
                              df3_An01_05_SAF_Summ_ByTrt <-

  ard_categorical(

    data = df2_An01_05_SAF_Summ_ByTrt

    , variables = 'TRT01A'

  ) |>

filter(stat_name == "n")  |>

mutate(operationid = "Mth01_CatVar_Count_ByGrp_1_n")}
if(nrow(df2_An01_05_SAF_Summ_ByTrt) != 0){
df3_An01_05_SAF_Summ_ByTrt <- df3_An01_05_SAF_Summ_ByTrt %>%
        dplyr::mutate(AnalsysisId = 'An01_05_SAF_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Count_ByGrp',
               OutputId = 'Out14-1-1')
} else {
    df3_An01_05_SAF_Summ_ByTrt = data.frame(AnalsysisId = 'An01_05_SAF_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Count_ByGrp',
               OutputId = 'Out14-1-1')
}


# Analysis An03_01_Age_Summ_ByTrt----
# Apply Analysis Set ---
# Analysis set :  Safety Population
df_An03_01_Age_Summ_ByTrt <- dplyr::filter(ADSL,
            SAFFL == 'Y')



#Apply Data Subset ---
df2_An03_01_Age_Summ_ByTrt <- df_An03_01_Age_Summ_ByTrt

#Apply Methods ---

# Method ID:              Mth02_ContVar_Summ_ByGrp
# Method name:            Summary by group of a continuous variable
# Method description:     Descriptive summary statistics across groups for a continuous variable

if(nrow(df2_An03_01_Age_Summ_ByTrt) != 0) {
                              df3_An03_01_Age_Summ_ByTrt <-

  ard_continuous(

    data = df2_An03_01_Age_Summ_ByTrt,

    by = c('TRT01A'),

    variables = AGE

  ) |>

mutate(operationid = case_when(stat_name == "N" ~ "Mth02_ContVar_Summ_ByGrp_1_n",

                                                                     stat_name == "mean" ~ "Mth02_ContVar_Summ_ByGrp_2_Mean",

                                                                     stat_name == "sd" ~ "Mth02_ContVar_Summ_ByGrp_3_SD",

                                                                     stat_name == "median" ~ "Mth02_ContVar_Summ_ByGrp_4_Median",

                                                                     stat_name == "p25" ~ "Mth02_ContVar_Summ_ByGrp_5_Q1",

                                                                     stat_name == "p75" ~ "Mth02_ContVar_Summ_ByGrp_6_Q3",

                                                                     stat_name == "min" ~ "Mth02_ContVar_Summ_ByGrp_7_Min",

                                                                     stat_name == "max" ~ "Mth02_ContVar_Summ_ByGrp_8_Max"))}
if(nrow(df2_An03_01_Age_Summ_ByTrt) != 0){
df3_An03_01_Age_Summ_ByTrt <- df3_An03_01_Age_Summ_ByTrt %>%
        dplyr::mutate(AnalsysisId = 'An03_01_Age_Summ_ByTrt',
               MethodId = 'Mth02_ContVar_Summ_ByGrp',
               OutputId = 'Out14-1-1')
} else {
    df3_An03_01_Age_Summ_ByTrt = data.frame(AnalsysisId = 'An03_01_Age_Summ_ByTrt',
               MethodId = 'Mth02_ContVar_Summ_ByGrp',
               OutputId = 'Out14-1-1')
}


# Analysis An03_01_Age_Comp_ByTrt----
# Apply Analysis Set ---
# Analysis set :  Safety Population
df_An03_01_Age_Comp_ByTrt <- dplyr::filter(ADSL,
            SAFFL == 'Y')



#Apply Data Subset ---
df2_An03_01_Age_Comp_ByTrt <- df_An03_01_Age_Comp_ByTrt

#Apply Methods ---

# Method ID:              Mth04_ContVar_Comp_Anova
# Method name:            Analysis of variance group comparison for a continuous variable
# Method description:     Comparison of groups by analysis of variance (ANOVA) for a continuous variable

if(nrow(df2_An03_01_Age_Comp_ByTrt) != 0) {
                              df3_An03_01_Age_Comp_ByTrt <-

    ard_stats_aov(AGE ~ TRT01A, data = df2_An03_01_Age_Comp_ByTrt) |>

filter(stat_name == "p.value") |>

mutate(operationid = "Mth04_ContVar_Comp_Anova_1_pval")}
if(nrow(df2_An03_01_Age_Comp_ByTrt) != 0){
df3_An03_01_Age_Comp_ByTrt <- df3_An03_01_Age_Comp_ByTrt %>%
        dplyr::mutate(AnalsysisId = 'An03_01_Age_Comp_ByTrt',
               MethodId = 'Mth04_ContVar_Comp_Anova',
               OutputId = 'Out14-1-1')
} else {
    df3_An03_01_Age_Comp_ByTrt = data.frame(AnalsysisId = 'An03_01_Age_Comp_ByTrt',
               MethodId = 'Mth04_ContVar_Comp_Anova',
               OutputId = 'Out14-1-1')
}


# Analysis An03_02_AgeGrp_Summ_ByTrt----
# Apply Analysis Set ---
# Analysis set :  Safety Population
df_An03_02_AgeGrp_Summ_ByTrt <- dplyr::filter(ADSL,
            SAFFL == 'Y')



#Apply Data Subset ---
df2_An03_02_AgeGrp_Summ_ByTrt <- df_An03_02_AgeGrp_Summ_ByTrt

#Apply Methods ---

# Method ID:              Mth01_CatVar_Summ_ByGrp
# Method name:            Summary by group of a categorical variable
# Method description:     Descriptive summary statistics across groups for a categorical variable, based on subject occurrence

if(nrow(df2_An03_02_AgeGrp_Summ_ByTrt) != 0) {
                              denom_dataset = df2_An01_05_SAF_Summ_ByTrt %>%

  select(TRT01A)

in_data = df2_An03_02_AgeGrp_Summ_ByTrt %>%

    distinct(TRT01A, AGEGR1, USUBJID) %>%

    mutate(dummy = "dummyvar")



dataDriven = FALSE

if(dataDriven == TRUE){

df3_An03_02_AgeGrp_Summ_ByTrt <-

  ard_categorical(

    data = in_data,

    strata = c('TRT01A', 'AGEGR1'),

    variables = 'dummy',

    denominator = denom_dataset

  ) } else {

df3_An03_02_AgeGrp_Summ_ByTrt <-

  ard_categorical(

    data = in_data,

    by = c('TRT01A', 'AGEGR1'),

    variables = 'dummy',

    denominator = denom_dataset

  ) }

df3_An03_02_AgeGrp_Summ_ByTrt <- df3_An03_02_AgeGrp_Summ_ByTrt|>

filter(stat_name %in% c("n", "p")) |>

mutate(operationid = case_when(stat_name == "n" ~ "Mth01_CatVar_Summ_ByGrp_1_n",

                                                              stat_name == "p" ~ "Mth01_CatVar_Summ_ByGrp_2_pct"))}
if(nrow(df2_An03_02_AgeGrp_Summ_ByTrt) != 0){
df3_An03_02_AgeGrp_Summ_ByTrt <- df3_An03_02_AgeGrp_Summ_ByTrt %>%
        dplyr::mutate(AnalsysisId = 'An03_02_AgeGrp_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OutputId = 'Out14-1-1')
} else {
    df3_An03_02_AgeGrp_Summ_ByTrt = data.frame(AnalsysisId = 'An03_02_AgeGrp_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OutputId = 'Out14-1-1')
}


# Analysis An03_02_AgeGrp_Comp_ByTrt----
# Apply Analysis Set ---
# Analysis set :  Safety Population
df_An03_02_AgeGrp_Comp_ByTrt <- dplyr::filter(ADSL,
            SAFFL == 'Y')



#Apply Data Subset ---
df2_An03_02_AgeGrp_Comp_ByTrt <- df_An03_02_AgeGrp_Comp_ByTrt

#Apply Methods ---

# Method ID:              Mth03_CatVar_Comp_PChiSq
# Method name:            Pearson's chi-square test group comparison for a categorical variable
# Method description:     Comparison of groups by Pearson's chi-square test for a categorical variable

if(nrow(df2_An03_02_AgeGrp_Comp_ByTrt) != 0) {
                              df3_An03_02_AgeGrp_Comp_ByTrt <-

    ard_stats_chisq_test(by = TRT01A, data = df2_An03_02_AgeGrp_Comp_ByTrt, variables = AGEGR1)|>

filter(stat_name == "p.value") |>

mutate(operationid = "Mth03_CatVar_Comp_PChiSq_1_pval")}
if(nrow(df2_An03_02_AgeGrp_Comp_ByTrt) != 0){
df3_An03_02_AgeGrp_Comp_ByTrt <- df3_An03_02_AgeGrp_Comp_ByTrt %>%
        dplyr::mutate(AnalsysisId = 'An03_02_AgeGrp_Comp_ByTrt',
               MethodId = 'Mth03_CatVar_Comp_PChiSq',
               OutputId = 'Out14-1-1')
} else {
    df3_An03_02_AgeGrp_Comp_ByTrt = data.frame(AnalsysisId = 'An03_02_AgeGrp_Comp_ByTrt',
               MethodId = 'Mth03_CatVar_Comp_PChiSq',
               OutputId = 'Out14-1-1')
}


# Analysis An03_03_Sex_Summ_ByTrt----
# Apply Analysis Set ---
# Analysis set :  Safety Population
df_An03_03_Sex_Summ_ByTrt <- dplyr::filter(ADSL,
            SAFFL == 'Y')



#Apply Data Subset ---
df2_An03_03_Sex_Summ_ByTrt <- df_An03_03_Sex_Summ_ByTrt

#Apply Methods ---

# Method ID:              Mth01_CatVar_Summ_ByGrp
# Method name:            Summary by group of a categorical variable
# Method description:     Descriptive summary statistics across groups for a categorical variable, based on subject occurrence

if(nrow(df2_An03_03_Sex_Summ_ByTrt) != 0) {
                              denom_dataset = df2_An01_05_SAF_Summ_ByTrt %>%

  select(TRT01A)

in_data = df2_An03_03_Sex_Summ_ByTrt %>%

    distinct(TRT01A, SEX, USUBJID) %>%

    mutate(dummy = "dummyvar")



dataDriven = FALSE

if(dataDriven == TRUE){

df3_An03_03_Sex_Summ_ByTrt <-

  ard_categorical(

    data = in_data,

    strata = c('TRT01A', 'SEX'),

    variables = 'dummy',

    denominator = denom_dataset

  ) } else {

df3_An03_03_Sex_Summ_ByTrt <-

  ard_categorical(

    data = in_data,

    by = c('TRT01A', 'SEX'),

    variables = 'dummy',

    denominator = denom_dataset

  ) }

df3_An03_03_Sex_Summ_ByTrt <- df3_An03_03_Sex_Summ_ByTrt|>

filter(stat_name %in% c("n", "p")) |>

mutate(operationid = case_when(stat_name == "n" ~ "Mth01_CatVar_Summ_ByGrp_1_n",

                                                              stat_name == "p" ~ "Mth01_CatVar_Summ_ByGrp_2_pct"))}
if(nrow(df2_An03_03_Sex_Summ_ByTrt) != 0){
df3_An03_03_Sex_Summ_ByTrt <- df3_An03_03_Sex_Summ_ByTrt %>%
        dplyr::mutate(AnalsysisId = 'An03_03_Sex_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OutputId = 'Out14-1-1')
} else {
    df3_An03_03_Sex_Summ_ByTrt = data.frame(AnalsysisId = 'An03_03_Sex_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OutputId = 'Out14-1-1')
}


# Analysis An03_03_Sex_Comp_ByTrt----
# Apply Analysis Set ---
# Analysis set :  Safety Population
df_An03_03_Sex_Comp_ByTrt <- dplyr::filter(ADSL,
            SAFFL == 'Y')



#Apply Data Subset ---
df2_An03_03_Sex_Comp_ByTrt <- df_An03_03_Sex_Comp_ByTrt

#Apply Methods ---

# Method ID:              Mth03_CatVar_Comp_PChiSq
# Method name:            Pearson's chi-square test group comparison for a categorical variable
# Method description:     Comparison of groups by Pearson's chi-square test for a categorical variable

if(nrow(df2_An03_03_Sex_Comp_ByTrt) != 0) {
                              df3_An03_03_Sex_Comp_ByTrt <-

    ard_stats_chisq_test(by = TRT01A, data = df2_An03_03_Sex_Comp_ByTrt, variables = SEX)|>

filter(stat_name == "p.value") |>

mutate(operationid = "Mth03_CatVar_Comp_PChiSq_1_pval")}
if(nrow(df2_An03_03_Sex_Comp_ByTrt) != 0){
df3_An03_03_Sex_Comp_ByTrt <- df3_An03_03_Sex_Comp_ByTrt %>%
        dplyr::mutate(AnalsysisId = 'An03_03_Sex_Comp_ByTrt',
               MethodId = 'Mth03_CatVar_Comp_PChiSq',
               OutputId = 'Out14-1-1')
} else {
    df3_An03_03_Sex_Comp_ByTrt = data.frame(AnalsysisId = 'An03_03_Sex_Comp_ByTrt',
               MethodId = 'Mth03_CatVar_Comp_PChiSq',
               OutputId = 'Out14-1-1')
}


# Analysis An03_04_Ethnic_Summ_ByTrt----
# Apply Analysis Set ---
# Analysis set :  Safety Population
df_An03_04_Ethnic_Summ_ByTrt <- dplyr::filter(ADSL,
            SAFFL == 'Y')



#Apply Data Subset ---
df2_An03_04_Ethnic_Summ_ByTrt <- df_An03_04_Ethnic_Summ_ByTrt

#Apply Methods ---

# Method ID:              Mth01_CatVar_Summ_ByGrp
# Method name:            Summary by group of a categorical variable
# Method description:     Descriptive summary statistics across groups for a categorical variable, based on subject occurrence

if(nrow(df2_An03_04_Ethnic_Summ_ByTrt) != 0) {
                              denom_dataset = df2_An01_05_SAF_Summ_ByTrt %>%

  select(TRT01A)

in_data = df2_An03_04_Ethnic_Summ_ByTrt %>%

    distinct(TRT01A, ETHNIC, USUBJID) %>%

    mutate(dummy = "dummyvar")



dataDriven = FALSE

if(dataDriven == TRUE){

df3_An03_04_Ethnic_Summ_ByTrt <-

  ard_categorical(

    data = in_data,

    strata = c('TRT01A', 'ETHNIC'),

    variables = 'dummy',

    denominator = denom_dataset

  ) } else {

df3_An03_04_Ethnic_Summ_ByTrt <-

  ard_categorical(

    data = in_data,

    by = c('TRT01A', 'ETHNIC'),

    variables = 'dummy',

    denominator = denom_dataset

  ) }

df3_An03_04_Ethnic_Summ_ByTrt <- df3_An03_04_Ethnic_Summ_ByTrt|>

filter(stat_name %in% c("n", "p")) |>

mutate(operationid = case_when(stat_name == "n" ~ "Mth01_CatVar_Summ_ByGrp_1_n",

                                                              stat_name == "p" ~ "Mth01_CatVar_Summ_ByGrp_2_pct"))}
if(nrow(df2_An03_04_Ethnic_Summ_ByTrt) != 0){
df3_An03_04_Ethnic_Summ_ByTrt <- df3_An03_04_Ethnic_Summ_ByTrt %>%
        dplyr::mutate(AnalsysisId = 'An03_04_Ethnic_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OutputId = 'Out14-1-1')
} else {
    df3_An03_04_Ethnic_Summ_ByTrt = data.frame(AnalsysisId = 'An03_04_Ethnic_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OutputId = 'Out14-1-1')
}


# Analysis An03_04_Ethnic_Comp_ByTrt----
# Apply Analysis Set ---
# Analysis set :  Safety Population
df_An03_04_Ethnic_Comp_ByTrt <- dplyr::filter(ADSL,
            SAFFL == 'Y')



#Apply Data Subset ---
df2_An03_04_Ethnic_Comp_ByTrt <- df_An03_04_Ethnic_Comp_ByTrt

#Apply Methods ---

# Method ID:              Mth03_CatVar_Comp_PChiSq
# Method name:            Pearson's chi-square test group comparison for a categorical variable
# Method description:     Comparison of groups by Pearson's chi-square test for a categorical variable

if(nrow(df2_An03_04_Ethnic_Comp_ByTrt) != 0) {
                              df3_An03_04_Ethnic_Comp_ByTrt <-

    ard_stats_chisq_test(by = TRT01A, data = df2_An03_04_Ethnic_Comp_ByTrt, variables = ETHNIC)|>

filter(stat_name == "p.value") |>

mutate(operationid = "Mth03_CatVar_Comp_PChiSq_1_pval")}
if(nrow(df2_An03_04_Ethnic_Comp_ByTrt) != 0){
df3_An03_04_Ethnic_Comp_ByTrt <- df3_An03_04_Ethnic_Comp_ByTrt %>%
        dplyr::mutate(AnalsysisId = 'An03_04_Ethnic_Comp_ByTrt',
               MethodId = 'Mth03_CatVar_Comp_PChiSq',
               OutputId = 'Out14-1-1')
} else {
    df3_An03_04_Ethnic_Comp_ByTrt = data.frame(AnalsysisId = 'An03_04_Ethnic_Comp_ByTrt',
               MethodId = 'Mth03_CatVar_Comp_PChiSq',
               OutputId = 'Out14-1-1')
}


# Analysis An03_05_Race_Summ_ByTrt----
# Apply Analysis Set ---
# Analysis set :  Safety Population
df_An03_05_Race_Summ_ByTrt <- dplyr::filter(ADSL,
            SAFFL == 'Y')



#Apply Data Subset ---
df2_An03_05_Race_Summ_ByTrt <- df_An03_05_Race_Summ_ByTrt

#Apply Methods ---

# Method ID:              Mth01_CatVar_Summ_ByGrp
# Method name:            Summary by group of a categorical variable
# Method description:     Descriptive summary statistics across groups for a categorical variable, based on subject occurrence

if(nrow(df2_An03_05_Race_Summ_ByTrt) != 0) {
                              denom_dataset = df2_An01_05_SAF_Summ_ByTrt %>%

  select(TRT01A)

in_data = df2_An03_05_Race_Summ_ByTrt %>%

    distinct(TRT01A, RACE, USUBJID) %>%

    mutate(dummy = "dummyvar")



dataDriven = FALSE

if(dataDriven == TRUE){

df3_An03_05_Race_Summ_ByTrt <-

  ard_categorical(

    data = in_data,

    strata = c('TRT01A', 'RACE'),

    variables = 'dummy',

    denominator = denom_dataset

  ) } else {

df3_An03_05_Race_Summ_ByTrt <-

  ard_categorical(

    data = in_data,

    by = c('TRT01A', 'RACE'),

    variables = 'dummy',

    denominator = denom_dataset

  ) }

df3_An03_05_Race_Summ_ByTrt <- df3_An03_05_Race_Summ_ByTrt|>

filter(stat_name %in% c("n", "p")) |>

mutate(operationid = case_when(stat_name == "n" ~ "Mth01_CatVar_Summ_ByGrp_1_n",

                                                              stat_name == "p" ~ "Mth01_CatVar_Summ_ByGrp_2_pct"))}
if(nrow(df2_An03_05_Race_Summ_ByTrt) != 0){
df3_An03_05_Race_Summ_ByTrt <- df3_An03_05_Race_Summ_ByTrt %>%
        dplyr::mutate(AnalsysisId = 'An03_05_Race_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OutputId = 'Out14-1-1')
} else {
    df3_An03_05_Race_Summ_ByTrt = data.frame(AnalsysisId = 'An03_05_Race_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Summ_ByGrp',
               OutputId = 'Out14-1-1')
}


# Analysis An03_05_Race_Comp_ByTrt----
# Apply Analysis Set ---
# Analysis set :  Safety Population
df_An03_05_Race_Comp_ByTrt <- dplyr::filter(ADSL,
            SAFFL == 'Y')



#Apply Data Subset ---
df2_An03_05_Race_Comp_ByTrt <- df_An03_05_Race_Comp_ByTrt

#Apply Methods ---

# Method ID:              Mth03_CatVar_Comp_PChiSq
# Method name:            Pearson's chi-square test group comparison for a categorical variable
# Method description:     Comparison of groups by Pearson's chi-square test for a categorical variable

if(nrow(df2_An03_05_Race_Comp_ByTrt) != 0) {
                              df3_An03_05_Race_Comp_ByTrt <-

    ard_stats_chisq_test(by = TRT01A, data = df2_An03_05_Race_Comp_ByTrt, variables = RACE)|>

filter(stat_name == "p.value") |>

mutate(operationid = "Mth03_CatVar_Comp_PChiSq_1_pval")}
if(nrow(df2_An03_05_Race_Comp_ByTrt) != 0){
df3_An03_05_Race_Comp_ByTrt <- df3_An03_05_Race_Comp_ByTrt %>%
        dplyr::mutate(AnalsysisId = 'An03_05_Race_Comp_ByTrt',
               MethodId = 'Mth03_CatVar_Comp_PChiSq',
               OutputId = 'Out14-1-1')
} else {
    df3_An03_05_Race_Comp_ByTrt = data.frame(AnalsysisId = 'An03_05_Race_Comp_ByTrt',
               MethodId = 'Mth03_CatVar_Comp_PChiSq',
               OutputId = 'Out14-1-1')
}


# Analysis An03_06_Height_Summ_ByTrt----
# Apply Analysis Set ---
# Analysis set :  Safety Population
df_An03_06_Height_Summ_ByTrt <- dplyr::filter(ADSL,
            SAFFL == 'Y')



#Apply Data Subset ---
df2_An03_06_Height_Summ_ByTrt <- df_An03_06_Height_Summ_ByTrt

#Apply Methods ---

# Method ID:              Mth02_ContVar_Summ_ByGrp
# Method name:            Summary by group of a continuous variable
# Method description:     Descriptive summary statistics across groups for a continuous variable

if(nrow(df2_An03_06_Height_Summ_ByTrt) != 0) {
                              df3_An03_06_Height_Summ_ByTrt <-

  ard_continuous(

    data = df2_An03_06_Height_Summ_ByTrt,

    by = c('TRT01A'),

    variables = HEIGHTBL

  ) |>

mutate(operationid = case_when(stat_name == "N" ~ "Mth02_ContVar_Summ_ByGrp_1_n",

                                                                     stat_name == "mean" ~ "Mth02_ContVar_Summ_ByGrp_2_Mean",

                                                                     stat_name == "sd" ~ "Mth02_ContVar_Summ_ByGrp_3_SD",

                                                                     stat_name == "median" ~ "Mth02_ContVar_Summ_ByGrp_4_Median",

                                                                     stat_name == "p25" ~ "Mth02_ContVar_Summ_ByGrp_5_Q1",

                                                                     stat_name == "p75" ~ "Mth02_ContVar_Summ_ByGrp_6_Q3",

                                                                     stat_name == "min" ~ "Mth02_ContVar_Summ_ByGrp_7_Min",

                                                                     stat_name == "max" ~ "Mth02_ContVar_Summ_ByGrp_8_Max"))}
if(nrow(df2_An03_06_Height_Summ_ByTrt) != 0){
df3_An03_06_Height_Summ_ByTrt <- df3_An03_06_Height_Summ_ByTrt %>%
        dplyr::mutate(AnalsysisId = 'An03_06_Height_Summ_ByTrt',
               MethodId = 'Mth02_ContVar_Summ_ByGrp',
               OutputId = 'Out14-1-1')
} else {
    df3_An03_06_Height_Summ_ByTrt = data.frame(AnalsysisId = 'An03_06_Height_Summ_ByTrt',
               MethodId = 'Mth02_ContVar_Summ_ByGrp',
               OutputId = 'Out14-1-1')
}


# Analysis An03_06_Height_Comp_ByTrt----
# Apply Analysis Set ---
# Analysis set :  Safety Population
df_An03_06_Height_Comp_ByTrt <- dplyr::filter(ADSL,
            SAFFL == 'Y')



#Apply Data Subset ---
df2_An03_06_Height_Comp_ByTrt <- df_An03_06_Height_Comp_ByTrt

#Apply Methods ---

# Method ID:              Mth04_ContVar_Comp_Anova
# Method name:            Analysis of variance group comparison for a continuous variable
# Method description:     Comparison of groups by analysis of variance (ANOVA) for a continuous variable

if(nrow(df2_An03_06_Height_Comp_ByTrt) != 0) {
                              df3_An03_06_Height_Comp_ByTrt <-

    ard_stats_aov(HEIGHTBL ~ TRT01A, data = df2_An03_06_Height_Comp_ByTrt) |>

filter(stat_name == "p.value") |>

mutate(operationid = "Mth04_ContVar_Comp_Anova_1_pval")}
if(nrow(df2_An03_06_Height_Comp_ByTrt) != 0){
df3_An03_06_Height_Comp_ByTrt <- df3_An03_06_Height_Comp_ByTrt %>%
        dplyr::mutate(AnalsysisId = 'An03_06_Height_Comp_ByTrt',
               MethodId = 'Mth04_ContVar_Comp_Anova',
               OutputId = 'Out14-1-1')
} else {
    df3_An03_06_Height_Comp_ByTrt = data.frame(AnalsysisId = 'An03_06_Height_Comp_ByTrt',
               MethodId = 'Mth04_ContVar_Comp_Anova',
               OutputId = 'Out14-1-1')
}


# combine analyses to create ARD ----
df4 <- dplyr::bind_rows(df3_An01_05_SAF_Summ_ByTrt,
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

 #Apply pattern format:

