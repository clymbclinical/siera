
# Programme:    Generate code to produce ARD for Out14-3-1-1
# Output:       Overall Summary of Treatment-Emergent Adverse Events
# Date created: 2025-09-22 14:06:38



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
               OutputId = 'Out14-3-1-1')
} else {
    df3_An01_05_SAF_Summ_ByTrt = data.frame(AnalysisId = 'An01_05_SAF_Summ_ByTrt',
               MethodId = 'Mth01_CatVar_Count_ByGrp',
               OutputId = 'Out14-3-1-1')
}


# Analysis An07_01_TEAE_Summ_ByTrt----
#Summary of Subjects with At Least One TEAE, by Treatment

# Apply Data Subset ---
# Data subset: Treatment-Emergent Adverse Events
df2_An07_01_TEAE_Summ_ByTrt <- df_pop |>
        dplyr::filter(TRTEMFL == 'Y')

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

  cards::ard_categorical(

    data = in_data,

    strata = c('TRT01A'),

    variables = 'dummy',

    denominator = denom_dataset

  ) } else {

df3_An07_01_TEAE_Summ_ByTrt <-

 cards::ard_categorical(

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


# Analysis An07_02_RelTEAE_Summ_ByTrt----
#Summary of Subjects with At Least One Related TEAE, by Treatment

# Apply Data Subset ---
# Data subset: Related Treatment-Emergent Adverse Events
df2_An07_02_RelTEAE_Summ_ByTrt <- df_pop |>
        dplyr::filter(TRTEMFL == 'Y' & AEREL %in% c('POSSIBLE | PROBABLE'))

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

  cards::ard_categorical(

    data = in_data,

    strata = c('TRT01A'),

    variables = 'dummy',

    denominator = denom_dataset

  ) } else {

df3_An07_02_RelTEAE_Summ_ByTrt <-

 cards::ard_categorical(

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


# Analysis An07_03_SerTEAE_Summ_ByTrt----
#Summary of Subjects with At Least One Serious TEAE, by Treatment

# Apply Data Subset ---
# Data subset: Serious Treatment-Emergent Adverse Events
df2_An07_03_SerTEAE_Summ_ByTrt <- df_pop |>
        dplyr::filter(TRTEMFL == 'Y' & AESER == 'Y')

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

  cards::ard_categorical(

    data = in_data,

    strata = c('TRT01A'),

    variables = 'dummy',

    denominator = denom_dataset

  ) } else {

df3_An07_03_SerTEAE_Summ_ByTrt <-

 cards::ard_categorical(

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


# Analysis An07_04_RelSerTEAE_Summ_ByTrt----
#Summary of Subjects with At Least One Related Serious TEAE, by Treatment

# Apply Data Subset ---
# Data subset: Related Serious Treatment-Emergent Adverse Events
df2_An07_04_RelSerTEAE_Summ_ByTrt <- df_pop |>
        dplyr::filter(TRTEMFL == 'Y' & AEREL %in% c('POSSIBLE | PROBABLE') & AESER == 'Y')

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

  cards::ard_categorical(

    data = in_data,

    strata = c('TRT01A'),

    variables = 'dummy',

    denominator = denom_dataset

  ) } else {

df3_An07_04_RelSerTEAE_Summ_ByTrt <-

 cards::ard_categorical(

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


# Analysis An07_05_TEAELd2Dth_Summ_ByTrt----
#Summary of Subjects with At Least One TEAE Leading to Death, by Treatment

# Apply Data Subset ---
# Data subset: Treatment-Emergent Adverse Events Leading to Death
df2_An07_05_TEAELd2Dth_Summ_ByTrt <- df_pop |>
        dplyr::filter(TRTEMFL == 'Y' & AESDTH == 'Y')

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

  cards::ard_categorical(

    data = in_data,

    strata = c('TRT01A'),

    variables = 'dummy',

    denominator = denom_dataset

  ) } else {

df3_An07_05_TEAELd2Dth_Summ_ByTrt <-

 cards::ard_categorical(

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


# Analysis An07_06_RelTEAELd2Dth_Summ_ByTrt----
#Summary of Subjects with At Least One Related TEAE Leading to Death, by Treatment

# Apply Data Subset ---
# Data subset: Related Treatment-Emergent Adverse Events Leading to Death
df2_An07_06_RelTEAELd2Dth_Summ_ByTrt <- df_pop |>
        dplyr::filter(TRTEMFL == 'Y' & AESDTH == 'Y', AEREL == 'POSSIBLE' | AEREL == 'PROBABLE')

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

  cards::ard_categorical(

    data = in_data,

    strata = c('TRT01A'),

    variables = 'dummy',

    denominator = denom_dataset

  ) } else {

df3_An07_06_RelTEAELd2Dth_Summ_ByTrt <-

 cards::ard_categorical(

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


# Analysis An07_07_TEAELd2DoseMod_Summ_ByTrt----
#Summary of Subjects with At Least One TEAE Leading to Dose Modification, by Treatment

# Apply Data Subset ---
# Data subset: Treatment-Emergent Adverse Events Leading to Dose Modification
df2_An07_07_TEAELd2DoseMod_Summ_ByTrt <- df_pop |>
        dplyr::filter(TRTEMFL == 'Y' & AEACN %in% c('DOSE REDUCED | DRUG INTERRUPTED'))

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

  cards::ard_categorical(

    data = in_data,

    strata = c('TRT01A'),

    variables = 'dummy',

    denominator = denom_dataset

  ) } else {

df3_An07_07_TEAELd2DoseMod_Summ_ByTrt <-

 cards::ard_categorical(

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


# Analysis An07_08_TEAELd2TrtDsc_Summ_ByTrt----
#Summary of Subjects with At Least One TEAE Leading to Treatment Discontinuation, by Treatment

# Apply Data Subset ---
# Data subset: Treatment-Emergent Adverse Events Leading to Treatment Discontinuation
df2_An07_08_TEAELd2TrtDsc_Summ_ByTrt <- df_pop |>
        dplyr::filter(TRTEMFL == 'Y' & AEACN == 'DRUG WITHDRAWN')

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

  cards::ard_categorical(

    data = in_data,

    strata = c('TRT01A'),

    variables = 'dummy',

    denominator = denom_dataset

  ) } else {

df3_An07_08_TEAELd2TrtDsc_Summ_ByTrt <-

 cards::ard_categorical(

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
