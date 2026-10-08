
# Programme:    Generate code to produce ARD for Out14-1-1
# Output:       Summary of Demographics
# Date created: 2026-10-08 15:52:59

  # load libraries ----
    library(dplyr)
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

# Method ID:              Mth01_CatVar_Count_ByGrp
# Method name:            Count by group for a categorical variable
# Method description:     Count across groups for a categorical variable, based on subject occurrence

df3_An01_05_SAF_Summ_ByTrt <- NULL
if (nrow(df2_An01_05_SAF_Summ_ByTrt) != 0) {
in_data = df2_An01_05_SAF_Summ_ByTrt |>
    dplyr::select(USUBJID, TRT01A) |>
    unique()
df3_An01_05_SAF_Summ_ByTrt <-
  cards::ard_tabulate(
    data = in_data
    , variables = 'TRT01A'
  ) |>
dplyr::filter(stat_name == 'n') |>
 dplyr::mutate(operationid = 'Mth01_CatVar_Count_ByGrp_1_n')
}

# Link ARS identifiers ---
df3_An01_05_SAF_Summ_ByTrt <- siera::ars_stamp(
  df3_An01_05_SAF_Summ_ByTrt,
  analysis_id = 'An01_05_SAF_Summ_ByTrt',   # analyses[].id
  method_id   = 'Mth01_CatVar_Count_ByGrp', # analyses[].methodId
  output_id   = 'Out14-1-1'                 # mainListOfContents outputId
)


# Analysis An03_01_Age_Summ_ByTrt----
#Summary of Age by Treatment
#Apply Data Subset ---
df2_An03_01_Age_Summ_ByTrt <- df_poptot

#Apply Method --- 

# Method ID:              Mth02_ContVar_Summ_ByGrp
# Method name:            Summary by group of a continuous variable
# Method description:     Descriptive summary statistics across groups for a continuous variable

df3_An03_01_Age_Summ_ByTrt <- NULL
if (nrow(df2_An03_01_Age_Summ_ByTrt) != 0) {
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
                                                                     stat_name == 'max' ~ 'Mth02_ContVar_Summ_ByGrp_8_Max'))
}

# Link ARS identifiers ---
df3_An03_01_Age_Summ_ByTrt <- siera::ars_stamp(
  df3_An03_01_Age_Summ_ByTrt,
  analysis_id = 'An03_01_Age_Summ_ByTrt',   # analyses[].id
  method_id   = 'Mth02_ContVar_Summ_ByGrp', # analyses[].methodId
  output_id   = 'Out14-1-1',                # mainListOfContents outputId
  groupings   = list(                       # analyses[].orderedGroupings
    siera::ars_grouping('AnlsGrouping_01_Trt', groups = c(
      'AnlsGrouping_01_Trt_1' = 'Placebo',
      'AnlsGrouping_01_Trt_2' = 'Xanomeline Low Dose',
      'AnlsGrouping_01_Trt_3' = 'Xanomeline High Dose'))
  )
)


# Analysis An03_01_Age_Comp_ByTrt----
#Comparison of Age by Treatment
#Apply Data Subset ---
df2_An03_01_Age_Comp_ByTrt <- df_poptot

#Apply Method --- 

# Method ID:              Mth04_ContVar_Comp_Anova
# Method name:            Analysis of variance group comparison for a continuous variable
# Method description:     Comparison of groups by analysis of variance (ANOVA) for a continuous variable

df3_An03_01_Age_Comp_ByTrt <- NULL
if (nrow(df2_An03_01_Age_Comp_ByTrt) != 0) {
df3_An03_01_Age_Comp_ByTrt <- 
    cardx::ard_stats_aov(AGE ~ TRT01A, data = df2_An03_01_Age_Comp_ByTrt) |>
dplyr::filter(stat_name == 'p.value') |>
dplyr::mutate(operationid = 'Mth04_ContVar_Comp_Anova_1_pval')
}

# Link ARS identifiers ---
df3_An03_01_Age_Comp_ByTrt <- siera::ars_stamp(
  df3_An03_01_Age_Comp_ByTrt,
  analysis_id = 'An03_01_Age_Comp_ByTrt',   # analyses[].id
  method_id   = 'Mth04_ContVar_Comp_Anova', # analyses[].methodId
  output_id   = 'Out14-1-1'                 # mainListOfContents outputId
)


# Analysis An03_02_AgeGrp_Summ_ByTrt----
#Summary of Subjects by Treatment and Age Group
#Apply Data Subset ---
df2_An03_02_AgeGrp_Summ_ByTrt <- df_poptot

#Apply Method --- 

# Method ID:              Mth01_CatVar_Summ_ByPreGrp
# Method name:            Summary by pre-defined group of a categorical variable
# Method description:     Descriptive summary statistics across groups for a categorical variable, based on subject occurrence, counted per pre-defined group condition of the inner grouping: every defined group is reported, including empty ones.

# Population-based (see df_poptot): the zero-fill below must also run when the
# data subset is empty, so siera drops its empty-data guard for this method -
# an empty subset yields n = 0 / 0.0% for every arm x group.
# Denominator: the referenced analysis's population count per Group1 level.
denom_An03_02_AgeGrp_Summ_ByTrt <- df2_An01_05_SAF_Summ_ByTrt |>
    dplyr::count(TRT01A) |>
    dplyr::rename(`...ard_N...` = n) |>
    dplyr::mutate(dplyr::across(-`...ard_N...`, as.character))

# Group1's levels come from the denominator, so an arm in which no subject
# qualifies still gets a zero row; Group2's levels are the ones the ARS
# metadata DEFINES, not the ones the data happens to contain.
.arms_An03_02_AgeGrp_Summ_ByTrt <- as.character(denom_An03_02_AgeGrp_Summ_ByTrt$TRT01A)
.levels_An03_02_AgeGrp_Summ_ByTrt <- c('< 65 years', '\u2265 65 years')

denom_An03_02_AgeGrp_Summ_ByTrt <- denom_An03_02_AgeGrp_Summ_ByTrt |>
    dplyr::mutate(TRT01A = factor(TRT01A, levels = .arms_An03_02_AgeGrp_Summ_ByTrt))

in_data_An03_02_AgeGrp_Summ_ByTrt <- df2_An03_02_AgeGrp_Summ_ByTrt |>
    dplyr::mutate(ars_group = dplyr::case_when(
      AGEGR1 == '<65' ~ '< 65 years',
      AGEGR1 %in% c('65-80', '>80') ~ '\u2265 65 years',
      TRUE ~ NA_character_
    )) |>
    # A data value satisfying none of the defined group conditions was never
    # asked for by the ARS, so it is excluded rather than tabulated.
    dplyr::filter(!is.na(ars_group)) |>
    dplyr::distinct(TRT01A, ars_group, USUBJID) |>
    dplyr::mutate(
      TRT01A = factor(as.character(TRT01A), levels = .arms_An03_02_AgeGrp_Summ_ByTrt),
      ars_group     = factor(ars_group, levels = .levels_An03_02_AgeGrp_Summ_ByTrt)
    ) |>
    dplyr::select(TRT01A, ars_group)

df3_An03_02_AgeGrp_Summ_ByTrt <- if (length(.levels_An03_02_AgeGrp_Summ_ByTrt) > 0) {
  cards::ard_tabulate(
      data = in_data_An03_02_AgeGrp_Summ_ByTrt,
      by = 'TRT01A',
      variables = 'ars_group',
      denominator = denom_An03_02_AgeGrp_Summ_ByTrt
    ) |>
  dplyr::filter(stat_name %in% c('n', 'p')) |>
  dplyr::rename(group2_level = variable_level) |>
  # cards returns the levels as list-columns of factors, whose as.character()
  # is the integer code - flatten to the labels before siera stamps group ids.
  dplyr::mutate(dplyr::across(
      dplyr::matches('_level$'),
      ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
    )) |>
  dplyr::mutate(operationid = dplyr::case_when(
      stat_name == 'n' ~ 'Mth01_CatVar_Summ_ByPreGrp_1_n',
      stat_name == 'p' ~ 'Mth01_CatVar_Summ_ByPreGrp_2_pct'
    ))
} else {
  tibble::tibble(group1_level = character(0), group2_level = character(0),
                 stat_name = character(0), stat = list(),
                 operationid = character(0))
}

# Link ARS identifiers ---
df3_An03_02_AgeGrp_Summ_ByTrt <- siera::ars_stamp(
  df3_An03_02_AgeGrp_Summ_ByTrt,
  analysis_id = 'An03_02_AgeGrp_Summ_ByTrt',  # analyses[].id
  method_id   = 'Mth01_CatVar_Summ_ByPreGrp', # analyses[].methodId
  output_id   = 'Out14-1-1',                  # mainListOfContents outputId
  groupings   = list(                         # analyses[].orderedGroupings
    siera::ars_grouping('AnlsGrouping_01_Trt', groups = c(
      'AnlsGrouping_01_Trt_1' = 'Placebo',
      'AnlsGrouping_01_Trt_2' = 'Xanomeline Low Dose',
      'AnlsGrouping_01_Trt_3' = 'Xanomeline High Dose')),
    siera::ars_grouping('AnlsGrouping_03_AgeGp', groups = c(
      'AnlsGrouping_03_AgeGp_1' = '< 65 years',
      'AnlsGrouping_03_AgeGp_2' = '\u2265 65 years'))
  )
)


# Analysis An03_02_AgeGrp_Comp_ByTrt----
#Comparison of Age Group by Treatment
#Apply Data Subset ---
df2_An03_02_AgeGrp_Comp_ByTrt <- df_poptot

#Apply Method --- 

# Method ID:              Mth03_CatVar_Comp_PChiSq
# Method name:            Pearson's chi-square test group comparison for a categorical variable
# Method description:     Comparison of groups by Pearson's chi-square test for a categorical variable

df3_An03_02_AgeGrp_Comp_ByTrt <- NULL
if (nrow(df2_An03_02_AgeGrp_Comp_ByTrt) != 0) {
# Test the groups the ARS metadata DEFINES for the inner grouping (matched by
# their conditions), not the raw data values: a multi-value IN group is one
# category, and a value satisfying no defined group was never asked for, so
# it is excluded. Defined groups without any subject drop out, as a
# chi-square over an all-zero column is undefined.
in_data_An03_02_AgeGrp_Comp_ByTrt <- df2_An03_02_AgeGrp_Comp_ByTrt |>
    dplyr::mutate(ars_group = dplyr::case_when(
      AGEGR1 == '<65' ~ '< 65 years',
      AGEGR1 %in% c('65-80', '>80') ~ '\u2265 65 years',
      TRUE ~ NA_character_
    )) |>
    dplyr::filter(!is.na(ars_group))

df3_An03_02_AgeGrp_Comp_ByTrt <-
    cardx::ard_stats_chisq_test(by = TRT01A, data = in_data_An03_02_AgeGrp_Comp_ByTrt, variables = ars_group) |>
  dplyr::filter(stat_name == 'p.value') |>
  dplyr::mutate(operationid = 'Mth03_CatVar_Comp_PChiSq_1_pval')
}

# Link ARS identifiers ---
df3_An03_02_AgeGrp_Comp_ByTrt <- siera::ars_stamp(
  df3_An03_02_AgeGrp_Comp_ByTrt,
  analysis_id = 'An03_02_AgeGrp_Comp_ByTrt', # analyses[].id
  method_id   = 'Mth03_CatVar_Comp_PChiSq',  # analyses[].methodId
  output_id   = 'Out14-1-1'                  # mainListOfContents outputId
)


# Analysis An03_03_Sex_Summ_ByTrt----
#Summary of Subjects by Treatment and Sex
#Apply Data Subset ---
df2_An03_03_Sex_Summ_ByTrt <- df_poptot

#Apply Method --- 

# Method ID:              Mth01_CatVar_Summ_ByPreGrp
# Method name:            Summary by pre-defined group of a categorical variable
# Method description:     Descriptive summary statistics across groups for a categorical variable, based on subject occurrence, counted per pre-defined group condition of the inner grouping: every defined group is reported, including empty ones.

# Population-based (see df_poptot): the zero-fill below must also run when the
# data subset is empty, so siera drops its empty-data guard for this method -
# an empty subset yields n = 0 / 0.0% for every arm x group.
# Denominator: the referenced analysis's population count per Group1 level.
denom_An03_03_Sex_Summ_ByTrt <- df2_An01_05_SAF_Summ_ByTrt |>
    dplyr::count(TRT01A) |>
    dplyr::rename(`...ard_N...` = n) |>
    dplyr::mutate(dplyr::across(-`...ard_N...`, as.character))

# Group1's levels come from the denominator, so an arm in which no subject
# qualifies still gets a zero row; Group2's levels are the ones the ARS
# metadata DEFINES, not the ones the data happens to contain.
.arms_An03_03_Sex_Summ_ByTrt <- as.character(denom_An03_03_Sex_Summ_ByTrt$TRT01A)
.levels_An03_03_Sex_Summ_ByTrt <- c('Male', 'Female')

denom_An03_03_Sex_Summ_ByTrt <- denom_An03_03_Sex_Summ_ByTrt |>
    dplyr::mutate(TRT01A = factor(TRT01A, levels = .arms_An03_03_Sex_Summ_ByTrt))

in_data_An03_03_Sex_Summ_ByTrt <- df2_An03_03_Sex_Summ_ByTrt |>
    dplyr::mutate(ars_group = dplyr::case_when(
      SEX == 'M' ~ 'Male',
      SEX == 'F' ~ 'Female',
      TRUE ~ NA_character_
    )) |>
    # A data value satisfying none of the defined group conditions was never
    # asked for by the ARS, so it is excluded rather than tabulated.
    dplyr::filter(!is.na(ars_group)) |>
    dplyr::distinct(TRT01A, ars_group, USUBJID) |>
    dplyr::mutate(
      TRT01A = factor(as.character(TRT01A), levels = .arms_An03_03_Sex_Summ_ByTrt),
      ars_group     = factor(ars_group, levels = .levels_An03_03_Sex_Summ_ByTrt)
    ) |>
    dplyr::select(TRT01A, ars_group)

df3_An03_03_Sex_Summ_ByTrt <- if (length(.levels_An03_03_Sex_Summ_ByTrt) > 0) {
  cards::ard_tabulate(
      data = in_data_An03_03_Sex_Summ_ByTrt,
      by = 'TRT01A',
      variables = 'ars_group',
      denominator = denom_An03_03_Sex_Summ_ByTrt
    ) |>
  dplyr::filter(stat_name %in% c('n', 'p')) |>
  dplyr::rename(group2_level = variable_level) |>
  # cards returns the levels as list-columns of factors, whose as.character()
  # is the integer code - flatten to the labels before siera stamps group ids.
  dplyr::mutate(dplyr::across(
      dplyr::matches('_level$'),
      ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
    )) |>
  dplyr::mutate(operationid = dplyr::case_when(
      stat_name == 'n' ~ 'Mth01_CatVar_Summ_ByPreGrp_1_n',
      stat_name == 'p' ~ 'Mth01_CatVar_Summ_ByPreGrp_2_pct'
    ))
} else {
  tibble::tibble(group1_level = character(0), group2_level = character(0),
                 stat_name = character(0), stat = list(),
                 operationid = character(0))
}

# Link ARS identifiers ---
df3_An03_03_Sex_Summ_ByTrt <- siera::ars_stamp(
  df3_An03_03_Sex_Summ_ByTrt,
  analysis_id = 'An03_03_Sex_Summ_ByTrt',     # analyses[].id
  method_id   = 'Mth01_CatVar_Summ_ByPreGrp', # analyses[].methodId
  output_id   = 'Out14-1-1',                  # mainListOfContents outputId
  groupings   = list(                         # analyses[].orderedGroupings
    siera::ars_grouping('AnlsGrouping_01_Trt', groups = c(
      'AnlsGrouping_01_Trt_1' = 'Placebo',
      'AnlsGrouping_01_Trt_2' = 'Xanomeline Low Dose',
      'AnlsGrouping_01_Trt_3' = 'Xanomeline High Dose')),
    siera::ars_grouping('AnlsGrouping_02_Sex', groups = c(
      'AnlsGrouping_02_Sex_1' = 'Male',
      'AnlsGrouping_02_Sex_2' = 'Female'))
  )
)


# Analysis An03_03_Sex_Comp_ByTrt----
#Comparison of Sex by Treatment
#Apply Data Subset ---
df2_An03_03_Sex_Comp_ByTrt <- df_poptot

#Apply Method --- 

# Method ID:              Mth03_CatVar_Comp_PChiSq
# Method name:            Pearson's chi-square test group comparison for a categorical variable
# Method description:     Comparison of groups by Pearson's chi-square test for a categorical variable

df3_An03_03_Sex_Comp_ByTrt <- NULL
if (nrow(df2_An03_03_Sex_Comp_ByTrt) != 0) {
# Test the groups the ARS metadata DEFINES for the inner grouping (matched by
# their conditions), not the raw data values: a multi-value IN group is one
# category, and a value satisfying no defined group was never asked for, so
# it is excluded. Defined groups without any subject drop out, as a
# chi-square over an all-zero column is undefined.
in_data_An03_03_Sex_Comp_ByTrt <- df2_An03_03_Sex_Comp_ByTrt |>
    dplyr::mutate(ars_group = dplyr::case_when(
      SEX == 'M' ~ 'Male',
      SEX == 'F' ~ 'Female',
      TRUE ~ NA_character_
    )) |>
    dplyr::filter(!is.na(ars_group))

df3_An03_03_Sex_Comp_ByTrt <-
    cardx::ard_stats_chisq_test(by = TRT01A, data = in_data_An03_03_Sex_Comp_ByTrt, variables = ars_group) |>
  dplyr::filter(stat_name == 'p.value') |>
  dplyr::mutate(operationid = 'Mth03_CatVar_Comp_PChiSq_1_pval')
}

# Link ARS identifiers ---
df3_An03_03_Sex_Comp_ByTrt <- siera::ars_stamp(
  df3_An03_03_Sex_Comp_ByTrt,
  analysis_id = 'An03_03_Sex_Comp_ByTrt',   # analyses[].id
  method_id   = 'Mth03_CatVar_Comp_PChiSq', # analyses[].methodId
  output_id   = 'Out14-1-1'                 # mainListOfContents outputId
)


# Analysis An03_04_Ethnic_Summ_ByTrt----
#Summary of Subjects by Treatment and Ethnicity
#Apply Data Subset ---
df2_An03_04_Ethnic_Summ_ByTrt <- df_poptot

#Apply Method --- 

# Method ID:              Mth01_CatVar_Summ_ByPreGrp
# Method name:            Summary by pre-defined group of a categorical variable
# Method description:     Descriptive summary statistics across groups for a categorical variable, based on subject occurrence, counted per pre-defined group condition of the inner grouping: every defined group is reported, including empty ones.

# Population-based (see df_poptot): the zero-fill below must also run when the
# data subset is empty, so siera drops its empty-data guard for this method -
# an empty subset yields n = 0 / 0.0% for every arm x group.
# Denominator: the referenced analysis's population count per Group1 level.
denom_An03_04_Ethnic_Summ_ByTrt <- df2_An01_05_SAF_Summ_ByTrt |>
    dplyr::count(TRT01A) |>
    dplyr::rename(`...ard_N...` = n) |>
    dplyr::mutate(dplyr::across(-`...ard_N...`, as.character))

# Group1's levels come from the denominator, so an arm in which no subject
# qualifies still gets a zero row; Group2's levels are the ones the ARS
# metadata DEFINES, not the ones the data happens to contain.
.arms_An03_04_Ethnic_Summ_ByTrt <- as.character(denom_An03_04_Ethnic_Summ_ByTrt$TRT01A)
.levels_An03_04_Ethnic_Summ_ByTrt <- c('Hispanic or Latino', 'Not Hispanic or Latino')

denom_An03_04_Ethnic_Summ_ByTrt <- denom_An03_04_Ethnic_Summ_ByTrt |>
    dplyr::mutate(TRT01A = factor(TRT01A, levels = .arms_An03_04_Ethnic_Summ_ByTrt))

in_data_An03_04_Ethnic_Summ_ByTrt <- df2_An03_04_Ethnic_Summ_ByTrt |>
    dplyr::mutate(ars_group = dplyr::case_when(
      ETHNIC == 'HISPANIC OR LATINO' ~ 'Hispanic or Latino',
      ETHNIC == 'NOT HISPANIC OR LATINO' ~ 'Not Hispanic or Latino',
      TRUE ~ NA_character_
    )) |>
    # A data value satisfying none of the defined group conditions was never
    # asked for by the ARS, so it is excluded rather than tabulated.
    dplyr::filter(!is.na(ars_group)) |>
    dplyr::distinct(TRT01A, ars_group, USUBJID) |>
    dplyr::mutate(
      TRT01A = factor(as.character(TRT01A), levels = .arms_An03_04_Ethnic_Summ_ByTrt),
      ars_group     = factor(ars_group, levels = .levels_An03_04_Ethnic_Summ_ByTrt)
    ) |>
    dplyr::select(TRT01A, ars_group)

df3_An03_04_Ethnic_Summ_ByTrt <- if (length(.levels_An03_04_Ethnic_Summ_ByTrt) > 0) {
  cards::ard_tabulate(
      data = in_data_An03_04_Ethnic_Summ_ByTrt,
      by = 'TRT01A',
      variables = 'ars_group',
      denominator = denom_An03_04_Ethnic_Summ_ByTrt
    ) |>
  dplyr::filter(stat_name %in% c('n', 'p')) |>
  dplyr::rename(group2_level = variable_level) |>
  # cards returns the levels as list-columns of factors, whose as.character()
  # is the integer code - flatten to the labels before siera stamps group ids.
  dplyr::mutate(dplyr::across(
      dplyr::matches('_level$'),
      ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
    )) |>
  dplyr::mutate(operationid = dplyr::case_when(
      stat_name == 'n' ~ 'Mth01_CatVar_Summ_ByPreGrp_1_n',
      stat_name == 'p' ~ 'Mth01_CatVar_Summ_ByPreGrp_2_pct'
    ))
} else {
  tibble::tibble(group1_level = character(0), group2_level = character(0),
                 stat_name = character(0), stat = list(),
                 operationid = character(0))
}

# Link ARS identifiers ---
df3_An03_04_Ethnic_Summ_ByTrt <- siera::ars_stamp(
  df3_An03_04_Ethnic_Summ_ByTrt,
  analysis_id = 'An03_04_Ethnic_Summ_ByTrt',  # analyses[].id
  method_id   = 'Mth01_CatVar_Summ_ByPreGrp', # analyses[].methodId
  output_id   = 'Out14-1-1',                  # mainListOfContents outputId
  groupings   = list(                         # analyses[].orderedGroupings
    siera::ars_grouping('AnlsGrouping_01_Trt', groups = c(
      'AnlsGrouping_01_Trt_1' = 'Placebo',
      'AnlsGrouping_01_Trt_2' = 'Xanomeline Low Dose',
      'AnlsGrouping_01_Trt_3' = 'Xanomeline High Dose')),
    siera::ars_grouping('AnlsGrouping_05_Ethnic', groups = c(
      'AnlsGrouping_05_Ethnic_1' = 'Hispanic or Latino',
      'AnlsGrouping_05_Ethnic_2' = 'Not Hispanic or Latino'))
  )
)


# Analysis An03_04_Ethnic_Comp_ByTrt----
#Comparison of Ethnicity by Treatment
#Apply Data Subset ---
df2_An03_04_Ethnic_Comp_ByTrt <- df_poptot

#Apply Method --- 

# Method ID:              Mth03_CatVar_Comp_PChiSq
# Method name:            Pearson's chi-square test group comparison for a categorical variable
# Method description:     Comparison of groups by Pearson's chi-square test for a categorical variable

df3_An03_04_Ethnic_Comp_ByTrt <- NULL
if (nrow(df2_An03_04_Ethnic_Comp_ByTrt) != 0) {
# Test the groups the ARS metadata DEFINES for the inner grouping (matched by
# their conditions), not the raw data values: a multi-value IN group is one
# category, and a value satisfying no defined group was never asked for, so
# it is excluded. Defined groups without any subject drop out, as a
# chi-square over an all-zero column is undefined.
in_data_An03_04_Ethnic_Comp_ByTrt <- df2_An03_04_Ethnic_Comp_ByTrt |>
    dplyr::mutate(ars_group = dplyr::case_when(
      ETHNIC == 'HISPANIC OR LATINO' ~ 'Hispanic or Latino',
      ETHNIC == 'NOT HISPANIC OR LATINO' ~ 'Not Hispanic or Latino',
      TRUE ~ NA_character_
    )) |>
    dplyr::filter(!is.na(ars_group))

df3_An03_04_Ethnic_Comp_ByTrt <-
    cardx::ard_stats_chisq_test(by = TRT01A, data = in_data_An03_04_Ethnic_Comp_ByTrt, variables = ars_group) |>
  dplyr::filter(stat_name == 'p.value') |>
  dplyr::mutate(operationid = 'Mth03_CatVar_Comp_PChiSq_1_pval')
}

# Link ARS identifiers ---
df3_An03_04_Ethnic_Comp_ByTrt <- siera::ars_stamp(
  df3_An03_04_Ethnic_Comp_ByTrt,
  analysis_id = 'An03_04_Ethnic_Comp_ByTrt', # analyses[].id
  method_id   = 'Mth03_CatVar_Comp_PChiSq',  # analyses[].methodId
  output_id   = 'Out14-1-1'                  # mainListOfContents outputId
)


# Analysis An03_05_Race_Summ_ByTrt----
#Summary of Subjects by Treatment and Race
#Apply Data Subset ---
df2_An03_05_Race_Summ_ByTrt <- df_poptot

#Apply Method --- 

# Method ID:              Mth01_CatVar_Summ_ByPreGrp
# Method name:            Summary by pre-defined group of a categorical variable
# Method description:     Descriptive summary statistics across groups for a categorical variable, based on subject occurrence, counted per pre-defined group condition of the inner grouping: every defined group is reported, including empty ones.

# Population-based (see df_poptot): the zero-fill below must also run when the
# data subset is empty, so siera drops its empty-data guard for this method -
# an empty subset yields n = 0 / 0.0% for every arm x group.
# Denominator: the referenced analysis's population count per Group1 level.
denom_An03_05_Race_Summ_ByTrt <- df2_An01_05_SAF_Summ_ByTrt |>
    dplyr::count(TRT01A) |>
    dplyr::rename(`...ard_N...` = n) |>
    dplyr::mutate(dplyr::across(-`...ard_N...`, as.character))

# Group1's levels come from the denominator, so an arm in which no subject
# qualifies still gets a zero row; Group2's levels are the ones the ARS
# metadata DEFINES, not the ones the data happens to contain.
.arms_An03_05_Race_Summ_ByTrt <- as.character(denom_An03_05_Race_Summ_ByTrt$TRT01A)
.levels_An03_05_Race_Summ_ByTrt <- c('American Indian or Alaska Native', 'Asian', 'Black or African American', 'Native Hawaiian or Other Pacific Islander', 'White', 'Multiple', 'Not Reported', 'Unknown', 'Other')

denom_An03_05_Race_Summ_ByTrt <- denom_An03_05_Race_Summ_ByTrt |>
    dplyr::mutate(TRT01A = factor(TRT01A, levels = .arms_An03_05_Race_Summ_ByTrt))

in_data_An03_05_Race_Summ_ByTrt <- df2_An03_05_Race_Summ_ByTrt |>
    dplyr::mutate(ars_group = dplyr::case_when(
      RACE == 'AMERICAN INDIAN OR ALASKA NATIVE' ~ 'American Indian or Alaska Native',
      RACE == 'ASIAN' ~ 'Asian',
      RACE == 'BLACK OR AFRICAN AMERICAN' ~ 'Black or African American',
      RACE == 'NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER' ~ 'Native Hawaiian or Other Pacific Islander',
      RACE == 'WHITE' ~ 'White',
      RACE == 'MULTIPLE' ~ 'Multiple',
      RACE == 'NOT REPORTED' ~ 'Not Reported',
      RACE == 'UNKNOWN' ~ 'Unknown',
      RACE == 'OTHER' ~ 'Other',
      TRUE ~ NA_character_
    )) |>
    # A data value satisfying none of the defined group conditions was never
    # asked for by the ARS, so it is excluded rather than tabulated.
    dplyr::filter(!is.na(ars_group)) |>
    dplyr::distinct(TRT01A, ars_group, USUBJID) |>
    dplyr::mutate(
      TRT01A = factor(as.character(TRT01A), levels = .arms_An03_05_Race_Summ_ByTrt),
      ars_group     = factor(ars_group, levels = .levels_An03_05_Race_Summ_ByTrt)
    ) |>
    dplyr::select(TRT01A, ars_group)

df3_An03_05_Race_Summ_ByTrt <- if (length(.levels_An03_05_Race_Summ_ByTrt) > 0) {
  cards::ard_tabulate(
      data = in_data_An03_05_Race_Summ_ByTrt,
      by = 'TRT01A',
      variables = 'ars_group',
      denominator = denom_An03_05_Race_Summ_ByTrt
    ) |>
  dplyr::filter(stat_name %in% c('n', 'p')) |>
  dplyr::rename(group2_level = variable_level) |>
  # cards returns the levels as list-columns of factors, whose as.character()
  # is the integer code - flatten to the labels before siera stamps group ids.
  dplyr::mutate(dplyr::across(
      dplyr::matches('_level$'),
      ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
    )) |>
  dplyr::mutate(operationid = dplyr::case_when(
      stat_name == 'n' ~ 'Mth01_CatVar_Summ_ByPreGrp_1_n',
      stat_name == 'p' ~ 'Mth01_CatVar_Summ_ByPreGrp_2_pct'
    ))
} else {
  tibble::tibble(group1_level = character(0), group2_level = character(0),
                 stat_name = character(0), stat = list(),
                 operationid = character(0))
}

# Link ARS identifiers ---
df3_An03_05_Race_Summ_ByTrt <- siera::ars_stamp(
  df3_An03_05_Race_Summ_ByTrt,
  analysis_id = 'An03_05_Race_Summ_ByTrt',    # analyses[].id
  method_id   = 'Mth01_CatVar_Summ_ByPreGrp', # analyses[].methodId
  output_id   = 'Out14-1-1',                  # mainListOfContents outputId
  groupings   = list(                         # analyses[].orderedGroupings
    siera::ars_grouping('AnlsGrouping_01_Trt', groups = c(
      'AnlsGrouping_01_Trt_1' = 'Placebo',
      'AnlsGrouping_01_Trt_2' = 'Xanomeline Low Dose',
      'AnlsGrouping_01_Trt_3' = 'Xanomeline High Dose')),
    siera::ars_grouping('AnlsGrouping_04_Race', groups = c(
      'AnlsGrouping_04_Race_1' = 'American Indian or Alaska Native',
      'AnlsGrouping_04_Race_2' = 'Asian',
      'AnlsGrouping_04_Race_3' = 'Black or African American',
      'AnlsGrouping_04_Race_4' = 'Native Hawaiian or Other Pacific Islander',
      'AnlsGrouping_04_Race_5' = 'White',
      'AnlsGrouping_04_Race_6' = 'Multiple',
      'AnlsGrouping_04_Race_7' = 'Not Reported',
      'AnlsGrouping_04_Race_8' = 'Unknown',
      'AnlsGrouping_04_Race_9' = 'Other'))
  )
)


# Analysis An03_05_Race_Comp_ByTrt----
#Comparison of Race by Treatment
#Apply Data Subset ---
df2_An03_05_Race_Comp_ByTrt <- df_poptot

#Apply Method --- 

# Method ID:              Mth03_CatVar_Comp_PChiSq
# Method name:            Pearson's chi-square test group comparison for a categorical variable
# Method description:     Comparison of groups by Pearson's chi-square test for a categorical variable

df3_An03_05_Race_Comp_ByTrt <- NULL
if (nrow(df2_An03_05_Race_Comp_ByTrt) != 0) {
# Test the groups the ARS metadata DEFINES for the inner grouping (matched by
# their conditions), not the raw data values: a multi-value IN group is one
# category, and a value satisfying no defined group was never asked for, so
# it is excluded. Defined groups without any subject drop out, as a
# chi-square over an all-zero column is undefined.
in_data_An03_05_Race_Comp_ByTrt <- df2_An03_05_Race_Comp_ByTrt |>
    dplyr::mutate(ars_group = dplyr::case_when(
      RACE == 'AMERICAN INDIAN OR ALASKA NATIVE' ~ 'American Indian or Alaska Native',
      RACE == 'ASIAN' ~ 'Asian',
      RACE == 'BLACK OR AFRICAN AMERICAN' ~ 'Black or African American',
      RACE == 'NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER' ~ 'Native Hawaiian or Other Pacific Islander',
      RACE == 'WHITE' ~ 'White',
      RACE == 'MULTIPLE' ~ 'Multiple',
      RACE == 'NOT REPORTED' ~ 'Not Reported',
      RACE == 'UNKNOWN' ~ 'Unknown',
      RACE == 'OTHER' ~ 'Other',
      TRUE ~ NA_character_
    )) |>
    dplyr::filter(!is.na(ars_group))

df3_An03_05_Race_Comp_ByTrt <-
    cardx::ard_stats_chisq_test(by = TRT01A, data = in_data_An03_05_Race_Comp_ByTrt, variables = ars_group) |>
  dplyr::filter(stat_name == 'p.value') |>
  dplyr::mutate(operationid = 'Mth03_CatVar_Comp_PChiSq_1_pval')
}

# Link ARS identifiers ---
df3_An03_05_Race_Comp_ByTrt <- siera::ars_stamp(
  df3_An03_05_Race_Comp_ByTrt,
  analysis_id = 'An03_05_Race_Comp_ByTrt',  # analyses[].id
  method_id   = 'Mth03_CatVar_Comp_PChiSq', # analyses[].methodId
  output_id   = 'Out14-1-1'                 # mainListOfContents outputId
)


# Analysis An03_06_Height_Summ_ByTrt----
#Summary of Height by Treatment
#Apply Data Subset ---
df2_An03_06_Height_Summ_ByTrt <- df_poptot

#Apply Method --- 

# Method ID:              Mth02_ContVar_Summ_ByGrp
# Method name:            Summary by group of a continuous variable
# Method description:     Descriptive summary statistics across groups for a continuous variable

df3_An03_06_Height_Summ_ByTrt <- NULL
if (nrow(df2_An03_06_Height_Summ_ByTrt) != 0) {
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
                                                                     stat_name == 'max' ~ 'Mth02_ContVar_Summ_ByGrp_8_Max'))
}

# Link ARS identifiers ---
df3_An03_06_Height_Summ_ByTrt <- siera::ars_stamp(
  df3_An03_06_Height_Summ_ByTrt,
  analysis_id = 'An03_06_Height_Summ_ByTrt', # analyses[].id
  method_id   = 'Mth02_ContVar_Summ_ByGrp',  # analyses[].methodId
  output_id   = 'Out14-1-1',                 # mainListOfContents outputId
  groupings   = list(                        # analyses[].orderedGroupings
    siera::ars_grouping('AnlsGrouping_01_Trt', groups = c(
      'AnlsGrouping_01_Trt_1' = 'Placebo',
      'AnlsGrouping_01_Trt_2' = 'Xanomeline Low Dose',
      'AnlsGrouping_01_Trt_3' = 'Xanomeline High Dose'))
  )
)


# Analysis An03_06_Height_Comp_ByTrt----
#Comparison of Height by Treatment
#Apply Data Subset ---
df2_An03_06_Height_Comp_ByTrt <- df_poptot

#Apply Method --- 

# Method ID:              Mth04_ContVar_Comp_Anova
# Method name:            Analysis of variance group comparison for a continuous variable
# Method description:     Comparison of groups by analysis of variance (ANOVA) for a continuous variable

df3_An03_06_Height_Comp_ByTrt <- NULL
if (nrow(df2_An03_06_Height_Comp_ByTrt) != 0) {
df3_An03_06_Height_Comp_ByTrt <- 
    cardx::ard_stats_aov(HEIGHTBL ~ TRT01A, data = df2_An03_06_Height_Comp_ByTrt) |>
dplyr::filter(stat_name == 'p.value') |>
dplyr::mutate(operationid = 'Mth04_ContVar_Comp_Anova_1_pval')
}

# Link ARS identifiers ---
df3_An03_06_Height_Comp_ByTrt <- siera::ars_stamp(
  df3_An03_06_Height_Comp_ByTrt,
  analysis_id = 'An03_06_Height_Comp_ByTrt', # analyses[].id
  method_id   = 'Mth04_ContVar_Comp_Anova',  # analyses[].methodId
  output_id   = 'Out14-1-1'                  # mainListOfContents outputId
)


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
    "Mth01_CatVar_Summ_ByPreGrp_1_n",
    "Mth01_CatVar_Summ_ByPreGrp_2_pct",
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

