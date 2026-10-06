# Test the groups the ARS metadata DEFINES for the inner grouping (matched by
# their conditions), not the raw data values: a multi-value IN group is one
# category, and a value satisfying no defined group was never asked for, so
# it is excluded. Defined groups without any subject drop out, as a
# chi-square over an all-zero column is undefined.
in_data_analysisidhere <- df2_analysisidhere |>
    dplyr::mutate(ars_group = dplyr::case_when(
      group2conditionshere,
      TRUE ~ NA_character_
    )) |>
    dplyr::filter(!is.na(ars_group))

df3_analysisidhere <-
    cardx::ard_stats_chisq_test(by = groupvar1here, data = in_data_analysisidhere, variables = ars_group) |>
  dplyr::filter(stat_name == 'p.value') |>
  dplyr::mutate(operationid = 'opid1here')
