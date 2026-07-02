df3_analysisidhere <-
    cardx::ard_stats_chisq_test(by = groupvar1here, data = df2_analysisidhere, variables = groupvar2here) |>
  dplyr::filter(stat_name == 'p.value') |>
  dplyr::mutate(operationid = 'opid1here')
