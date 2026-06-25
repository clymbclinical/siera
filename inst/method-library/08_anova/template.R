df3_analysisidhere <-
    cardx::ard_stats_aov(anavarhere ~ groupvar1here, data = df2_analysisidhere) |>
  dplyr::filter(stat_name == 'p.value') |>
  dplyr::mutate(operationid = 'opid1here')
