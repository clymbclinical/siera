df3_analysisidhere <-
  cards::ard_summary(
    data = df2_analysisidhere,
    by = c(byvarshere),
    variables = anavarhere
  ) |>
  dplyr::mutate(operationid = dplyr::case_when(stat_name == 'N'      ~ 'opid1here',
                                               stat_name == 'mean'   ~ 'opid2here',
                                               stat_name == 'sd'     ~ 'opid3here',
                                               stat_name == 'median' ~ 'opid4here',
                                               stat_name == 'p25'    ~ 'opid5here',
                                               stat_name == 'p75'    ~ 'opid6here',
                                               stat_name == 'min'    ~ 'opid7here',
                                               stat_name == 'max'    ~ 'opid8here'))
