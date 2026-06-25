.arms_analysisidhere <- utils::head(sort(unique(df2_analysisidhere$groupvar1here)), 2)
full_analysisidhere <- df_poptot |> dplyr::filter(groupvar1here %in% .arms_analysisidhere)

success_analysisidhere = df2_analysisidhere |>
    dplyr::distinct(anavarhere) |>
    dplyr::mutate(FL = 1L)

ana_analysisidhere = full_analysisidhere |>
    dplyr::left_join(dplyr::select(success_analysisidhere, anavarhere, FL), by = "anavarhere") |>
    dplyr::mutate(FL = dplyr::if_else(is.na(FL), 0L, FL))

df3_analysisidhere = if (length(.arms_analysisidhere) >= 2) {
  cardx::ard_stats_fisher_test(data = ana_analysisidhere, by = groupvar1here, variables = FL) |>
   dplyr::filter(stat_name %in% c('estimate', 'conf.low', 'conf.high')) |>
   dplyr::mutate(operationid = dplyr::case_when(stat_name == 'estimate'  ~ 'opid1here',
                                                stat_name == 'conf.low'  ~ 'opid2here',
                                                stat_name == 'conf.high' ~ 'opid3here'))
} else {
  tibble::tibble(variable = character(0), stat_name = character(0),
                 stat = list(), operationid = character(0))
}
