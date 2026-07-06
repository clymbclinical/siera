.arms_analysisidhere <- utils::head(sort(unique(df2_analysisidhere$groupvar1here)), 2)
full_analysisidhere <- df_poptot |> dplyr::filter(groupvar1here %in% .arms_analysisidhere)

.pairs_analysisidhere <- df2_analysisidhere |>
  dplyr::distinct(groupvar2here, groupvar3here) |>
  dplyr::arrange(groupvar2here, groupvar3here)

.rd_one_analysisidhere <- function(.g2, .g3) {
  success <- df2_analysisidhere |>
      dplyr::filter(as.character(groupvar2here) == .g2,
                    as.character(groupvar3here) == .g3) |>
      dplyr::distinct(anavarhere) |>
      dplyr::mutate(FL = 1L)
  ana <- full_analysisidhere |>
      dplyr::left_join(dplyr::select(success, anavarhere, FL), by = "anavarhere") |>
      dplyr::mutate(FL = dplyr::if_else(is.na(FL), 0L, FL))
  cardx::ard_stats_prop_test(data = ana, by = groupvar1here, variables = FL, correct = FALSE) |>
      dplyr::filter(stat_name %in% c("estimate", "conf.low", "conf.high")) |>
      dplyr::mutate(
          stat = lapply(stat, function(.v) .v * 100),
          group2_level = .g2,
          group3_level = .g3,
          operationid = dplyr::case_when(
              stat_name == "estimate"  ~ "opid1here",
              stat_name == "conf.low"  ~ "opid2here",
              stat_name == "conf.high" ~ "opid3here"
          )
      )
}

df3_analysisidhere <- if (length(.arms_analysisidhere) >= 2 && nrow(.pairs_analysisidhere) > 0) {
  dplyr::bind_rows(Map(
      .rd_one_analysisidhere,
      as.character(.pairs_analysisidhere$groupvar2here),
      as.character(.pairs_analysisidhere$groupvar3here)
  ))
} else {
  tibble::tibble(variable = character(0), group2_level = character(0),
                 group3_level = character(0), stat_name = character(0),
                 stat = list(), operationid = character(0))
}
