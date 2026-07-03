.groupvals_analysisidhere <- c(group2valueshere)
.arms_analysisidhere <- utils::head(sort(unique(df2_analysisidhere$groupvar1here)), 2)
full_analysisidhere <- df_poptot |> dplyr::filter(groupvar1here %in% .arms_analysisidhere)

.rd_one_analysisidhere <- function(.val) {
  success <- df2_analysisidhere |>
      dplyr::filter(as.character(groupvar2here) == .val) |>
      dplyr::distinct(anavarhere) |>
      dplyr::mutate(FL = 1L)
  ana <- full_analysisidhere |>
      dplyr::left_join(dplyr::select(success, anavarhere, FL), by = "anavarhere") |>
      dplyr::mutate(FL = dplyr::if_else(is.na(FL), 0L, FL))
  x1 <- sum(ana$FL[ana$groupvar1here == .arms_analysisidhere[[1]]])
  x2 <- sum(ana$FL[ana$groupvar1here == .arms_analysisidhere[[2]]])
  n1 <- sum(ana$groupvar1here == .arms_analysisidhere[[1]])
  n2 <- sum(ana$groupvar1here == .arms_analysisidhere[[2]])
  pt <- suppressWarnings(stats::prop.test(x = c(x1, x2), n = c(n1, n2), correct = FALSE))
  tibble::tibble(
      variable = "FL",
      variable_level = list(NULL, NULL, NULL),
      stat_name = c("estimate", "conf.low", "conf.high"),
      stat = list(
          (pt$estimate[[1]] - pt$estimate[[2]]) * 100,
          pt$conf.int[[1]] * 100,
          pt$conf.int[[2]] * 100
      ),
      group2_level = .val,
      operationid = c('opid1here', 'opid2here', 'opid3here')
  )
}

df3_analysisidhere <- if (length(.arms_analysisidhere) >= 2 && length(.groupvals_analysisidhere) > 0) {
  dplyr::bind_rows(lapply(.groupvals_analysisidhere, .rd_one_analysisidhere))
} else {
  tibble::tibble(variable = character(0), group2_level = character(0),
                 stat_name = character(0), stat = list(),
                 operationid = character(0))
}