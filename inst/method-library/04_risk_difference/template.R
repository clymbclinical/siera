.arms_analysisidhere <- unique(df2_analysisidhere$groupvar1here)
full_analysisidhere <- if (length(.arms_analysisidhere) >= 2) {
  df_poptot |> dplyr::filter(groupvar1here %in% .arms_analysisidhere)
} else {
  df_poptot
}

success_analysisidhere = df2_analysisidhere |>
    dplyr::distinct(anavarhere) |>
    dplyr::mutate(FL = 1L)

ana_analysisidhere = full_analysisidhere |>
    dplyr::left_join(
        success_analysisidhere |> dplyr::select(anavarhere, FL),
        by = "anavarhere"
    ) |>
    dplyr::mutate(FL = dplyr::if_else(is.na(FL), 0L, FL))

grp_analysisidhere <- sort(unique(ana_analysisidhere$groupvar1here))

if (length(grp_analysisidhere) >= 2) {
n1_analysisidhere <- sum(ana_analysisidhere$groupvar1here == grp_analysisidhere[[1]])
n2_analysisidhere <- sum(ana_analysisidhere$groupvar1here == grp_analysisidhere[[2]])
x1_analysisidhere <- sum(ana_analysisidhere$FL[ana_analysisidhere$groupvar1here == grp_analysisidhere[[1]]])
x2_analysisidhere <- sum(ana_analysisidhere$FL[ana_analysisidhere$groupvar1here == grp_analysisidhere[[2]]])
pt_analysisidhere <- stats::prop.test(
    x = c(x1_analysisidhere, x2_analysisidhere),
    n = c(n1_analysisidhere, n2_analysisidhere),
    correct = FALSE
)

df3_analysisidhere <- tibble::tibble(
    variable = "FL",
    variable_level = list(NULL, NULL, NULL),
    stat_name = c("estimate", "conf.low", "conf.high"),
    stat = list(
        (pt_analysisidhere$estimate[[1]] - pt_analysisidhere$estimate[[2]]) * 100,
        pt_analysisidhere$conf.int[[1]] * 100,
        pt_analysisidhere$conf.int[[2]] * 100
    ),
    operationid = c('opid1here', 'opid2here', 'opid3here')
)
} else {
df3_analysisidhere <- tibble::tibble(
    variable = "FL",
    variable_level = list(NULL, NULL, NULL),
    stat_name = c("estimate", "conf.low", "conf.high"),
    stat = list(NA_real_, NA_real_, NA_real_),
    operationid = c('opid1here', 'opid2here', 'opid3here')
)
}
