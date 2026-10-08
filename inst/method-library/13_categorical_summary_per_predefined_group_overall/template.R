# Population-based (see df_poptot): the zero-fill below must also run when the
# data subset is empty, so siera drops its empty-data guard for this method -
# an empty subset yields n = 0 / 0.0% for every defined group.
# Denominator: the referenced analysis's distinct subjects, as ONE overall
# count - this analysis has no treatment grouping to split it by.
denom_analysisidhere <- tibble::tibble(
    `...ard_N...` = dplyr::n_distinct(df2_denomanaidhere$anavarhere)
)

# The levels are the ones the ARS metadata DEFINES, not the ones the data
# happens to contain.
.levels_analysisidhere <- c(group1levelshere)

in_data_analysisidhere <- df2_analysisidhere |>
    dplyr::mutate(ars_group = dplyr::case_when(
      group1conditionshere,
      TRUE ~ NA_character_
    )) |>
    # A data value satisfying none of the defined group conditions was never
    # asked for by the ARS, so it is excluded rather than tabulated.
    dplyr::filter(!is.na(ars_group)) |>
    dplyr::distinct(ars_group, anavarhere) |>
    dplyr::mutate(ars_group = factor(ars_group, levels = .levels_analysisidhere)) |>
    dplyr::select(ars_group)

df3_analysisidhere <- if (length(.levels_analysisidhere) > 0) {
  cards::ard_tabulate(
      data = in_data_analysisidhere,
      variables = 'ars_group',
      denominator = denom_analysisidhere
    ) |>
  dplyr::filter(stat_name %in% c('n', 'p')) |>
  dplyr::rename(group1_level = variable_level) |>
  # cards returns the levels as list-columns of factors, whose as.character()
  # is the integer code - flatten to the labels before siera stamps group ids.
  dplyr::mutate(dplyr::across(
      dplyr::matches('_level$'),
      ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
    )) |>
  dplyr::mutate(operationid = dplyr::case_when(
      stat_name == 'n' ~ 'opid1here',
      stat_name == 'p' ~ 'opid2here'
    ))
} else {
  tibble::tibble(group1_level = character(0), stat_name = character(0),
                 stat = list(), operationid = character(0))
}
