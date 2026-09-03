# Denominator: the referenced analysis's population count per Group1 level.
denom_analysisidhere <- df2_denomanaidhere |>
    dplyr::count(denom_anagroupvarshere) |>
    dplyr::rename(`...ard_N...` = n) |>
    dplyr::mutate(dplyr::across(-`...ard_N...`, as.character))

# Group1's levels come from the denominator, so an arm in which no subject
# qualifies still gets a zero row; Group2's levels are the ones the ARS
# metadata DEFINES, not the ones the data happens to contain.
.arms_analysisidhere <- as.character(denom_analysisidhere$groupvar1here)
.levels_analysisidhere <- c(group2levelshere)

denom_analysisidhere <- denom_analysisidhere |>
    dplyr::mutate(groupvar1here = factor(groupvar1here, levels = .arms_analysisidhere))

in_data_analysisidhere <- df2_analysisidhere |>
    dplyr::mutate(ars_group = dplyr::case_when(
      group2conditionshere,
      TRUE ~ NA_character_
    )) |>
    # A data value satisfying none of the defined group conditions was never
    # asked for by the ARS, so it is excluded rather than tabulated.
    dplyr::filter(!is.na(ars_group)) |>
    dplyr::distinct(groupvar1here, ars_group, anavarhere) |>
    dplyr::mutate(
      groupvar1here = factor(as.character(groupvar1here), levels = .arms_analysisidhere),
      ars_group     = factor(ars_group, levels = .levels_analysisidhere)
    ) |>
    dplyr::select(groupvar1here, ars_group)

df3_analysisidhere <- if (length(.levels_analysisidhere) > 0) {
  cards::ard_tabulate(
      data = in_data_analysisidhere,
      by = 'groupvar1here',
      variables = 'ars_group',
      denominator = denom_analysisidhere
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
      stat_name == 'n' ~ 'opid1here',
      stat_name == 'p' ~ 'opid2here'
    ))
} else {
  tibble::tibble(group1_level = character(0), group2_level = character(0),
                 stat_name = character(0), stat = list(),
                 operationid = character(0))
}
