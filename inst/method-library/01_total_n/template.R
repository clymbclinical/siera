in_data = df2_analysisidhere |>
    dplyr::select(anavarhere, groupvar1here) |>
    unique()
df3_analysisidhere <-
  cards::ard_tabulate(
    data = in_data
    bystmthere
  ) |>
  dplyr::filter(stat_name == 'n') |>
  dplyr::mutate(operationid = 'opid1here')
