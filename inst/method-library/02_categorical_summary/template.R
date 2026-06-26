denom_dataset = df2_denomanaidhere |>
  dplyr::select(denom_anagroupvarshere)

in_data = df2_analysisidhere |>
    dplyr::distinct(distinctlisthere) |>
    dplyr::mutate(dummy = 'dummyvar')

dataDriven = isdatadrivenhere
if(dataDriven == TRUE){
df3_analysisidhere <-
  cards::ard_tabulate(
    data = in_data,
    strata = c(byvarshere),
    variables = 'dummy',
    denominator = denom_dataset
  ) } else {
df3_analysisidhere <-
 cards::ard_tabulate(
    data = in_data,
    by = c(byvarshere),
    variables = 'dummy',
    denominator = denom_dataset
  ) }
df3_analysisidhere <- df3_analysisidhere|>
  dplyr::filter(stat_name %in% c('n', 'p')) |>
  dplyr::mutate(operationid = dplyr::case_when(stat_name == 'n' ~ 'opid1here',
                                               stat_name == 'p' ~ 'opid2here'))
