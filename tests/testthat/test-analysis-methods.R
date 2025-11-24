skip_on_cran()

make_metadata <- function() {
  ARS_path <- ARS_example("exampleARS_2.json")
  siera:::.read_ars_metadata(ARS_path)
}

test_that("errors when a referenced method is missing", {
  metadata <- make_metadata()
  analysis_id <- metadata$Analyses$id[1]
  method_id <- metadata$Analyses$method_id[1]
  output_id <- metadata$Lopa %>%
    dplyr::filter(listItem_analysisId == analysis_id) %>%
    dplyr::pull(listItem_outputId) %>%
    .[1]

  expect_error(
    siera:::.generate_analysis_method_section(
      analysis_methods = metadata$AnalysisMethods %>% dplyr::filter(id != method_id),
      analysis_method_code_template = metadata$AnalysisMethodCodeTemplate,
      analysis_method_code_parameters = metadata$AnalysisMethodCodeParameters,
      method_id = method_id,
      analysis_id = analysis_id,
      output_id = output_id,
      envir = new.env(parent = baseenv())
    ),
    "MethodId .* is not defined in AnalysisMethods metadata"
  )
})

test_that("warns when a parameter value source cannot be resolved", {
  metadata <- make_metadata()

  method_with_param <- metadata$AnalysisMethodCodeParameters$method_id[1]
  analysis_id <- metadata$Analyses %>%
    dplyr::filter(method_id == method_with_param) %>%
    dplyr::pull(id) %>%
    .[1]
  output_id <- metadata$Lopa %>%
    dplyr::filter(listItem_analysisId == analysis_id) %>%
    dplyr::pull(listItem_outputId) %>%
    .[1]

  bad_parameters <- metadata$AnalysisMethodCodeParameters
  first_idx <- which(bad_parameters$method_id == method_with_param)[1]
  bad_parameters$parameter_valueSource[first_idx] <- "missing_value_source"

  expect_warning(
    result <- siera:::.generate_analysis_method_section(
      analysis_methods = metadata$AnalysisMethods,
      analysis_method_code_template = metadata$AnalysisMethodCodeTemplate,
      analysis_method_code_parameters = bad_parameters,
      method_id = method_with_param,
      analysis_id = analysis_id,
      output_id = output_id,
      envir = new.env(parent = baseenv())
    ),
    "parameter .* references unknown valueSource"
  )

  expect_type(result$code, "character")
})
