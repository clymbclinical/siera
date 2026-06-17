library(tibble)


test_that(".generate_analysis_method_section constructs method block with operations", {
  analysis_methods <- tibble(
    id = c("MTH001", "MTH001", "MTH001"),
    name = c("Descriptive stats", "Descriptive stats", "Descriptive stats"),
    description = c("Calculate summary", "Calculate summary", "Calculate summary"),
    label = c("desc", "desc", "desc"),
    operation_id = c("OP01", "OP02", "OP03")
  )

  template <- tibble(
    method_id = "MTH001",
    context = "R",
    specifiedAs = "Code",
    templateCode = "df3_analysisidhere <- df3_analysisidhere |> dplyr::mutate(result = parameter_placeholder)"
  )

  parameters <- tibble(
    method_id = "MTH001",
    parameter_name = "parameter_placeholder",
    parameter_valueSource = "operation_2"
  )

  result <- siera:::`.generate_analysis_method_section`(
    analysis_methods = analysis_methods,
    analysis_method_code_template = template,
    analysis_method_code_parameters = parameters,
    method_id = "MTH001",
    analysis_id = "AN001",
    output_id = "OUT123"
  )

  expect_type(result$code, "character")
  expect_match(result$code, "Method ID:\\s+MTH001")
  expect_match(result$code, "Method name:\\s+Descriptive stats")
  expect_match(result$code, "result = OP02")
  expect_false(grepl("parameter_placeholder", result$code, fixed = TRUE))

  expect_equal(
    result$operations,
    list(operation_1 = "OP01", operation_2 = "OP02", operation_3 = "OP03")
  )

  expect_equal(result$method$name, "Descriptive stats")
  expect_equal(result$method$description, "Calculate summary")
})

test_that("caller environment is not mutated and value_sources resolves non-operation parameters", {
  analysis_methods <- tibble(
    id = c("MTH_SRC", "MTH_SRC"),
    name = c("Source test", "Source test"),
    description = c("Test value_sources arg", "Test value_sources arg"),
    label = c("src", "src"),
    operation_id = c("OP_A", "OP_B")
  )

  template <- tibble(
    method_id = "MTH_SRC",
    context = "R",
    specifiedAs = "Code",
    templateCode = "df3_analysisidhere <- cards::ard_tabulate(data = df, by_here, variables = ana_var_here)"
  )

  parameters <- tibble(
    method_id = c("MTH_SRC", "MTH_SRC"),
    parameter_name = c("by_here", "ana_var_here"),
    parameter_valueSource = c("by_vars", "ana_var")
  )

  caller_env <- new.env(parent = emptyenv())

  result <- siera:::`.generate_analysis_method_section`(
    analysis_methods = analysis_methods,
    analysis_method_code_template = template,
    analysis_method_code_parameters = parameters,
    method_id = "MTH_SRC",
    analysis_id = "AN_SRC",
    output_id = "OUT_SRC",
    value_sources = list(by_vars = ", by = 'TRT01A'", ana_var = "AVAL")
  )

  # Caller environment must NOT have been mutated
  expect_length(ls(caller_env), 0L)

  # value_sources values are substituted into the template
  expect_match(result$code, "by = 'TRT01A'", fixed = TRUE)
  expect_match(result$code, "AVAL", fixed = TRUE)

  # Operation IDs are still returned correctly
  expect_equal(result$operations, list(operation_1 = "OP_A", operation_2 = "OP_B"))
})

test_that(".generate_analysis_method_section keeps placeholders when no parameters available", {
  analysis_methods <- tibble(
    id = "MTH100",
    name = "Method with no params",
    description = "A method without substitutions",
    label = "label",
    operation_id = "OP10"
  )

  template <- tibble(
    method_id = "MTH100",
    context = "R (siera)",
    specifiedAs = "Code",
    templateCode = "df3_analysisidhere <- df3_analysisidhere |> dplyr::mutate(flag = parameter_missing)"
  )

  parameters <- tibble(
    method_id = "MTH100",
    parameter_name = "parameter_missing",
    parameter_valueSource = ""
  )

  result <- siera:::`.generate_analysis_method_section`(
    analysis_methods = analysis_methods,
    analysis_method_code_template = template,
    analysis_method_code_parameters = parameters,
    method_id = "MTH100",
    analysis_id = "AN_NO_PARAM",
    output_id = "OUT_NO_PARAM"
  )

  expect_match(result$code, "Method ID:\\s+MTH100")
  expect_match(
    result$code,
    "df3_AN_NO_PARAM <- df3_AN_NO_PARAM \\|> dplyr::mutate\\(flag = parameter_missing\\)"
  )
  expect_equal(result$operations, list(operation_1 = "OP10"))
})

test_that("errors when a referenced method is missing", {
  skip_on_cran()

  metadata <- siera:::.read_ars_metadata(ARS_example("exampleARS_2.json"))
  analysis_id <- metadata$Analyses$id[1]
  method_id   <- metadata$Analyses$method_id[1]
  output_id   <- metadata$Lopa |>
    dplyr::filter(listItem_analysisId == analysis_id) |>
    dplyr::pull(listItem_outputId) |>
    _[1]

  expect_error(
    siera:::`.generate_analysis_method_section`(
      analysis_methods = metadata$AnalysisMethods |> dplyr::filter(id != method_id),
      analysis_method_code_template = metadata$AnalysisMethodCodeTemplate,
      analysis_method_code_parameters = metadata$AnalysisMethodCodeParameters,
      method_id   = method_id,
      analysis_id = analysis_id,
      output_id   = output_id,
      value_sources = list()
    ),
    "MethodId .* is not defined in AnalysisMethods metadata"
  )
})

test_that("warns when a parameter value source cannot be resolved", {
  skip_on_cran()

  metadata          <- siera:::.read_ars_metadata(ARS_example("exampleARS_2.json"))
  method_with_param <- metadata$AnalysisMethodCodeParameters$method_id[1]
  analysis_id       <- metadata$Analyses |>
    dplyr::filter(method_id == method_with_param) |>
    dplyr::pull(id) |>
    _[1]
  output_id <- metadata$Lopa |>
    dplyr::filter(listItem_analysisId == analysis_id) |>
    dplyr::pull(listItem_outputId) |>
    _[1]

  bad_parameters <- metadata$AnalysisMethodCodeParameters
  first_idx      <- which(bad_parameters$method_id == method_with_param)[1]
  bad_parameters$parameter_valueSource[first_idx] <- "missing_value_source"

  expect_warning(
    result <- siera:::`.generate_analysis_method_section`(
      analysis_methods = metadata$AnalysisMethods,
      analysis_method_code_template = metadata$AnalysisMethodCodeTemplate,
      analysis_method_code_parameters = bad_parameters,
      method_id   = method_with_param,
      analysis_id = analysis_id,
      output_id   = output_id,
      value_sources = list()
    ),
    "parameter .* references unknown valueSource"
  )

  expect_type(result$code, "character")
})

test_that(".generate_analysis_method_section warns when no template exists", {
  analysis_methods <- tibble(
    id = "MTH200",
    name = "Method without template",
    description = "Used when templates are missing",
    label = "label",
    operation_id = "OP200"
  )

  template <- tibble(
    method_id = character(),
    context = character(),
    specifiedAs = character(),
    templateCode = character()
  )

  parameters <- tibble(
    method_id = character(),
    parameter_name = character(),
    parameter_valueSource = character()
  )

  expect_warning(
    result <- siera:::`.generate_analysis_method_section`(
      analysis_methods = analysis_methods,
      analysis_method_code_template = template,
      analysis_method_code_parameters = parameters,
      method_id = "MTH200",
      analysis_id = "AN_WARN",
      output_id = "OUT_WARN"
    ),
    "does not have any AnalysisMethodCode entries"
  )

  expect_match(result$code, "Method ID:\\s+MTH200")
  expect_match(result$code, "Apply Method")
  expect_equal(result$operations, list(operation_1 = "OP200"))
})
