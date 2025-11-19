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

  target_env <- new.env(parent = emptyenv())

  result <- siera:::`.generate_analysis_method_section`(
    analysis_methods = analysis_methods,
    analysis_method_code_template = template,
    analysis_method_code_parameters = parameters,
    method_id = "MTH001",
    analysis_id = "AN001",
    output_id = "OUT123",
    envir = target_env
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
  expect_equal(target_env$operation_1, "OP01")
  expect_equal(target_env$operation_3, "OP03")

  expect_equal(result$method$name, "Descriptive stats")
  expect_equal(result$method$description, "Calculate summary")
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
