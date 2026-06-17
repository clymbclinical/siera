#' Format numeric values consistently
#'
#' Internal helper to apply consistent number formatting across analyses.
#' @param analysis_methods AnalysisMethod dataset for the reporting event
#' @param analysis_method_code_template AnalysisMethodCodeTemplate dataset for the reporting event
#' @param analysis_method_code_parameters AnalysisMethodCodeParameters dataset for the reporting event
#' @param method_id MethodId for the method applied to current Analysis
#' @param analysis_id AnalysisId for current Analysis
#' @param output_id OutputId to which current Analysis belongs
#' @param value_sources Named list of string values that ARS code-template
#'   parameters can reference via their valueSource key (e.g. by_vars, ana_var,
#'   AG_var1). Operation IDs (operation_1, operation_2, …) are derived from the
#'   method itself and do not need to be supplied here.

#' @return Character vector with formatted numbers.
#' @keywords internal

.generate_analysis_method_section <- function(analysis_methods,
                                              analysis_method_code_template,
                                              analysis_method_code_parameters,
                                              method_id,
                                              analysis_id,
                                              output_id,
                                              value_sources = list()) {
  if (is.null(method_id) || length(method_id) == 0 || is.na(method_id) || identical(method_id, "")) {
    cli::cli_abort(
      "Metadata issue in Analyses {analysis_id}: Analysis is missing a MethodId; method-specific code cannot be generated"
    )
  }

  method_id <- as.character(method_id)[1]

  # Retrieve the method attributes that describe this analysis step.
  method <- analysis_methods %>%
    dplyr::filter(id == method_id) %>%
    dplyr::select(name, description, label, id) %>%
    unique()

  if (nrow(method) == 0) {
    cli::cli_abort(
      "Metadata issue in Analyses {analysis_id}: MethodId {method_id} is not defined in AnalysisMethods metadata"
    )
  }

  # Capture every operation linked to the method so they can be referenced
  # by the generated code when needed.
  operations <- analysis_methods %>%
    dplyr::filter(id == method_id) %>%
    dplyr::select(operation_id)

  operation_values <- operations$operation_id

  operations_named <- stats::setNames(
    as.list(operation_values),
    paste0("operation_", seq_along(operation_values))
  )

  # Merge self-computed operation IDs with caller-supplied value sources so
  # the parameter substitution loop has a single flat lookup table.
  # Operation IDs always shadow caller-supplied values of the same name.
  all_value_sources <- c(operations_named, value_sources)

  methodname <- method$name
  methoddesc <- method$description
  methodlabel <- method$label
  methodid <- method$id

  template_code <- analysis_method_code_template %>%
    dplyr::filter(
      method_id == methodid,
      context %in% c("R", "R (siera)", "siera"),
      specifiedAs == "Code"
    ) %>%
    dplyr::pull(templateCode)

  if (length(template_code) == 0 || all(is.na(template_code))) {
    cli::cli_warn(
      "AnalysisMethod {.val {methodid}} linked to Analysis {.val {analysis_id}} does not have any AnalysisMethodCode entries. Method-specific code generation will be skipped."
    )
    template_code <- ""
  } else {
    template_code <- paste0(template_code, collapse = "\n")
  }

  # Parameter substitution is based on ARS metadata entries that can refer
  # to values calculated earlier in the script.
  anmetparam_s <- analysis_method_code_parameters %>%
    dplyr::filter(
      method_id == methodid,
      parameter_valueSource != ""
    )

  anmetcode_temp <- paste0(
    "if(nrow(df2_analysisidhere) != 0) {\n                              ",
    template_code,
    "}"
  )

  for (i in seq_len(nrow(anmetparam_s))) {
    value_source <- anmetparam_s$parameter_valueSource[i]
    parameter_name <- anmetparam_s$parameter_name[i]

    if (!value_source %in% names(all_value_sources)) {
      cli::cli_warn(
        "Metadata issue in AnalysisMethodCodeParameters for Analysis {analysis_id}: parameter {.val {parameter_name}} references unknown valueSource {.val {value_source}}"
      )
      next
    }

    rep <- all_value_sources[[value_source]]
    if (!is.null(rep) && !is.na(rep)) {
      anmetcode_temp <- gsub(
        parameter_name,
        rep,
        anmetcode_temp,
        fixed = TRUE
      )
    }
  }

  anmetcode_final <- gsub("methodidhere", methodid, anmetcode_temp, fixed = TRUE)
  anmetcode_final <- gsub("analysisidhere", analysis_id, anmetcode_final, fixed = TRUE)

  template_intro <- "
# Method ID:              methodidhere
# Method name:            methodnamehere
# Method description:     methoddeschere
"

  code_method_tmp_1 <- gsub("methodidhere", methodid, template_intro, fixed = TRUE)
  code_method_tmp_1 <- gsub("methodnamehere", methodname, code_method_tmp_1, fixed = TRUE)
  code_method_tmp_1 <- gsub("methoddeschere", methoddesc, code_method_tmp_1, fixed = TRUE)

  template <-
    "\nif(nrow(df2_analysisidhere) != 0){\ndf3_analysisidhere <- df3_analysisidhere |>
        dplyr::mutate(AnalysisId = 'analysisidhere',
               MethodId = 'methodidhere',
               OutputId = 'outputidhere')\n} else {\n    df3_analysisidhere = data.frame(AnalysisId = 'analysisidhere',
               MethodId = 'methodidhere',
               OutputId = 'outputidhere')\n}\n    "

  code <- gsub("methodidhere", methodid, template, fixed = TRUE)
  code <- gsub("analysisidhere", analysis_id, code, fixed = TRUE)
  code_method_tmp_3 <- gsub("outputidhere", output_id, code, fixed = TRUE)

  code_method <- paste0(
    code_method_tmp_1, "\n",
    anmetcode_final,
    code_method_tmp_3
  )

  list(
    code = paste0("#Apply Method --- \n", code_method),
    operations = operations_named,
    method = method
  )
}
