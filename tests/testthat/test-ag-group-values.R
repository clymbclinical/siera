# Unit tests for .ag_group_values(), the resolver behind the
# AG_var2_group_values valueSource (#171): a quoted, comma-separated list of a
# pre-defined grouping's group condition values, in group order.

.ag_test_groupings <- function() {
  tibble::tibble(
    id = c(rep("AG_SEV", 3), rep("AG_MIX", 2), "AG_DD"),
    groupingVariable = c(rep("AESEV", 3), rep("AEACN", 2), "AEDECOD"),
    dataDriven = c(rep("FALSE", 5), "TRUE"),
    group_id = c("AG_SEV_01", "AG_SEV_02", "AG_SEV_03",
                 "AG_MIX_01", "AG_MIX_02", NA),
    group_order = c(1, 2, 3, 1, 2, NA),
    group_condition_comparator = c("EQ", "EQ", "EQ", "EQ", "IN", NA),
    group_condition_value = c("SEVERE", "MODERATE", "MILD",
                              "DRUG INTERRUPTED", "DOSE REDUCED", NA)
  )
}

test_that(".ag_group_values returns quoted EQ condition values in group order", {
  # Rows deliberately shuffled: order must come from group_order, not row order.
  ag <- .ag_test_groupings()[c(3, 1, 2, 4, 5, 6), ]
  expect_identical(
    siera:::.ag_group_values(ag, "AG_SEV"),
    "'SEVERE', 'MODERATE', 'MILD'"
  )
})

test_that(".ag_group_values escapes single quotes in group values", {
  ag <- tibble::tibble(
    id = "AG_Q",
    group_id = "AG_Q_01",
    group_order = 1,
    group_condition_comparator = "EQ",
    group_condition_value = "PATIENT'S CHOICE"
  )
  expect_identical(
    siera:::.ag_group_values(ag, "AG_Q"),
    "'PATIENT\\'S CHOICE'"
  )
})

test_that(".ag_group_values skips non-EQ groups with a warning", {
  ag <- .ag_test_groupings()
  expect_warning(
    res <- siera:::.ag_group_values(ag, "AG_MIX"),
    "Only single-value EQ group conditions"
  )
  expect_identical(res, "'DRUG INTERRUPTED'")
})

test_that(".ag_group_values returns \"\" when every group is non-EQ", {
  ag <- tibble::tibble(
    id = c("AG_IN", "AG_IN"),
    group_id = c("AG_IN_01", "AG_IN_02"),
    group_order = c(1, 2),
    group_condition_comparator = c("IN", "NOTIN"),
    group_condition_value = c("A", "B")
  )
  expect_warning(res <- siera:::.ag_group_values(ag, "AG_IN"), "skipping")
  expect_identical(res, "")
})

test_that(".ag_group_values warns and returns \"\" for a grouping without groups", {
  # A data-driven grouping contributes only NA group rows after bind_rows.
  ag <- .ag_test_groupings()
  expect_warning(
    res <- siera:::.ag_group_values(ag, "AG_DD"),
    "defines no groups"
  )
  expect_identical(res, "")
})

test_that(".ag_group_values warns and returns \"\" when no group columns exist", {
  # Metadata where every grouping is data-driven has no group_* columns at all.
  ag <- tibble::tibble(id = "AG_DD", groupingVariable = "AEDECOD",
                       dataDriven = "TRUE")
  expect_warning(
    res <- siera:::.ag_group_values(ag, "AG_DD"),
    "defines no groups"
  )
  expect_identical(res, "")
})

test_that("function-valued value_sources resolve lazily in the method section", {
  analysis_methods <- tibble::tibble(
    id = "MTH_LZ",
    name = "Lazy test",
    description = "Lazy value source",
    label = "lazy",
    operation_id = "OP_1"
  )
  template <- tibble::tibble(
    method_id = "MTH_LZ",
    context = "R",
    specifiedAs = "Code",
    templateCode = "df3_analysisidhere <- data.frame(groups = c(group2valueshere))"
  )
  parameters <- tibble::tibble(
    method_id = "MTH_LZ",
    parameter_name = "group2valueshere",
    parameter_valueSource = "AG_var2_group_values"
  )

  result <- siera:::.generate_analysis_method_section(
    analysis_methods = analysis_methods,
    analysis_method_code_template = template,
    analysis_method_code_parameters = parameters,
    method_id = "MTH_LZ",
    analysis_id = "AN_LZ",
    output_id = "OUT_LZ",
    value_sources = list(
      AG_var2_group_values = function() "'SEVERE', 'MODERATE'"
    )
  )
  expect_match(result$code, "c('SEVERE', 'MODERATE')", fixed = TRUE)
  expect_false(grepl("group2valueshere", result$code, fixed = TRUE))
})

test_that("function-valued value_sources are NOT called unless referenced", {
  analysis_methods <- tibble::tibble(
    id = "MTH_NL",
    name = "No lazy call",
    description = "Unreferenced lazy source",
    label = "nolazy",
    operation_id = "OP_1"
  )
  template <- tibble::tibble(
    method_id = "MTH_NL",
    context = "R",
    specifiedAs = "Code",
    templateCode = "df3_analysisidhere <- data.frame(x = anavarhere)"
  )
  parameters <- tibble::tibble(
    method_id = "MTH_NL",
    parameter_name = "anavarhere",
    parameter_valueSource = "ana_var"
  )

  expect_no_warning(
    siera:::.generate_analysis_method_section(
      analysis_methods = analysis_methods,
      analysis_method_code_template = template,
      analysis_method_code_parameters = parameters,
      method_id = "MTH_NL",
      analysis_id = "AN_NL",
      output_id = "OUT_NL",
      value_sources = list(
        ana_var = "AVAL",
        AG_var2_group_values = function() {
          stop("must not be called when no parameter references it")
        }
      )
    )
  )
})