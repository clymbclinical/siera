library(tibble)

make_template <- function(method_id, code, context = "R (siera)") {
  tibble(
    method_id   = method_id,
    context     = context,
    specifiedAs = "Code",
    templateCode = code
  )
}

make_params <- function(method_id, name, source) {
  tibble(
    method_id              = method_id,
    parameter_name         = name,
    parameter_valueSource  = source
  )
}

test_that(".n_group_cols_from_template returns 0L when num_grp is 0", {
  result <- siera:::`.n_group_cols_from_template`(
    num_grp                         = 0L,
    method_id                       = "MTH01",
    analysis_method_code_template   = make_template("MTH01", "by_vars"),
    analysis_method_code_parameters = make_params("MTH01", "by_vars", "by_vars")
  )
  expect_identical(result, 0L)
})

test_that(".n_group_cols_from_template returns num_grp - 1 when by_vars is active", {
  template <- make_template("MTH01", "ard_tabulate(by = by_vars, variables = ana_var)")
  params <- tibble(
    method_id             = c("MTH01", "MTH01"),
    parameter_name        = c("by_vars", "ana_var"),
    parameter_valueSource = c("by_vars",  "ana_var")
  )

  result <- siera:::`.n_group_cols_from_template`(
    num_grp                         = 3L,
    method_id                       = "MTH01",
    analysis_method_code_template   = template,
    analysis_method_code_parameters = params
  )
  expect_identical(result, 2L)
})

test_that(".n_group_cols_from_template returns num_grp - 1 when strata_vars is active", {
  template <- make_template("MTH02", "ard_tabulate(strata = strata_vars, variables = ana_var)")
  params <- tibble(
    method_id             = c("MTH02", "MTH02"),
    parameter_name        = c("strata_vars", "ana_var"),
    parameter_valueSource = c("strata_vars", "ana_var")
  )

  result <- siera:::`.n_group_cols_from_template`(
    num_grp                         = 2L,
    method_id                       = "MTH02",
    analysis_method_code_template   = template,
    analysis_method_code_parameters = params
  )
  expect_identical(result, 1L)
})

test_that(".n_group_cols_from_template returns num_grp when by_listc is active", {
  template <- make_template("MTH03", "ard_summary(by = by_listc, variables = ana_var)")
  params <- tibble(
    method_id             = c("MTH03", "MTH03"),
    parameter_name        = c("by_listc", "ana_var"),
    parameter_valueSource = c("by_listc", "ana_var")
  )

  result <- siera:::`.n_group_cols_from_template`(
    num_grp                         = 2L,
    method_id                       = "MTH03",
    analysis_method_code_template   = template,
    analysis_method_code_parameters = params
  )
  expect_identical(result, 2L)
})

test_that(".n_group_cols_from_template returns 0L when no group-column sources are active", {
  template <- make_template("MTH04", "ard_summary(variables = ana_var)")
  params <- make_params("MTH04", "ana_var", "ana_var")

  result <- siera:::`.n_group_cols_from_template`(
    num_grp                         = 2L,
    method_id                       = "MTH04",
    analysis_method_code_template   = template,
    analysis_method_code_parameters = params
  )
  expect_identical(result, 0L)
})

test_that(".n_group_cols_from_template ignores parameters whose tokens are absent from the template", {
  # The parameter table has a by_vars entry but the template code does NOT
  # contain the string "by_vars" — it is a spurious entry.
  template <- make_template("MTH05", "ard_summary(variables = ana_var)")
  params <- tibble(
    method_id             = c("MTH05", "MTH05"),
    parameter_name        = c("by_vars", "ana_var"),
    parameter_valueSource = c("by_vars",  "ana_var")
  )

  result <- siera:::`.n_group_cols_from_template`(
    num_grp                         = 2L,
    method_id                       = "MTH05",
    analysis_method_code_template   = template,
    analysis_method_code_parameters = params
  )
  # "by_vars" is not in the template, so it must not count toward group cols.
  expect_identical(result, 0L)
})

test_that(".n_group_cols_from_template clamps num_grp - 1 to minimum of 0", {
  # num_grp = 1 with by_vars active → max(0, 1 - 1) = 0, not negative.
  template <- make_template("MTH06", "ard_tabulate(by = by_vars)")
  params <- make_params("MTH06", "by_vars", "by_vars")

  result <- siera:::`.n_group_cols_from_template`(
    num_grp                         = 1L,
    method_id                       = "MTH06",
    analysis_method_code_template   = template,
    analysis_method_code_parameters = params
  )
  expect_identical(result, 0L)
})
