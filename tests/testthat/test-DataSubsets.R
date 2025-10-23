condition_fun <- siera:::`.generate_data_subset_condition`
code_fun <- siera:::`.generate_data_subset_code`

test_that("generate_data_subset_condition handles empty variable", {
  expect_equal(condition_fun(NULL, "EQ", "x", "json"), "")
  expect_equal(condition_fun(NA_character_, "EQ", "x", "json"), "")
})

test_that("generate_data_subset_condition translates IN comparator for xlsx", {
  out <- condition_fun(
    variable = "PARAM",
    comparator = "IN",
    value = "A|B|C",
    file_ext = "xlsx"
  )
  expect_equal(out, "PARAM %in% c('A', 'B', 'C')")
})

test_that("generate_data_subset_condition translates IN comparator for numeric vectors", {
  out <- condition_fun(
    variable = "AGE",
    comparator = "IN",
    value = c("1", "2", "3"),
    file_ext = "json"
  )
  expect_equal(out, "AGE %in% c(1, 2, 3)")
})

test_that("generate_data_subset_condition handles NE blank values", {
  expect_equal(
    condition_fun(
      variable = "TRT01AN",
      comparator = "NE",
      value = "",
      file_ext = "json"
    ),
    "!is.na(TRT01AN) & TRT01AN!= ''"
  )
  expect_equal(
    condition_fun(
      variable = "TRT01AN",
      comparator = "NE",
      value = "NA",
      file_ext = "json"
    ),
    "!is.na(TRT01AN) & TRT01AN!= ''"
  )
})

test_that("generate_data_subset_condition maps standard comparators", {
  expect_equal(
    condition_fun("AGE", "GE", "65", "json"),
    "AGE >= 65"
  )
  expect_equal(
    condition_fun("SEX", "EQ", "M", "json"),
    "SEX == 'M'"
  )
})

test_that("generate_data_subset_condition handles edge inputs", {
  expect_equal(
    condition_fun("VISIT", "IN", character(), "json"),
    "VISIT %in% c('')"
  )

  expect_equal(
    condition_fun("AGE", "IN", "1|2|3", "xlsx"),
    "AGE %in% c(1, 2, 3)"
  )
})

test_that("generate_data_subset_code returns defaults when metadata missing", {
  res_null <- code_fun(NULL, 1, "An_01", "adsl", "json")
  expect_true(grepl("Apply Data Subset", res_null$code))
  expect_null(res_null$filter_expression)
  expect_true(is.na(res_null$subset_name))

  res_na <- code_fun(tibble::tibble(), NA_integer_, "An_01", "adsl", "json")
  expect_true(grepl("Apply Data Subset", res_na$code))
  expect_null(res_na$filter_expression)
  expect_true(is.na(res_na$subset_name))

  res_missing <- code_fun(tibble::tibble(id = integer()), 99, "An_01", "adsl", "json")
  expect_true(grepl("Apply Data Subset", res_missing$code))
  expect_null(res_missing$filter_expression)
  expect_true(is.na(res_missing$subset_name))
})

test_that("generate_data_subset_code builds filter for single level", {
  metadata <- tibble::tibble(
    id = 100,
    name = "Age >= 65",
    level = 1,
    condition_variable = "AGE",
    condition_comparator = "GE",
    condition_value = "65",
    compoundExpression_logicalOperator = NA_character_
  )

  res <- code_fun(metadata, 100, "An_01", "adsl", "json")
  expect_equal(res$subset_name, "Age >= 65")
  expect_equal(res$filter_expression, "AGE >= 65")
  expect_true(grepl("dplyr::filter(AGE >= 65)", res$code, fixed = TRUE))
})

test_that("generate_data_subset_code combines multiple expressions with logical operator", {
  metadata <- tibble::tibble(
    id = c(200, 200, 200),
    name = c("High risk", "High risk", "High risk"),
    level = c(1, 2, 2),
    condition_variable = c(NA, "AGE", "SEX"),
    condition_comparator = c(NA, "GE", "EQ"),
    condition_value = c(NA, "65", "F"),
    compoundExpression_logicalOperator = c("AND", NA, NA)
  )

  res <- code_fun(metadata, 200, "An_02", "adsl", "json")
  expect_equal(res$filter_expression, "AGE >= 65 & SEX == 'F'")
  expect_true(grepl("AGE >= 65 & SEX == 'F'", res$code))
})


test_that("generate_data_subset_code builds multi-level nested expressions", {
  metadata <- tibble::tibble(
    id = rep(300, 5),
    name = rep("Complex subset", 5),
    level = c(1, 2, 2, 3, 3),
    condition_variable = c(NA, "REGION", NA, "AGE", "AGE"),
    condition_comparator = c(NA, "EQ", NA, "GE", "GE"),
    condition_value = c(NA, "EU", NA, "30", "40"),
    compoundExpression_logicalOperator = c("AND", NA, "OR", NA, NA)
  )

  res <- code_fun(metadata, 300, "An_03", "adsl", "json")
  expect_equal(res$subset_name, "Complex subset")
  expect_equal(
    res$filter_expression,
    "REGION == 'EU', AGE >= 30 | AGE >= 40"
  )
  expect_true(
    grepl("dplyr::filter(REGION == 'EU', AGE >= 30 | AGE >= 40)", res$code, fixed = TRUE)
  )
})

test_that("generate_data_subset_code returns default code when no expressions created", {
  metadata <- tibble::tibble(
    id = rep(310, 3),
    name = rep("Empty subset", 3),
    level = c(1, 2, 2),
    condition_variable = c(NA, NA, NA),
    condition_comparator = c(NA, NA, NA),
    condition_value = c(NA, NA, NA),
    compoundExpression_logicalOperator = c("AND", NA, NA)
  )

  res <- code_fun(metadata, 310, "An_04", "adsl", "json")
  expect_equal(res$subset_name, "Empty subset")
  expect_null(res$filter_expression)
  expect_true(grepl("Apply Data Subset", res$code))
})
