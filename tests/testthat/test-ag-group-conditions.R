# Unit tests for .ag_group_conditions(), the resolver behind the
# AG_var2_group_conditions / AG_var2_group_levels valueSources (#187): the
# dplyr::case_when() body that maps data values onto a pre-defined grouping's
# defined groups, plus the matching factor levels that zero-fill absent groups.

.agc_test_groupings <- function() {
  tibble::tibble(
    id = c(rep("AG_SEV", 3), rep("AG_IN", 3), rep("AG_MIX", 2), "AG_DD"),
    groupingVariable = c(rep("AESEV", 3), rep("AEACN", 3), rep("AGE", 2), "AEDECOD"),
    dataDriven = c(rep("FALSE", 8), "TRUE"),
    group_id = c("AG_SEV_01", "AG_SEV_02", "AG_SEV_03",
                 # One IN group contributing two rows (the JSON reader unnests
                 # condition.value), followed by an EQ group.
                 "AG_IN_01", "AG_IN_01", "AG_IN_02",
                 "AG_MIX_01", "AG_MIX_02", NA),
    group_order = c(1, 2, 3, 1, 1, 2, 1, 2, NA),
    group_condition_variable = c(rep("AESEV", 3), rep("AEACN", 3), rep("AGE", 2), NA),
    group_condition_comparator = c("EQ", "EQ", "EQ", "IN", "IN", "EQ", "GE", "EQ", NA),
    group_condition_value = c("SEVERE", "MODERATE", "MILD",
                              "DRUG INTERRUPTED", "DOSE REDUCED", "OTHER",
                              "65", "18", NA)
  )
}

test_that(".ag_group_conditions builds a case_when body and levels in group order", {
  # Rows deliberately shuffled: order must come from group_order, not row order.
  ag <- .agc_test_groupings()[c(3, 1, 2, 4, 5, 6, 7, 8, 9), ]
  res <- siera:::.ag_group_conditions(ag, "AG_SEV")

  expect_identical(
    res$conditions,
    paste0("AESEV == 'SEVERE' ~ 'SEVERE',\n      ",
           "AESEV == 'MODERATE' ~ 'MODERATE',\n      ",
           "AESEV == 'MILD' ~ 'MILD'")
  )
  expect_identical(res$levels, "'SEVERE', 'MODERATE', 'MILD'")
})

test_that(".ag_group_conditions keeps a multi-value IN group as ONE group", {
  # The whole point of #187: an IN group is a single category whose condition
  # covers several data values, and its level is the first of them.
  res <- siera:::.ag_group_conditions(.agc_test_groupings(), "AG_IN")

  expect_identical(
    res$conditions,
    paste0("AEACN %in% c('DRUG INTERRUPTED', 'DOSE REDUCED') ~ 'DRUG INTERRUPTED',",
           "\n      AEACN == 'OTHER' ~ 'OTHER'")
  )
  expect_identical(res$levels, "'DRUG INTERRUPTED', 'OTHER'")
})

test_that(".ag_group_conditions splits a delimited xlsx IN cell for the level too", {
  # The xlsx reader keeps a multi-value IN condition in one pipe-delimited cell.
  ag <- tibble::tibble(
    id = "AG_X", group_id = "AG_X_01", group_order = 1,
    group_condition_variable = "AEACN",
    group_condition_comparator = "IN",
    group_condition_value = "DRUG INTERRUPTED|DOSE REDUCED"
  )
  res <- siera:::.ag_group_conditions(ag, "AG_X", file_ext = "xlsx")

  expect_identical(
    res$conditions,
    "AEACN %in% c('DRUG INTERRUPTED', 'DOSE REDUCED') ~ 'DRUG INTERRUPTED'"
  )
  expect_identical(res$levels, "'DRUG INTERRUPTED'")
})

test_that(".ag_group_conditions escapes single quotes in group levels", {
  ag <- tibble::tibble(
    id = "AG_Q", group_id = "AG_Q_01", group_order = 1,
    group_condition_variable = "AEACN",
    group_condition_comparator = "EQ",
    group_condition_value = "PATIENT'S CHOICE"
  )
  res <- siera:::.ag_group_conditions(ag, "AG_Q")

  expect_identical(res$levels, "'PATIENT\\'S CHOICE'")
  expect_match(res$conditions, "~ 'PATIENT\\\\'S CHOICE'$")
})

test_that(".ag_group_conditions skips comparators other than EQ/IN with a warning", {
  expect_warning(
    res <- siera:::.ag_group_conditions(.agc_test_groupings(), "AG_MIX"),
    "Only EQ and IN group conditions"
  )
  # The GE group is dropped; the EQ group survives.
  expect_identical(res$conditions, "AGE == 18 ~ '18'")
  expect_identical(res$levels, "'18'")
})

test_that(".ag_group_conditions falls back to a no-match arm when nothing is usable", {
  ag <- tibble::tibble(
    id = c("AG_N", "AG_N"),
    group_id = c("AG_N_01", "AG_N_02"),
    group_order = c(1, 2),
    group_condition_variable = c("AGE", "AGE"),
    group_condition_comparator = c("GT", "LT"),
    group_condition_value = c("65", "18")
  )
  expect_warning(res <- siera:::.ag_group_conditions(ag, "AG_N"), "skipping")

  # The fallback must still parse where the template embeds it.
  expect_identical(res$conditions, "FALSE ~ NA_character_")
  expect_identical(res$levels, "")
  expect_no_error(parse(text = paste0(
    "dplyr::case_when(", res$conditions, ", TRUE ~ NA_character_)"
  )))
})

test_that(".ag_group_conditions skips a group whose condition is unusable", {
  # A missing condition variable yields an empty expression from
  # .generate_data_subset_condition(), which cannot be used as a case_when arm.
  ag <- tibble::tibble(
    id = "AG_E", group_id = "AG_E_01", group_order = 1,
    group_condition_variable = NA_character_,
    group_condition_comparator = "EQ",
    group_condition_value = "SEVERE"
  )
  expect_warning(res <- siera:::.ag_group_conditions(ag, "AG_E"), "skipping")
  expect_identical(res$conditions, "FALSE ~ NA_character_")
})

test_that(".ag_group_conditions warns for a grouping that defines no groups", {
  # A data-driven grouping contributes only NA group rows after bind_rows.
  expect_warning(
    res <- siera:::.ag_group_conditions(.agc_test_groupings(), "AG_DD"),
    "defines no groups"
  )
  expect_identical(res$conditions, "FALSE ~ NA_character_")
  expect_identical(res$levels, "")
})

test_that(".ag_group_conditions warns when no group columns exist at all", {
  # Metadata where every grouping is data-driven has no group_* columns.
  ag <- tibble::tibble(id = "AG_DD", groupingVariable = "AEDECOD",
                       dataDriven = "TRUE")
  expect_warning(
    res <- siera:::.ag_group_conditions(ag, "AG_DD"),
    "defines no groups"
  )
  expect_identical(res$conditions, "FALSE ~ NA_character_")
  expect_identical(res$levels, "")
})

test_that("AG_var2_group_conditions makes siera stamp a full set of group columns", {
  # A template driven by group conditions puts the inner grouping into
  # variables= (like by_vars) but renames variable_level to group2_level
  # itself, so it produces num_grp group columns, not num_grp - 1.
  template <- tibble::tibble(
    method_id = "MTH_GC",
    context = "R (siera)",
    specifiedAs = "Code",
    templateCode = "df3_analysisidhere <- dplyr::case_when(group2conditionshere)"
  )
  parameters <- tibble::tibble(
    method_id = "MTH_GC",
    parameter_name = "group2conditionshere",
    parameter_valueSource = "AG_var2_group_conditions"
  )

  expect_identical(
    siera:::.n_group_cols_from_template(
      num_grp = 2L,
      method_id = "MTH_GC",
      analysis_method_code_template = template,
      analysis_method_code_parameters = parameters
    ),
    2L
  )
})
