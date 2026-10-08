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

test_that("AG_var1_group_conditions makes siera stamp the single grouping (#227)", {
  # The overall (no treatment) counterpart: one pre-defined grouping, renamed
  # to group1_level by the template, so grouping 1 is stamped.
  template <- tibble::tibble(
    method_id = "MTH_GC1",
    context = "R (siera)",
    specifiedAs = "Code",
    templateCode = "df3_analysisidhere <- dplyr::case_when(group1conditionshere)"
  )
  parameters <- tibble::tibble(
    method_id = "MTH_GC1",
    parameter_name = "group1conditionshere",
    parameter_valueSource = "AG_var1_group_conditions"
  )

  expect_identical(
    siera:::.n_group_cols_from_template(
      num_grp = 1L,
      method_id = "MTH_GC1",
      analysis_method_code_template = template,
      analysis_method_code_parameters = parameters
    ),
    1L
  )
})

test_that(".ag_group_conditions names the valueSource it resolves in its messages", {
  expect_warning(
    siera:::.ag_group_conditions(.agc_test_groupings(), "AG_DD",
                                 value_source = "AG_var1_group_conditions"),
    "AG_var1_group_conditions: grouping"
  )
  # Default label stays the grouping-2 one.
  expect_warning(
    siera:::.ag_group_conditions(.agc_test_groupings(), "AG_DD"),
    "AG_var2_group_conditions: grouping"
  )
})

test_that(".generate_groupid_code maps every value of a multi-row IN group to its group id", {
  # The reader unnests a multi-value IN condition to one row per value with the
  # group id repeated, so each value maps back to the same group.
  ag <- tibble::tibble(
    id = "AG_X", group_id = c("AG_X_01", "AG_X_01", "AG_X_02"),
    group_condition_comparator = c("IN", "IN", "EQ"),
    group_condition_value = c("DRUG INTERRUPTED", "DOSE REDUCED", "OTHER")
  )
  code <- siera:::.generate_groupid_code(
    analysis_id = "An_1", groupids = "AG_X", n_group_cols = 1L,
    AG_dataDriven = "FALSE", analysis_groupings = ag
  )
  expect_match(code, "== 'DRUG INTERRUPTED' ~ 'AG_X_01'", fixed = TRUE)
  expect_match(code, "== 'DOSE REDUCED' ~ 'AG_X_01'", fixed = TRUE)
  expect_match(code, "== 'OTHER' ~ 'AG_X_02'", fixed = TRUE)
})

test_that(".ag_group_conditions aborts when two groups share a level", {
  # AG_X_01 is an IN group (one row per value); AG_X_02 repeats its first value.
  ag <- tibble::tibble(
    id = "AG_X", group_id = c("AG_X_01", "AG_X_01", "AG_X_02"),
    group_order = c(1, 1, 2),
    group_condition_variable = "AESEV",
    group_condition_comparator = c("IN", "IN", "EQ"),
    group_condition_value = c("SEVERE", "MODERATE", "SEVERE")
  )
  expect_error(
    siera:::.ag_group_conditions(ag, "AG_X"),
    "share the level"
  )
})

test_that("the per-predefined-group method is population-based (empty subset still zero-fills)", {
  # Installed package under R CMD check; source tree under devtools::test().
  for (dir in c("11_categorical_summary_per_predefined_group",
                "13_categorical_summary_per_predefined_group_overall")) {
    rel <- file.path(dir, "template.R")
    f <- system.file("method-library", rel, package = "siera")
    if (!nzchar(f)) f <- testthat::test_path("..", "..", "inst", "method-library", rel)
    tmpl <- readLines(f, warn = FALSE)
    expect_true(any(grepl("df_poptot", tmpl, fixed = TRUE)), info = dir)
  }
})

# Group names as levels (#232) ------------------------------------------------

.agc_named_groupings <- function() {
  # An IN group (two rows) whose name differs from its values, an EQ group
  # whose name differs from its value, and a group without a name.
  tibble::tibble(
    id = "AG_AGE", dataDriven = "FALSE",
    group_id = c("AG_AGE_1", "AG_AGE_2", "AG_AGE_2", "AG_AGE_3"),
    group_name = c("< 65 years", "≥ 65 years", "≥ 65 years", NA),
    group_order = c(1, 2, 2, 3),
    group_condition_variable = "AGEGR1",
    group_condition_comparator = c("EQ", "IN", "IN", "EQ"),
    group_condition_value = c("<65", "65-80", ">80", "UNK")
  )
}

test_that(".group_level is the group name, else the first condition value", {
  ag <- .agc_named_groupings()
  expect_identical(siera:::.group_level(ag[2:3, ]), "≥ 65 years")
  expect_identical(siera:::.group_level(ag[4, ]), "UNK")
  blank <- ag[1, ]
  blank$group_name <- "  "
  expect_identical(siera:::.group_level(blank), "<65")
  expect_identical(siera:::.group_level(ag[1, setdiff(names(ag), "group_name")]), "<65")
})

test_that(".ag_group_conditions uses the group names as levels (#232)", {
  res <- siera:::.ag_group_conditions(.agc_named_groupings(), "AG_AGE")
  expect_identical(
    res$levels,
    "'< 65 years', '\\u2265 65 years', 'UNK'"
  )
  expect_match(res$conditions,
               "AGEGR1 %in% c('65-80', '>80') ~ '\\u2265 65 years'", fixed = TRUE)
})

test_that(".ag_group_conditions aborts when two groups share a name", {
  ag <- .agc_named_groupings()
  ag$group_name[4] <- "< 65 years"
  expect_error(siera:::.ag_group_conditions(ag, "AG_AGE"), "share the level")
})

test_that(".grouping_stamp_spec maps group names to ids when by_name = TRUE", {
  ag <- .agc_named_groupings()
  by_value <- siera:::.grouping_stamp_spec("AG_AGE", FALSE, ag)
  expect_identical(by_value$values, c("<65", "65-80", ">80", "UNK"))
  expect_identical(by_value$group_ids, c("AG_AGE_1", "AG_AGE_2", "AG_AGE_2", "AG_AGE_3"))

  by_name <- siera:::.grouping_stamp_spec("AG_AGE", FALSE, ag, by_name = TRUE)
  expect_identical(by_name$values, c("< 65 years", "≥ 65 years", "UNK"))
  expect_identical(by_name$group_ids, c("AG_AGE_1", "AG_AGE_2", "AG_AGE_3"))

  # both generators stamp by name for that grouping
  code <- siera:::.generate_groupid_code("An_1", "AG_AGE", 1L, "FALSE", ag,
                                         stamp_by_name = TRUE)
  expect_match(code, "== '\\u2265 65 years' ~ 'AG_AGE_2'", fixed = TRUE)
  stamp <- siera:::.generate_stamp_code("An_1", "Mth_1", "Out_1", "AG_AGE", 1L,
                                        "FALSE", ag, stamp_by_name = TRUE)
  expect_match(stamp, "'AG_AGE_2' = '\\u2265 65 years'", fixed = TRUE)
})

test_that(".stamp_by_name flags the groupings a group-conditions template levels", {
  tmpl <- tibble::tibble(
    method_id = c("M_GC2", "M_GC1", "M_PLAIN"), context = "R (siera)",
    specifiedAs = "Code",
    templateCode = c("x <- case_when(conds2here)", "x <- case_when(conds1here)",
                     "by = c(byhere)")
  )
  params <- tibble::tibble(
    method_id = c("M_GC2", "M_GC1", "M_PLAIN"),
    parameter_name = c("conds2here", "conds1here", "byhere"),
    parameter_valueSource = c("AG_var2_group_conditions",
                              "AG_var1_group_levels", "by_listc")
  )
  expect_identical(siera:::.stamp_by_name(2L, "M_GC2", tmpl, params), c(FALSE, TRUE))
  expect_identical(siera:::.stamp_by_name(1L, "M_GC1", tmpl, params), TRUE)
  expect_identical(siera:::.stamp_by_name(2L, "M_PLAIN", tmpl, params), c(FALSE, FALSE))
  expect_identical(siera:::.stamp_by_name(0L, "M_GC2", tmpl, params), logical(0))
})

test_that(".escape_single_quote writes values as portable R string literals", {
  x <- c("plain", "it's", "a\\b", "Placebo \n(N=XX)", "tab\there\r",
         "≥ 65 years", "\U0001F600", NA)
  esc <- siera:::.escape_single_quote(x)
  expect_identical(esc[1], "plain")
  expect_identical(esc[2], "it\\'s")
  expect_identical(esc[6], "\\u2265 65 years")
  expect_identical(esc[7], "\\U{1F600}")
  expect_true(is.na(esc[8]))
  # every escaped value is ASCII and parses back to the original
  expect_true(all(!grepl("[^ -~]", esc[1:7])))
  for (i in 1:7) {
    expect_identical(eval(parse(text = paste0("'", esc[i], "'"))), x[i], info = x[i])
  }
  expect_identical(siera:::.escape_single_quote(character(0)), character(0))
})
