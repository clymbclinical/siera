# Method library: chisq_per_predefined_group (#215)
#
# The template is rendered the way readARS() substitutes it (placeholder ->
# resolved valueSource) and run against the bundled ADSL; the p-value is
# checked against an independent stats::chisq.test() on the grouped table.

.render_library_template <- function(key, values) {
  # Installed package under R CMD check; source tree under devtools::test().
  rel <- file.path(key, "template.R")
  f <- system.file("method-library", rel, package = "siera")
  if (!nzchar(f)) {
    f <- testthat::test_path("..", "..", "inst", "method-library", rel)
  }
  code <- paste(readLines(f, warn = FALSE), collapse = "\n")
  for (token in names(values)) {
    code <- gsub(token, values[[token]], code, fixed = TRUE)
  }
  code
}

.run_template <- function(key, values, df2) {
  env <- new.env()
  assign(paste0("df2_", values[["analysisidhere"]]), df2, envir = env)
  eval(parse(text = .render_library_template(key, values)), envir = env)
  get(paste0("df3_", values[["analysisidhere"]]), envir = env)
}

.p <- function(ard) unname(unlist(ard$stat))

.adsl_saf <- function() {
  adsl <- utils::read.csv(ARS_example("ADSL.csv"), stringsAsFactors = FALSE)
  adsl[adsl$SAFFL == "Y", ]
}

# Group definitions as the JSON reader delivers them: an IN group contributes
# one row per value.
.age_groups <- function(values = c("<65", "65-80", ">80"),
                        ids = c("AG_A_1", "AG_A_2", "AG_A_2"),
                        comparators = c("EQ", "IN", "IN")) {
  tibble::tibble(
    id = "AG_A", group_id = ids, group_order = as.integer(factor(ids)),
    group_condition_variable = "AGEGR1",
    group_condition_comparator = comparators,
    group_condition_value = values
  )
}

test_that("chisq_per_predefined_group tests the ARS-defined groups (#215)", {
  skip_if_not_installed("cardx")
  adsl <- .adsl_saf()
  conditions <- siera:::.ag_group_conditions(.age_groups(), "AG_A")$conditions

  ard <- .run_template("12_chisq_per_predefined_group", c(
    analysisidhere       = "An_T",
    groupvar1here        = "TRT01A",
    group2conditionshere = conditions,
    opid1here            = "Op_pval"
  ), adsl)

  # "< 65" vs ">= 65" (= "65-80" + ">80"), as the analysis grouping defines
  grouped <- ifelse(adsl$AGEGR1 == "<65", "<65", ">=65")
  expected <- stats::chisq.test(table(grouped, adsl$TRT01A))$p.value

  expect_identical(ard$operationid, "Op_pval")
  expect_equal(.p(ard), expected)
  expect_equal(round(.p(ard), 4), 0.4239)

  # The raw-value chisq method tests the three AGEGR1 categories instead.
  raw <- .run_template("07_chisq", c(
    analysisidhere = "An_R",
    groupvar1here  = "TRT01A",
    groupvar2here  = "AGEGR1",
    opid1here      = "Op_pval"
  ), adsl)
  expect_equal(round(.p(raw), 4), 0.1439)
})

test_that("for one-to-one groups it equals the raw-value chisq method", {
  skip_if_not_installed("cardx")
  adsl <- .adsl_saf()
  sex <- tibble::tibble(
    id = "AG_S", group_id = c("AG_S_1", "AG_S_2"), group_order = 1:2,
    group_condition_variable = "SEX",
    group_condition_comparator = "EQ",
    group_condition_value = c("M", "F")
  )
  conditions <- siera:::.ag_group_conditions(sex, "AG_S")$conditions

  grouped <- .run_template("12_chisq_per_predefined_group", c(
    analysisidhere = "An_G", groupvar1here = "TRT01A",
    group2conditionshere = conditions, opid1here = "Op_pval"
  ), adsl)
  raw <- .run_template("07_chisq", c(
    analysisidhere = "An_R", groupvar1here = "TRT01A",
    groupvar2here = "SEX", opid1here = "Op_pval"
  ), adsl)

  expect_equal(.p(grouped), .p(raw))
})

test_that("values outside every defined group are excluded from the test", {
  skip_if_not_installed("cardx")
  adsl <- .adsl_saf()
  # Only "< 65" and "65-80" are defined; ">80" subjects are not tested.
  two <- .age_groups(values = c("<65", "65-80"), ids = c("AG_A_1", "AG_A_2"),
                     comparators = "EQ")
  conditions <- siera:::.ag_group_conditions(two, "AG_A")$conditions

  ard <- .run_template("12_chisq_per_predefined_group", c(
    analysisidhere = "An_T", groupvar1here = "TRT01A",
    group2conditionshere = conditions, opid1here = "Op_pval"
  ), adsl)

  kept <- adsl[adsl$AGEGR1 %in% c("<65", "65-80"), ]
  expected <- stats::chisq.test(table(kept$AGEGR1, kept$TRT01A))$p.value
  expect_equal(.p(ard), expected)
})
