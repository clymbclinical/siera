# Pre-defined groupings as ARS-ordered factors (#203): for a dataDriven: false
# grouping consumed through a generic valueSource (by_vars, by_listc, ...),
# readARS() rewrites the grouping variable on df2_<analysis> into a factor of
# the declared groups, so the ARS - not the data - determines the ARD row set.

.dg_groupings <- function() {
  tibble::tibble(
    id = c(rep("AG_TRT", 3), rep("AG_SEX", 3), "AG_DD", rep("AG_TOT", 2),
           rep("AG_OTHER", 2), rep("AG_DUP", 2), rep("AG_OVER", 5)),
    groupingVariable = c(rep("TRT01A", 3), rep("SEX", 3), "AEDECOD",
                         rep("TRT01A", 2), rep("AGEGR1", 2), rep("SEX", 2),
                         rep("TRT01A", 5)),
    dataDriven = c(rep("FALSE", 6), "TRUE", rep("FALSE", 11)),
    group_id = c("AG_TRT_1", "AG_TRT_2", "AG_TRT_3",
                 # an IN group contributes one row per value
                 "AG_SEX_1", "AG_SEX_2", "AG_SEX_2",
                 NA,
                 "AG_TOT_1", "AG_TOT_2",
                 "AG_OTHER_1", "AG_OTHER_2",
                 "AG_DUP_1", "AG_DUP_2",
                 # a 'Total' group defined as IN over the arms: all EQ/IN,
                 # distinct levels, but overlapping value sets
                 "AG_OVER_1", "AG_OVER_2", "AG_OVER_3", "AG_OVER_3", "AG_OVER_3"),
    group_name = c("Placebo arm", "Low dose arm", "High dose arm",
                   "Male", "Female or other", "Female or other",
                   NA, "Placebo", "Total",
                   "<65", ">=65",
                   "M", "Male",
                   "Placebo", "Low", "Total", "Total", "Total"),
    group_order = c(1, 2, 3, 1, 2, 2, NA, 1, 2, 1, 2, 1, 2, 1, 2, 3, 3, 3),
    group_condition_variable = c(rep("TRT01A", 3), rep("SEX", 3), NA,
                                 rep("TRT01A", 2), rep("AGEGR1N", 2), rep("SEX", 2),
                                 rep("TRT01A", 5)),
    group_condition_comparator = c("EQ", "EQ", "EQ", "EQ", "IN", "IN", NA,
                                   "EQ", "NE", "EQ", "EQ", "EQ", "EQ",
                                   "EQ", "EQ", "IN", "IN", "IN"),
    group_condition_value = c("Placebo", "Low", "High", "M", "F", "X", NA,
                              "Placebo", "", "1", "2", "M", "M",
                              "Placebo", "Low", "Placebo", "Low", "High")
  )
}

.dg_template <- function(code = "cards::ard_tabulate(df2_analysisidhere byvarshere)") {
  tibble::tibble(method_id = "M1", context = "R", specifiedAs = "Code",
                 templateCode = code)
}

.dg_params <- function(value_sources = "by_vars") {
  tibble::tibble(method_id = "M1",
                 parameter_name = c("byvarshere", "analysisidhere")[seq_along(value_sources)],
                 parameter_valueSource = value_sources)
}

.dg_plan <- function(groupids, num_grp = length(groupids),
                     template = .dg_template(), params = .dg_params(),
                     subset_variables = character(0),
                     groupings = .dg_groupings()) {
  ag_vars <- vapply(groupids, function(g) {
    groupings$groupingVariable[groupings$id == g][1]
  }, character(1L), USE.NAMES = FALSE)
  ag_dd <- vapply(groupids, function(g) {
    groupings$dataDriven[groupings$id == g][1]
  }, character(1L), USE.NAMES = FALSE)
  siera:::.derived_grouping_plan(
    groupids = groupids, num_grp = num_grp, AG_vars = ag_vars,
    AG_dataDriven = ag_dd, analysis_groupings = groupings,
    method_id = "M1", analysis_method_code_template = template,
    analysis_method_code_parameters = params,
    subset_variables = subset_variables
  )
}

test_that(".derived_group_level keeps an EQ value and names an IN group", {
  ag <- .dg_groupings()
  expect_identical(siera:::.derived_group_level(ag[ag$group_id %in% "AG_SEX_1", ]), "M")
  expect_identical(siera:::.derived_group_level(ag[ag$group_id %in% "AG_SEX_2", ]),
                   "Female or other")
})

test_that(".derived_grouping_plan derives eligible pre-defined groupings only", {
  # outer pre-defined + inner pre-defined, both via by_vars
  expect_identical(as.vector(.dg_plan(c("AG_TRT", "AG_SEX"))), c(TRUE, TRUE))
  # a data-driven grouping is left alone
  expect_identical(as.vector(.dg_plan(c("AG_TRT", "AG_DD"))), c(TRUE, FALSE))
  # only the active (resultsByGroup) groupings are candidates
  expect_identical(as.vector(.dg_plan(c("AG_TRT", "AG_SEX"), num_grp = 1)),
                   c(TRUE, FALSE))
  expect_identical(as.vector(.dg_plan("AG_TRT", num_grp = 0)), FALSE)
  # a grouping the data subset conditions on is a deliberate slice
  expect_identical(as.vector(.dg_plan(c("AG_TRT", "AG_SEX"), subset_variables = "SEX")),
                   c(TRUE, FALSE))
  # a pre-defined grouping without declared groups has nothing to derive
  ag <- .dg_groupings()
  ag$group_id[ag$id == "AG_TRT"] <- NA
  expect_identical(as.vector(.dg_plan("AG_TRT", groupings = ag)), FALSE)
})

test_that(".derived_grouping_plan leaves templates that do not use generic valueSources", {
  # population-based templates (risk differences) join onto df_poptot
  pop <- .dg_template("df_poptot |> dplyr::filter(byvarshere)")
  expect_identical(as.vector(.dg_plan("AG_TRT", template = pop)), FALSE)
  # the grouping is only reached through AG_var1 (e.g. chi-square, anova)
  expect_identical(as.vector(.dg_plan("AG_TRT", params = .dg_params("AG_var1"))), FALSE)
  # the template already aggregates per declared group itself (#187)
  own <- .dg_params(c("by_vars", "AG_var1_group_conditions"))
  own$parameter_name[2] <- "group1conditionshere"
  tpl <- .dg_template("cards::ard_tabulate(df2 byvarshere) group1conditionshere")
  expect_identical(as.vector(.dg_plan("AG_TRT", template = tpl, params = own)), FALSE)
  # a reporting event whose groupings are all data-driven has no group columns
  bare <- .dg_groupings()[, c("id", "groupingVariable", "dataDriven")]
  expect_identical(as.vector(.dg_plan("AG_TRT", groupings = bare)), FALSE)
})

test_that(".derived_grouping_plan reports groupings it cannot express as factors", {
  # an overlapping 'Total' group (NE '') cannot share a factor with the arms
  tot <- .dg_plan("AG_TOT")
  expect_false(tot[[1]])
  expect_match(attr(tot, "blocked")[["AG_TOT"]], "AG_TOT_2 use a comparator other than EQ/IN")
  # groups conditioned on another variable than the grouping variable
  other <- .dg_plan("AG_OTHER")
  expect_match(attr(other, "blocked")[["AG_OTHER"]], "not on the grouping variable AGEGR1")
  # two groups whose levels coincide
  dup <- .dg_plan("AG_DUP")
  expect_match(attr(dup, "blocked")[["AG_DUP"]], "share the level 'M'")
  # a 'Total' group defined as IN over the arms passes the comparator and level
  # checks but overlaps the arms: a first-match case_when would leave it empty
  over <- .dg_plan("AG_OVER")
  expect_false(over[[1]])
  expect_match(attr(over, "blocked")[["AG_OVER"]],
               "Groups AG_OVER_1 and AG_OVER_3 overlap on the value 'Placebo'")
  # a group without a condition value defines no value set: reported as such,
  # not as two groups 'overlapping' on NA
  ag <- .dg_groupings()
  ag$group_condition_value[ag$group_id %in% c("AG_TRT_2", "AG_TRT_3")] <- NA
  novalue <- .dg_plan("AG_TRT", groupings = ag)
  expect_false(novalue[[1]])
  expect_match(attr(novalue, "blocked")[["AG_TRT"]],
               "Group(s) AG_TRT_2, AG_TRT_3 have no condition value", fixed = TRUE)
  # nothing is reported for derivable groupings
  expect_length(attr(.dg_plan("AG_TRT"), "blocked"), 0L)
})

test_that(".derived_grouping_plan treats a population slice like a data subset", {
  # an analysis set restricted on the grouping variable (e.g. treated subjects
  # = TRT01A IN the active arms) excludes groups on purpose
  expect_identical(as.vector(.dg_plan(c("AG_TRT", "AG_SEX"),
                                      subset_variables = "TRT01A")),
                   c(FALSE, TRUE))
})

test_that(".analysis_set_variables lists the variable an analysis set conditions on", {
  sets <- tibble::tibble(id = c("S1", "S2", "S3"),
                         condition_variable = c("SAFFL", "TRT01A", NA))
  expect_identical(siera:::.analysis_set_variables(sets, "S2"), "TRT01A")
  # a filter-less set, an unknown set, no set, or metadata without the column
  expect_identical(siera:::.analysis_set_variables(sets, "S3"), character(0))
  expect_identical(siera:::.analysis_set_variables(sets, "S9"), character(0))
  expect_identical(siera:::.analysis_set_variables(sets, NA_character_), character(0))
  expect_identical(siera:::.analysis_set_variables(NULL, "S1"), character(0))
  expect_identical(siera:::.analysis_set_variables(sets["id"], "S1"), character(0))
})

test_that("readARS keeps the observed values of a grouping its analysis set slices", {
  ars <- jsonlite::fromJSON(ARS_example("exampleARS_4.json"), simplifyVector = FALSE)
  # restrict the Safety Population to the two active arms (TRT01AN != 3): the
  # Placebo group is excluded by the population, so it must not come back as a
  # zero row
  ars$analysisSets[[1]]$condition <- list(dataset = "ADSL", variable = "TRT01AN",
                                          comparator = "NE", value = list("3"))
  dir <- withr::local_tempdir()
  path <- file.path(dir, "sliced.json")
  jsonlite::write_json(ars, path, auto_unbox = TRUE, null = "null", digits = NA)
  suppressWarnings(readARS(path, dir, dir, spec_output = "Out_01"))
  script <- readLines(file.path(dir, "ARD_Out_01.R"))
  expect_false(any(grepl("TRT01AN = factor(", script, fixed = TRUE)))
  # the sex grouping, which the population does not touch, is still derived
  expect_true(any(grepl("ASEX = factor(", script, fixed = TRUE)))

  # the unmodified example derives the treatment grouping
  dir2 <- withr::local_tempdir()
  suppressWarnings(readARS(ARS_example("exampleARS_4.json"), dir2, dir2,
                           spec_output = "Out_01"))
  expect_true(any(grepl("TRT01AN = factor(",
                        readLines(file.path(dir2, "ARD_Out_01.R")), fixed = TRUE)))
})

test_that(".warn_blocked_groupings warns once per grouping", {
  blocked <- c(AG_TOT = "Group(s) AG_TOT_2 use a comparator other than EQ/IN.")
  expect_warning(warned <- siera:::.warn_blocked_groupings(blocked),
                 "AG_TOT.*observed data values")
  expect_identical(warned, "AG_TOT")
  expect_no_warning(again <- siera:::.warn_blocked_groupings(blocked, warned))
  expect_identical(again, "AG_TOT")
  expect_no_warning(siera:::.warn_blocked_groupings(character(0)))
})

test_that(".generate_derived_grouping_code emits ARS-ordered factors", {
  ag <- .dg_groupings()
  expect_identical(
    siera:::.generate_derived_grouping_code("An_1", c("AG_TRT", "AG_SEX"),
                                            c(FALSE, FALSE), c("TRT01A", "SEX"), ag),
    ""
  )
  code <- siera:::.generate_derived_grouping_code(
    "An_1", c("AG_TRT", "AG_SEX"), c(TRUE, TRUE), c("TRT01A", "SEX"), ag
  )
  expect_match(code, "df2_An_1 <- df2_An_1 |>", fixed = TRUE)
  expect_match(code, "levels = c('Placebo', 'Low', 'High')", fixed = TRUE)
  expect_match(code, "SEX %in% c('F', 'X') ~ 'Female or other'", fixed = TRUE)
  expect_match(code, "levels = c('M', 'Female or other')", fixed = TRUE)
  expect_match(code, "dplyr::filter(!is.na(TRT01A), !is.na(SEX))", fixed = TRUE)

  # the generated block runs: undeclared values go, declared order is kept
  env <- new.env()
  env$df2_An_1 <- data.frame(TRT01A = c("High", "Placebo", "Other"),
                             SEX = c("X", "M", "M"))
  expect_no_warning(eval(parse(text = code), envir = env))
  expect_identical(levels(env$df2_An_1$TRT01A), c("Placebo", "Low", "High"))
  expect_identical(as.character(env$df2_An_1$SEX), c("Female or other", "M"))

  # a subset whose values match NO declared group would silently become a
  # table of zeros: the script warns (an empty subset stays silent)
  env$df2_An_1 <- data.frame(TRT01A = c("Placebo", "Low"), SEX = c("Male", "Female"))
  expect_warning(
    eval(parse(text = code), envir = env),
    "Analysis An_1: no record matches a declared group of AG_TRT, AG_SEX"
  )
  expect_identical(nrow(env$df2_An_1), 0L)
  env$df2_An_1 <- data.frame(TRT01A = character(0), SEX = character(0))
  expect_no_warning(eval(parse(text = code), envir = env))
})

test_that("a derived grouping keeps character-coded values that look numeric", {
  # groups EQ '01' / '02' on a character variable: compared as numbers ('01'
  # == 1) every record would fall outside the declared groups and be dropped
  ag <- tibble::tibble(
    id = "AG_CODE", groupingVariable = "TRT01A", dataDriven = "FALSE",
    group_id = c("AG_CODE_1", "AG_CODE_2", "AG_CODE_3", "AG_CODE_3"),
    group_name = c("Placebo", "Active", "Other", "Other"),
    group_order = c(1, 2, 3, 3),
    group_condition_variable = "TRT01A",
    group_condition_comparator = c("EQ", "EQ", "IN", "IN"),
    group_condition_value = c("01", "02", "3", "A")
  )
  code <- siera:::.generate_derived_grouping_code("An_1", "AG_CODE", TRUE,
                                                  "TRT01A", ag)
  expect_match(code, "TRT01A %in% c('01', 1) ~ '01'", fixed = TRUE)
  # a mixed list keeps every member
  expect_match(code, "TRT01A %in% c(3, 'A') ~ 'Other'", fixed = TRUE)

  # character-coded data keeps its records ...
  env <- new.env()
  env$df2_An_1 <- data.frame(TRT01A = c("01", "02", "02", "A", "3", "X"))
  eval(parse(text = code), envir = env)
  expect_identical(as.character(env$df2_An_1$TRT01A),
                   c("01", "02", "02", "Other", "Other"))
  # ... and so does numeric data whose ARS values carry a leading zero
  env$df2_An_1 <- data.frame(TRT01A = c(1, 2, 3, 7))
  eval(parse(text = code), envir = env)
  expect_identical(as.character(env$df2_An_1$TRT01A), c("01", "02", "Other"))
})

test_that(".grouping_stamp_spec stamps a derived grouping by its factor levels", {
  spec <- siera:::.grouping_stamp_spec("AG_SEX", "FALSE", .dg_groupings(),
                                       derived = TRUE)
  expect_identical(spec$group_ids, c("AG_SEX_1", "AG_SEX_2"))
  expect_identical(spec$values, c("M", "Female or other"))
})

test_that(".data_subset_variables lists a subset's condition variables", {
  ds <- tibble::tibble(id = c("D1", "D1", "D2"),
                       condition_variable = c("SAFFL", NA, "AESEV"))
  expect_identical(siera:::.data_subset_variables(ds, "D1"), "SAFFL")
  expect_identical(siera:::.data_subset_variables(ds, NA), character(0))
  expect_identical(siera:::.data_subset_variables(NULL, "D1"), character(0))
  expect_identical(siera:::.data_subset_variables(ds["id"], "D1"), character(0))
})

test_that("readARS validates predefined_zero_fill", {
  out <- withr::local_tempdir()
  for (bad in list(NA, "yes", c(TRUE, FALSE), NULL)) {
    expect_error(
      readARS(ARS_example("exampleARS_4.json"), out, out,
              predefined_zero_fill = bad),
      "predefined_zero_fill"
    )
  }
})

test_that("predefined_zero_fill = FALSE restores the raw-value scripts", {
  out_on <- withr::local_tempdir()
  out_off <- withr::local_tempdir()
  ars <- ARS_example("Common_Safety_Displays_cards.json")
  readARS(ars, out_on, out_on, spec_output = "Out14-1-1")
  readARS(ars, out_off, out_off, spec_output = "Out14-1-1",
          predefined_zero_fill = FALSE)
  on <- readLines(file.path(out_on, "ARD_Out14-1-1.R"))
  off <- readLines(file.path(out_off, "ARD_Out14-1-1.R"))
  expect_true(any(grepl("ARS-ordered factors", on, fixed = TRUE)))
  expect_false(any(grepl("ARS-ordered factors", off, fixed = TRUE)))
})

# End-to-end: exampleARS_4 rewritten so its sex grouping matches the bundled
# ADSL coding, with a declared-but-absent group ('U'), an IN group, and an
# IN treatment group (Placebo = TRT01AN 3 or 9) whose level is its name.
.dg_example_ars <- function() {
  ars <- jsonlite::fromJSON(ARS_example("exampleARS_4.json"),
                            simplifyVector = FALSE)
  for (i in seq_along(ars$analysisGroupings)) {
    g <- ars$analysisGroupings[[i]]
    if (g$id == "AnlsGrouping_02_Asex") {
      g$groups <- list(
        list(id = "Sex_M", name = "Male", order = 1L,
             condition = list(dataset = "ADSL", variable = "ASEX",
                              comparator = "EQ", value = list("M"))),
        list(id = "Sex_F", name = "Female or other", order = 2L,
             condition = list(dataset = "ADSL", variable = "ASEX",
                              comparator = "IN", value = list("F", "X"))),
        list(id = "Sex_U", name = "Undifferentiated", order = 3L,
             condition = list(dataset = "ADSL", variable = "ASEX",
                              comparator = "EQ", value = list("U")))
      )
    }
    if (g$id == "AnlsGrouping_01_Trt01An") {
      g$groups[[3]]$condition$comparator <- "IN"
      g$groups[[3]]$condition$value <- list("3", "9")
      g$groups[[3]]$name <- "Placebo"
    }
    ars$analysisGroupings[[i]] <- g
  }
  path <- file.path(withr::local_tempdir(.local_envir = parent.frame()),
                    "derived.json")
  jsonlite::write_json(ars, path, auto_unbox = TRUE, null = "null", digits = NA)
  path
}

test_that("declared groups are zero-filled, ordered and stamped at runtime (#203)", {
  skip_on_cran()
  skip_if_not_installed("cards")

  ars <- .dg_example_ars()
  adam_dir <- system.file("extdata", package = "siera")

  ards <- lapply(c("wrapped", "expanded"), function(style) {
    out <- withr::local_tempdir()
    # the age grouping's groups condition on AGEGR1, not on AGEGR1N
    expect_warning(
      readARS(ars, out, adam_dir, spec_output = "Out_01", code_style = style),
      "AnlsGrouping_03_Agegr1N"
    )
    e <- new.env(parent = baseenv())
    suppressWarnings(suppressPackageStartupMessages(
      source(file.path(out, "ARD_Out_01.R"), local = e, chdir = TRUE)
    ))
    e$ARD
  })
  ard <- ards[[1]]
  # both code styles stamp the derived groupings identically
  expect_identical(
    ards[[1]][, c("AnalysisId", "group1_level", "group1_groupId", "stat_name")],
    ards[[2]][, c("AnalysisId", "group1_level", "group1_groupId", "stat_name")]
  )

  # An_04: sex alone - every declared group, in declared order, the absent one
  # zero-filled; the IN group counts both of its values
  an04 <- ard[ard$AnalysisId == "An_04" & ard$stat_name == "n", ]
  expect_identical(as.character(an04$variable_level),
                   c("M", "Female or other", "U"))
  expect_equal(unlist(an04$stat), c(111, 143, 0))

  # An_03: treatment x sex - the IN treatment group is stamped by its name
  an03 <- ard[ard$AnalysisId == "An_03" & ard$stat_name == "n", ]
  expect_identical(unique(as.character(an03$group1_level)), c("1", "2", "Placebo"))
  expect_identical(
    unique(an03$group1_groupId),
    paste0("AnlsGrouping_01_Trt01An_0", 1:3)
  )
  # 3 arms x 3 declared sex groups, the undifferentiated column all zero
  expect_equal(nrow(an03), 9L)
  expect_true(all(unlist(an03$stat[an03$variable_level == "U"]) == 0))
})

# Empty data subsets (#203): an analysis with a declared-group factor runs its
# method even when no record qualifies, so the declared groups come out as zero
# rows; a method that cannot compute on 0 rows falls back to the stub row.
.dg_method_code <- function(template, code_style = "wrapped",
                            always_compute = TRUE) {
  siera:::.generate_analysis_method_section(
    analysis_methods = tibble::tibble(id = "M1", name = "m", description = "d",
                                      label = "l", operation_id = "M1_n"),
    analysis_method_code_template = .dg_template(template),
    analysis_method_code_parameters = tibble::tibble(
      method_id = character(0), parameter_name = character(0),
      parameter_valueSource = character(0)),
    method_id = "M1", analysis_id = "An_1", output_id = "Out_1",
    code_style = code_style, always_compute = always_compute
  )
}

.dg_run_method <- function(template, df2, code_style = "wrapped") {
  res <- .dg_method_code(template, code_style)
  env <- new.env()
  env$df2_An_1 <- df2
  eval(parse(text = res$code), envir = env)
  env$df3_An_1
}

test_that("always_compute drops the empty-data guard and adds the stub fallback", {
  res <- .dg_method_code("df3_analysisidhere <- data.frame(x = 1)")
  expect_true(res$always_compute)
  expect_false(grepl("if (nrow(df2_An_1) != 0) {", res$code, fixed = TRUE))
  expect_match(res$code, "df3_An_1 <- tryCatch({", fixed = TRUE)

  off <- .dg_method_code("df3_analysisidhere <- data.frame(x = 1)",
                         always_compute = FALSE)
  expect_false(off$always_compute)
  expect_match(off$code, "if (nrow(df2_An_1) != 0) {", fixed = TRUE)

  # a population-based template already always computes, without the fallback
  pop <- .dg_method_code("df3_analysisidhere <- df_poptot")
  expect_false(pop$always_compute)
  expect_false(grepl("tryCatch", pop$code, fixed = TRUE))
})

test_that("an always-computed method reports zero rows on an empty subset", {
  skip_if_not_installed("cards")
  tpl <- "df3_analysisidhere <- cards::ard_tabulate(df2_analysisidhere, variables = 'SEX')"
  empty <- data.frame(SEX = factor(character(0), levels = c("M", "F")))
  for (style in c("wrapped", "expanded")) {
    df3 <- .dg_run_method(tpl, empty, style)
    n <- df3[df3$stat_name == "n", ]
    expect_identical(vapply(n$variable_level, as.character, ""), c("M", "F"))
    expect_equal(unlist(n$stat), c(0, 0))
  }
})

test_that("a method that cannot compute on 0 rows falls back to the stub", {
  failing <- "stop('cannot compute')\ndf3_analysisidhere <- data.frame(x = 1)"
  empty <- data.frame(SEX = character(0))
  # the swallowed error stays visible as a warning naming the analysis, the
  # method and the cause, so a defective template cannot pass as a stub
  expect_warning(
    expect_null(.dg_run_method(failing, empty, "wrapped")),
    "Analysis An_1: method M1 failed on an empty data subset \\(cannot compute\\)"
  )
  expect_warning(stub <- .dg_run_method(failing, empty, "expanded"),
                 "emitting the stub row")
  expect_identical(names(stub), c("AnalysisId", "MethodId", "OutputId"))

  # an empty result on an empty subset is a stub too, silently: the template
  # ran and simply found nothing
  expect_no_warning(
    expect_null(.dg_run_method("df3_analysisidhere <- data.frame(x = numeric(0))",
                               empty, "wrapped"))
  )

  # with data, a failing method is a real error and is not hidden
  expect_error(.dg_run_method(failing, data.frame(SEX = "M"), "wrapped"),
               "cannot compute")
})

test_that("the expanded group stamp skips the stub of an always-computed method", {
  code <- siera:::.generate_groupid_code(
    analysis_id = "An_1", groupids = "AG_TRT", n_group_cols = 1L,
    AG_dataDriven = "FALSE", analysis_groupings = .dg_groupings(),
    stamp_derived = TRUE, always_compute = TRUE
  )
  expect_match(code, "if('group1_level' %in% names(df3_An_1)){", fixed = TRUE)
  env <- new.env()
  env$df3_An_1 <- data.frame(AnalysisId = "An_1")
  eval(parse(text = code), envir = env)
  expect_identical(names(env$df3_An_1), "AnalysisId")
  env$df3_An_1 <- data.frame(group1_level = "Placebo")
  eval(parse(text = code), envir = env)
  expect_identical(env$df3_An_1$group1_groupId, "AG_TRT_1")
})
