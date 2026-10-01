# ars_grouping() ----------------------------------------------------------

test_that("ars_grouping() builds a siera_ars_grouping for a pre-defined grouping", {
  g <- ars_grouping(
    "AG_Trt",
    groups = c("AG_Trt_1" = "Placebo", "AG_Trt_2" = "Low")
  )

  expect_s3_class(g, "siera_ars_grouping")
  expect_identical(g$id, "AG_Trt")
  expect_identical(g$groups, c("AG_Trt_1" = "Placebo", "AG_Trt_2" = "Low"))
  expect_false(g$data_driven)
})

test_that("ars_grouping() builds data-driven and group-less groupings", {
  dd <- ars_grouping("AG_Soc", data_driven = TRUE)
  expect_true(dd$data_driven)
  expect_length(dd$groups, 0L)

  # `groups = NULL` (the default) and character(0) both mean "no groups"
  expect_length(ars_grouping("AG_X")$groups, 0L)
  expect_length(ars_grouping("AG_X", groups = character(0))$groups, 0L)
})

test_that("ars_grouping() validates its arguments", {
  expect_error(ars_grouping(1), "must be a single, non-empty string")
  expect_error(ars_grouping(c("a", "b")), "must be a single, non-empty string")
  expect_error(ars_grouping(NA_character_), "must be a single, non-empty string")
  expect_error(ars_grouping(""), "must be a single, non-empty string")

  expect_error(ars_grouping("a", data_driven = "yes"), "single")
  expect_error(ars_grouping("a", data_driven = c(TRUE, FALSE)), "single")
  expect_error(ars_grouping("a", data_driven = NA), "single")

  expect_error(ars_grouping("a", groups = 1:2), "named")
  # unnamed
  expect_error(ars_grouping("a", groups = c("x", "y")), "must be named")
  # partially named / empty name
  expect_error(ars_grouping("a", groups = c(g1 = "x", "y")), "must be named")
  # NA name
  nm <- c("x", "y")
  names(nm) <- c("g1", NA)
  expect_error(ars_grouping("a", groups = nm), "must be named")

  expect_error(
    ars_grouping("a", groups = c(g1 = "x"), data_driven = TRUE),
    "data-driven grouping cannot define"
  )
})

# ars_stamp() -------------------------------------------------------------

.stamp_ard <- function() {
  tibble::tibble(
    group1_level = c("Placebo", "Low", "Other"),
    variable_level = list(NULL, NULL, NULL),
    stat_name = "n",
    stat = list(86L, 84L, 1L)
  )
}

trt <- ars_grouping(
  "AG_Trt",
  groups = c("AG_Trt_1" = "Placebo", "AG_Trt_2" = "Low")
)

test_that("ars_stamp() returns a data.frame stub for a NULL ARD", {
  out <- ars_stamp(NULL, "An_1", "Mth_1", "Out_1")
  expect_identical(
    out,
    data.frame(AnalysisId = "An_1", MethodId = "Mth_1", OutputId = "Out_1")
  )
  expect_false(tibble::is_tibble(out))

  # groupings are ignored for the stub (no group columns), as in expanded style
  out2 <- ars_stamp(NULL, "An_1", "Mth_1", "Out_1", groupings = list(trt))
  expect_identical(out2, out)
})

test_that("ars_stamp() adds identifier columns after the existing ones", {
  ard <- tibble::tibble(stat_name = "n", stat = list(1L))
  out <- ars_stamp(ard, "An_1", "Mth_1", "Out_1")

  expect_identical(
    names(out),
    c("stat_name", "stat", "AnalysisId", "MethodId", "OutputId")
  )
  expect_identical(out$AnalysisId, "An_1")
  expect_identical(out$MethodId, "Mth_1")
  expect_identical(out$OutputId, "Out_1")
  # no groupings argument: same as an empty list or NULL
  expect_identical(out, ars_stamp(ard, "An_1", "Mth_1", "Out_1", groupings = list()))
  expect_identical(out, ars_stamp(ard, "An_1", "Mth_1", "Out_1", groupings = NULL))
})

test_that("ars_stamp() maps pre-defined groups and leaves unmatched levels NA", {
  out <- ars_stamp(.stamp_ard(), "An_1", "Mth_1", "Out_1", groupings = list(trt))

  expect_identical(
    names(out)[-seq_len(4)],
    c("AnalysisId", "MethodId", "OutputId", "group1_groupingId", "group1_groupId")
  )
  expect_identical(out$group1_groupingId, rep("AG_Trt", 3))
  expect_identical(out$group1_groupId, c("AG_Trt_1", "AG_Trt_2", NA_character_))
})

test_that("ars_stamp() takes the first defined group on duplicate values and supports IN groups", {
  g <- ars_grouping(
    "AG_Sev",
    groups = c(
      "AG_Sev_1" = "MILD",
      "AG_Sev_2" = "MODERATE",
      "AG_Sev_2" = "SEVERE",  # IN condition: group id repeated per value
      "AG_Sev_3" = "MILD"     # duplicate value: first match wins
    )
  )
  ard <- tibble::tibble(group1_level = c("MILD", "SEVERE", "MODERATE", "x"))
  out <- ars_stamp(ard, "An_1", "Mth_1", "Out_1", groupings = list(g))
  expect_identical(
    out$group1_groupId,
    c("AG_Sev_1", "AG_Sev_2", "AG_Sev_2", NA_character_)
  )
})

test_that("ars_stamp() stamps groupValue for data-driven groupings", {
  g <- ars_grouping("AG_Soc", data_driven = TRUE)
  ard <- tibble::tibble(group1_level = list("A", "B"))
  out <- ars_stamp(ard, "An_1", "Mth_1", "Out_1", groupings = list(g))

  expect_identical(
    names(out)[-1],
    c("AnalysisId", "MethodId", "OutputId", "group1_groupingId",
      "group1_groupId", "group1_groupValue")
  )
  expect_identical(out$group1_groupId, c(NA_character_, NA_character_))
  expect_identical(out$group1_groupValue, c("A", "B"))
})

test_that("ars_stamp() leaves groupId NA for a pre-defined grouping without groups", {
  ard <- tibble::tibble(group1_level = c("a", "b"))
  out <- ars_stamp(ard, "An_1", "Mth_1", "Out_1", groupings = list(ars_grouping("AG_X")))
  expect_identical(out$group1_groupingId, c("AG_X", "AG_X"))
  expect_identical(out$group1_groupId, c(NA_character_, NA_character_))
  expect_false("group1_groupValue" %in% names(out))
})

test_that("ars_stamp() numbers groupings by their position in the list", {
  ard <- tibble::tibble(
    group1_level = c("Placebo", "Low"),
    group2_level = c("a", "b")
  )
  out <- ars_stamp(
    ard, "An_1", "Mth_1", "Out_1",
    groupings = list(trt, ars_grouping("AG_Inner", data_driven = TRUE))
  )
  expect_identical(
    names(out)[-(1:2)],
    c("AnalysisId", "MethodId", "OutputId",
      "group1_groupingId", "group1_groupId",
      "group2_groupingId", "group2_groupId", "group2_groupValue")
  )
  expect_identical(out$group2_groupingId, c("AG_Inner", "AG_Inner"))
  expect_identical(out$group2_groupValue, c("a", "b"))
})

test_that("ars_stamp() coerces *_level list columns to character and leaves stat alone", {
  out <- ars_stamp(.stamp_ard(), "An_1", "Mth_1", "Out_1")

  # list-of-NULLs become NA_character_
  expect_identical(out$variable_level, rep(NA_character_, 3))
  expect_type(out$group1_level, "character")
  # the numeric stat list column is untouched
  expect_type(out$stat, "list")
  expect_identical(out$stat, list(86L, 84L, 1L))
})

test_that("ars_stamp() handles a zero-row ARD", {
  ard <- tibble::tibble(group1_level = character(0), stat = list())
  out <- ars_stamp(ard, "An_1", "Mth_1", "Out_1", groupings = list(trt))
  expect_identical(nrow(out), 0L)
  expect_true(all(
    c("AnalysisId", "MethodId", "OutputId", "group1_groupingId", "group1_groupId") %in%
      names(out)
  ))
})

test_that("ars_stamp() aborts when a group<n>_level column is missing", {
  ard <- tibble::tibble(stat = list(1L))
  expect_error(
    ars_stamp(ard, "An_1", "Mth_1", "Out_1", groupings = list(trt)),
    "group1_level"
  )
  # second grouping needs group2_level
  ard2 <- tibble::tibble(group1_level = "Placebo")
  expect_error(
    ars_stamp(ard2, "An_1", "Mth_1", "Out_1", groupings = list(trt, trt)),
    "group2_level"
  )
})

test_that("ars_stamp() validates its arguments", {
  ard <- tibble::tibble(stat = list(1L))
  expect_error(ars_stamp("nope", "a", "m", "o"), "must be a data frame")
  expect_error(ars_stamp(ard, 1, "m", "o"), "analysis_id")
  expect_error(ars_stamp(ard, "a", NA_character_, "o"), "method_id")
  expect_error(ars_stamp(ard, "a", "m", c("o", "p")), "output_id")
  expect_error(ars_stamp(ard, "a", "m", ""), "output_id")

  # a bare grouping must be wrapped in list()
  expect_error(
    ars_stamp(ard, "a", "m", "o", groupings = trt),
    "Wrap a single grouping"
  )
  expect_error(ars_stamp(ard, "a", "m", "o", groupings = "x"), "list")
  expect_error(ars_stamp(ard, "a", "m", "o", groupings = list("x")), "list")
})

# Equivalence with the expanded generator ---------------------------------

test_that("ars_stamp() reproduces the expanded ID-linking code exactly", {
  # IN-style repeated group ids, an unmatched level, a value with a quote, and
  # a data-driven second grouping: both generators must stamp the same ARD.
  groupings_tbl <- tibble::tibble(
    id = c("AG_A", "AG_A", "AG_A", "AG_B"),
    group_id = c("AG_A_1", "AG_A_2", "AG_A_2", NA),
    group_condition_value = c("Pl'acebo", "Low", "High", NA),
    dataDriven = c("FALSE", "FALSE", "FALSE", "TRUE")
  )
  ard <- tibble::tibble(
    group1_level = list("Pl'acebo", "Low", "High", "unmatched"),
    group2_level = list("x", "y", "z", "w"),
    variable_level = list(NULL, "a", NULL, "b"),
    stat_name = "n",
    stat = list(1, 2, 3, 4)
  )

  expanded_code <- paste0(
    .generate_groupid_code(
      analysis_id = "An_1",
      groupids = c("AG_A", "AG_B"),
      n_group_cols = 2L,
      AG_dataDriven = c("FALSE", "TRUE"),
      analysis_groupings = groupings_tbl,
      population_based = TRUE
    ),
    "df3_An_1 <- df3_An_1 |>\n",
    "  dplyr::mutate(dplyr::across(\n",
    "    dplyr::matches('_level$'),\n",
    "    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))\n",
    "  ))\n"
  )
  env <- new.env(parent = baseenv())
  env$df3_An_1 <- dplyr::mutate(
    ard, AnalysisId = "An_1", MethodId = "Mth_1", OutputId = "Out_1"
  )
  eval(parse(text = expanded_code), envir = env)

  wrapped_code <- .generate_stamp_code(
    analysis_id = "An_1", method_id = "Mth_1", output_id = "Out_1",
    groupids = c("AG_A", "AG_B"), n_group_cols = 2L,
    AG_dataDriven = c("FALSE", "TRUE"), analysis_groupings = groupings_tbl
  )
  env2 <- new.env(parent = asNamespace("siera"))
  env2$df3_An_1 <- ard
  eval(parse(text = wrapped_code), envir = env2)

  expect_identical(env2$df3_An_1, env$df3_An_1)
  expect_identical(env2$df3_An_1$group1_groupId[1], "AG_A_1")
})

# .generate_stamp_code() --------------------------------------------------

test_that(".generate_stamp_code() emits a traceable ars_stamp() call", {
  groupings_tbl <- tibble::tibble(
    id = c("AG_Trt", "AG_Trt"),
    group_id = c("AG_Trt_1", "AG_Trt_2"),
    group_condition_value = c("Placebo", "Xanomeline Low Dose"),
    dataDriven = c("FALSE", "FALSE")
  )
  code <- .generate_stamp_code(
    analysis_id = "An_1", method_id = "Mth_1", output_id = "Out_1",
    groupids = "AG_Trt", n_group_cols = 1L, AG_dataDriven = "FALSE",
    analysis_groupings = groupings_tbl
  )

  expect_silent(parse(text = code))
  expect_match(code, "df3_An_1 <- siera::ars_stamp(", fixed = TRUE)
  expect_match(code, "analysis_id = 'An_1'", fixed = TRUE)
  expect_match(code, "# analyses[].id", fixed = TRUE)
  expect_match(code, "# analyses[].methodId", fixed = TRUE)
  expect_match(code, "# mainListOfContents outputId", fixed = TRUE)
  expect_match(code, "# analyses[].orderedGroupings", fixed = TRUE)
  expect_match(code, "siera::ars_grouping('AG_Trt', groups = c(", fixed = TRUE)
  expect_match(code, "'AG_Trt_2' = 'Xanomeline Low Dose'", fixed = TRUE)
  expect_false(grepl("case_when|vapply", code))
})

test_that(".generate_stamp_code() omits groupings when the method adds no group columns", {
  code <- .generate_stamp_code(
    analysis_id = "An_1", method_id = "Mth_1", output_id = "Out_1",
    groupids = "AG_Trt", n_group_cols = 0L, AG_dataDriven = "FALSE",
    analysis_groupings = tibble::tibble()
  )
  expect_silent(parse(text = code))
  expect_false(grepl("groupings", code, fixed = TRUE))
  # no trailing comma after the last argument
  expect_match(code, "output_id   = 'Out_1'  *# mainListOfContents outputId")
})

test_that(".generate_stamp_code() covers data-driven, group-less and quoted values", {
  groupings_tbl <- tibble::tibble(
    id = c("AG_Q", "AG_Q"),
    group_id = c("AG_Q_1", "AG_Q_2"),
    group_condition_value = c("it's", "plain"),
    dataDriven = c("FALSE", "FALSE")
  )
  code <- .generate_stamp_code(
    analysis_id = "An_1", method_id = "Mth'1", output_id = "Out_1",
    groupids = c("AG_Q", "AG_DD", "AG_NONE"), n_group_cols = 3L,
    AG_dataDriven = c("FALSE", "TRUE", "FALSE"),
    analysis_groupings = groupings_tbl
  )
  expect_silent(parse(text = code))
  expect_match(code, "'AG_Q_1' = 'it\\'s'", fixed = TRUE)
  expect_match(code, "method_id   = 'Mth\\'1'", fixed = TRUE)
  expect_match(code, "siera::ars_grouping('AG_DD', data_driven = TRUE)", fixed = TRUE)
  expect_match(code, "siera::ars_grouping('AG_NONE')", fixed = TRUE)
})

# Wrapped method section --------------------------------------------------

.wrapped_method_fixture <- function(template_code) {
  list(
    methods = tibble::tibble(
      id = "MTH1", name = "Method one", description = "Desc",
      label = "m1", operation_id = c("OP1", "OP2")
    ),
    template = tibble::tibble(
      method_id = "MTH1", context = "R (siera)", specifiedAs = "Code",
      templateCode = template_code
    ),
    parameters = tibble::tibble(
      method_id = "MTH1", parameter_name = "opid1here",
      parameter_valueSource = "operation_1"
    )
  )
}

test_that("wrapped method section guards the call and leaves stamping to ars_stamp()", {
  fx <- .wrapped_method_fixture(
    "df3_analysisidhere <- cards::ard_summary(df2_analysisidhere) |> dplyr::mutate(operationid = 'opid1here')"
  )
  res <- .generate_analysis_method_section(
    fx$methods, fx$template, fx$parameters,
    method_id = "MTH1", analysis_id = "AN1", output_id = "OUT1",
    code_style = "wrapped"
  )

  expect_false(res$population_based)
  expect_match(res$code, "df3_AN1 <- NULL\nif (nrow(df2_AN1) != 0) {", fixed = TRUE)
  expect_match(res$code, "operationid = 'OP1'", fixed = TRUE)
  # no duplicate marker, no inline stamp / stub
  expect_false(grepl("#Apply Method", res$code, fixed = TRUE))
  expect_false(grepl("AnalysisId", res$code, fixed = TRUE))
  expect_false(grepl("data.frame(", res$code, fixed = TRUE))
  expect_match(res$code, "Method ID:\\s+MTH1")
  expect_equal(res$operations, list(operation_1 = "OP1", operation_2 = "OP2"))
  expect_silent(parse(text = res$code))
})

test_that("wrapped method section drops the empty-data guard for population-based methods", {
  fx <- .wrapped_method_fixture(
    "df3_analysisidhere <- stats::prop.test(df_poptot$x) # opid1here"
  )
  res <- .generate_analysis_method_section(
    fx$methods, fx$template, fx$parameters,
    method_id = "MTH1", analysis_id = "AN1", output_id = "OUT1",
    code_style = "wrapped"
  )

  expect_true(res$population_based)
  expect_false(grepl("nrow(df2_AN1)", res$code, fixed = TRUE))
  expect_false(grepl("<- NULL", res$code, fixed = TRUE))
  expect_match(res$code, "df3_AN1 <- stats::prop.test(df_poptot$x) # OP1", fixed = TRUE)
})

test_that("expanded stays the default for .generate_analysis_method_section()", {
  fx <- .wrapped_method_fixture("df3_analysisidhere <- df2_analysisidhere")
  res <- .generate_analysis_method_section(
    fx$methods, fx$template, fx$parameters,
    method_id = "MTH1", analysis_id = "AN1", output_id = "OUT1"
  )
  expect_match(res$code, "#Apply Method", fixed = TRUE)
  expect_match(res$code, "OutputId = 'OUT1'", fixed = TRUE)

  expect_error(
    .generate_analysis_method_section(
      fx$methods, fx$template, fx$parameters,
      method_id = "MTH1", analysis_id = "AN1", output_id = "OUT1",
      code_style = "nope"
    ),
    "should be one of"
  )
})

# readARS(code_style = ) --------------------------------------------------

test_that("readARS() validates code_style strictly", {
  ars <- ARS_example("exampleARS_6.json")
  out <- withr::local_tempdir()

  expect_error(readARS(ars, out, code_style = "nope"), "must be one of")
  expect_error(readARS(ars, out, code_style = c("wrapped", "expanded")), "must be one of")
  # partial matching is not allowed
  expect_error(readARS(ars, out, code_style = "wrap"), "must be one of")
  expect_error(readARS(ars, out, code_style = 1), "must be one of")
})

test_that("readARS() defaults code_style to wrapped (also for NULL)", {
  ars <- ARS_example("exampleARS_6.json")
  adam <- withr::local_tempdir()

  out_default <- withr::local_tempdir()
  readARS(ars, out_default, adam)
  out_null <- withr::local_tempdir()
  readARS(ars, out_null, adam, code_style = NULL)

  strip <- function(f) {
    x <- readLines(f, warn = FALSE)
    x[!grepl("Date created", x, fixed = TRUE)]
  }
  expect_identical(
    strip(file.path(out_default, "ARD_Out_01.R")),
    strip(file.path(out_null, "ARD_Out_01.R"))
  )
  expect_true(any(grepl("siera::ars_stamp(", readLines(file.path(out_default, "ARD_Out_01.R")), fixed = TRUE)))
})

test_that("wrapped scripts call ars_stamp() and omit the expanded boilerplate; both ARS formats", {
  for (f in c("exampleARS_6.json", "exampleARS_6.xlsx")) {
    adam <- withr::local_tempdir()
    out_w <- withr::local_tempdir()
    out_e <- withr::local_tempdir()
    readARS(ARS_example(f), out_w, adam, code_style = "wrapped")
    readARS(ARS_example(f), out_e, adam, code_style = "expanded")

    wrapped <- paste(readLines(file.path(out_w, "ARD_Out_01.R"), warn = FALSE), collapse = "\n")
    expanded <- paste(readLines(file.path(out_e, "ARD_Out_01.R"), warn = FALSE), collapse = "\n")

    expect_match(wrapped, "siera::ars_stamp(", fixed = TRUE)
    expect_match(wrapped, "siera::ars_grouping(", fixed = TRUE)
    expect_match(wrapped, "<- NULL\nif (nrow(df2_", fixed = TRUE)
    expect_false(grepl("case_when(\n        as.character(group", wrapped, fixed = TRUE))
    expect_false(grepl("OutputId = ", wrapped, fixed = TRUE))
    expect_false(grepl("matches('_level$')", wrapped, fixed = TRUE))
    expect_false(grepl("siera::ars_stamp", expanded, fixed = TRUE))
    expect_match(expanded, "OutputId = ", fixed = TRUE)

    # the "Apply Method" marker appears once per analysis in wrapped style
    n_analyses <- lengths(regmatches(wrapped, gregexpr("# Analysis ", wrapped, fixed = TRUE)))
    n_markers <- lengths(regmatches(wrapped, gregexpr("#Apply Method ---", wrapped, fixed = TRUE)))
    expect_identical(n_markers, n_analyses)
    expect_silent(parse(text = wrapped))
  }
})
