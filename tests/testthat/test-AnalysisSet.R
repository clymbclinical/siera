# Tests for .generate_population_code() / .resolve_analysis_set() (#197):
# populations are derived per analysis from metadata, never from the position
# of an analysis within its output.

pop_code <- function(...) {
  siera:::`.generate_population_code`(...)
}

expect_code_lines <- function(code, expected) {
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  if (length(lines) > 0 && lines[[1]] == "") {
    lines <- lines[-1]
  }
  testthat::expect_equal(lines, expected)
}

saf_set <- function(id = "AS1", dataset = "ADSL", variable = "SAFFL",
                    comparator = "EQ", value = "Y", name = "Safety") {
  tibble::tibble(
    id = id,
    condition_dataset = dataset,
    condition_variable = variable,
    condition_comparator = comparator,
    condition_value = value,
    name = name
  )
}

trt_grouping <- tibble::tibble(
  id = c("AG_TRT", "AG_SOC", "AG_PARAM"),
  groupingDataset = c("ADSL", "ADAE", "ADLB")
)

subsets <- tibble::tibble(
  id = c("DS_TEAE", "DS_LB"),
  condition_dataset = c("ADAE", "ADLB")
)

anas_for <- function(ids) tibble::tibble(listItem_analysisId = ids)

test_that("single set on the analysis dataset: one filter, subject-level frames", {
  analyses <- tibble::tibble(
    id = c("AN1", "AN2"),
    dataset = c("ADSL", "ADSL"),
    variable = c("USUBJID", "AGE"),
    analysisSetId = "AS1",
    groupingId1 = "AG_TRT",
    dataSubsetId = NA_character_
  )

  res <- pop_code(anas_for(c("AN1", "AN2")), analyses, saf_set(), trt_grouping, subsets)

  expect_code_lines(res$code, c(
    "# Apply Analysis Set ---",
    "df_pop <- dplyr::filter(ADSL,",
    "            SAFFL == 'Y')",
    "df_poptot <- df_pop"
  ))
  expect_equal(res$frames, c(AN1 = "df_poptot", AN2 = "df_poptot"))
  expect_equal(res$poptot, c(AN1 = "df_poptot", AN2 = "df_poptot"))
})

test_that("content analyses merge, while a bigN declared on ADAE stays subject-level", {
  # In the eTFL AE tables the bigN analysis declares dataset = ADAE with
  # variable USUBJID; it must still count every subject in the population.
  analyses <- tibble::tibble(
    id = c("BIGN", "TEAE", "SOC"),
    dataset = "ADAE",
    variable = "USUBJID",
    analysisSetId = "AS1",
    groupingId1 = "AG_TRT",
    groupingId2 = c(NA, NA, "AG_SOC"),
    dataSubsetId = c(NA, "DS_TEAE", NA)
  )

  res <- pop_code(anas_for(analyses$id), analyses, saf_set(), trt_grouping, subsets)

  expect_code_lines(res$code, c(
    "# Apply Analysis Set ---",
    "overlap <- intersect(names(ADSL), names(ADAE))",
    "overlapfin <- setdiff(overlap, 'USUBJID')",
    "df_pop <- dplyr::filter(ADSL,",
    "            SAFFL == 'Y') |>",
    "            merge(ADAE |> dplyr::select(-dplyr::all_of(overlapfin)),",
    "                  by = 'USUBJID',",
    "                  all = FALSE)",
    "df_poptot = dplyr::filter(ADSL,",
    "            SAFFL == 'Y')"
  ))
  # subset on ADAE and grouping on ADAE both need the merged frame
  expect_equal(res$frames, c(BIGN = "df_poptot", TEAE = "df_pop", SOC = "df_pop"))
})

test_that("a second bigN anywhere in the output gets the subject-level frame", {
  # e.g. a Total treatment column: two population counts, not by position
  analyses <- tibble::tibble(
    id = c("TEAE", "BIGN", "BIGN_TOT"),
    dataset = "ADAE",
    variable = "USUBJID",
    analysisSetId = "AS1",
    groupingId1 = c("AG_TRT", "AG_TRT", NA),
    dataSubsetId = c("DS_TEAE", NA, NA)
  )

  res <- pop_code(anas_for(analyses$id), analyses, saf_set(), trt_grouping, subsets)

  expect_equal(
    res$frames,
    c(TEAE = "df_pop", BIGN = "df_poptot", BIGN_TOT = "df_poptot")
  )
})

test_that("a non-USUBJID variable on a content dataset needs the merged frame", {
  analyses <- tibble::tibble(
    id = c("BIGN", "AVAL"),
    dataset = c("ADSL", "ADEXSUM"),
    variable = c("USUBJID", "AVAL"),
    analysisSetId = "AS1",
    groupingId1 = "AG_TRT",
    dataSubsetId = NA_character_
  )

  res <- pop_code(anas_for(analyses$id), analyses, saf_set(), trt_grouping, subsets)

  expect_match(res$code, "merge(ADEXSUM", fixed = TRUE)
  expect_equal(res$frames, c(BIGN = "df_poptot", AVAL = "df_pop"))
})

test_that("one set merged onto two content datasets gets one frame per dataset", {
  analyses <- tibble::tibble(
    id = c("BIGN", "AE", "LB"),
    dataset = c("ADSL", "ADAE", "ADLB"),
    variable = "USUBJID",
    analysisSetId = "AS1",
    groupingId1 = "AG_TRT",
    dataSubsetId = c(NA, "DS_TEAE", "DS_LB")
  )

  res <- pop_code(anas_for(analyses$id), analyses, saf_set(), trt_grouping, subsets)

  expect_equal(res$frames, c(BIGN = "df_poptot", AE = "df_pop__ADAE", LB = "df_pop__ADLB"))
  expect_match(res$code, "df_pop__ADAE <- dplyr::filter(ADSL,", fixed = TRUE)
  expect_match(res$code, "merge(ADAE |>", fixed = TRUE)
  expect_match(res$code, "df_pop__ADLB <- dplyr::filter(ADSL,", fixed = TRUE)
  expect_match(res$code, "merge(ADLB |>", fixed = TRUE)
  expect_match(res$code, "df_poptot = dplyr::filter(ADSL,", fixed = TRUE)
})

test_that("an analysis touching two content datasets aborts", {
  analyses <- tibble::tibble(
    id = "AN1",
    dataset = "ADAE",
    variable = "USUBJID",
    analysisSetId = "AS1",
    groupingId1 = "AG_PARAM",
    dataSubsetId = "DS_TEAE"
  )

  expect_error(
    pop_code(anas_for("AN1"), analyses, saf_set(), trt_grouping, subsets),
    "more than one dataset"
  )
})

test_that("each analysis set gets its own suffixed frames", {
  sets <- dplyr::bind_rows(
    saf_set("AS_ALL", variable = "ARM", comparator = "NE", value = "",
            name = "  All Subjects\n[1]"),
    saf_set("AS_PP", variable = "PPROTFL", name = NA_character_)
  )
  analyses <- tibble::tibble(
    id = c("AN1", "AN2", "AN3"),
    dataset = "ADSL",
    variable = "USUBJID",
    analysisSetId = c("AS_ALL", "AS_PP", "AS_ALL"),
    groupingId1 = "AG_TRT",
    dataSubsetId = NA_character_
  )

  res <- pop_code(anas_for(analyses$id), analyses, sets, trt_grouping, subsets)

  expect_code_lines(res$code, c(
    "# Apply Analysis Set ---",
    "# Analysis set: AS_ALL - All Subjects [1]",
    "df_pop__AS_ALL <- dplyr::filter(ADSL,",
    "            ARM != '')",
    "df_poptot__AS_ALL <- df_pop__AS_ALL",
    "",
    "# Analysis set: AS_PP",
    "df_pop__AS_PP <- dplyr::filter(ADSL,",
    "            PPROTFL == 'Y')",
    "df_poptot__AS_PP <- df_pop__AS_PP"
  ))
  expect_equal(
    res$frames,
    c(AN1 = "df_poptot__AS_ALL", AN2 = "df_poptot__AS_PP", AN3 = "df_poptot__AS_ALL")
  )
  expect_equal(res$poptot, res$frames)
})

test_that("a missing analysis set id warns and falls back to the unfiltered dataset", {
  analyses <- tibble::tibble(id = "AN1", dataset = "ADSL")

  expect_warning(
    res <- pop_code(anas_for("AN1"), analyses, saf_set()),
    "missing an analysisSetId"
  )
  # historical fallback layout: no leading blank line
  expect_equal(res$code, "# Apply Analysis Set ---\ndf_pop <- ADSL\ndf_poptot <- df_pop\n")
  expect_equal(res$frames, c(AN1 = "df_poptot"))
})

test_that("a set-less analysis alongside a real set is named 'unspecified'", {
  analyses <- tibble::tibble(
    id = c("AN1", "AN2"),
    dataset = c("ADSL", "ADAE"),
    variable = c("USUBJID", "AETERM"),
    analysisSetId = c("AS1", NA)
  )

  expect_warning(
    res <- pop_code(anas_for(analyses$id), analyses, saf_set()),
    "AN2: Analysis is missing an analysisSetId"
  )
  expect_match(res$code, "# Analysis set: (none)", fixed = TRUE)
  # unfiltered fallback populations are never merged
  expect_match(res$code, "df_pop__unspecified <- ADAE", fixed = TRUE)
  expect_equal(res$frames, c(AN1 = "df_poptot__AS1", AN2 = "df_poptot__unspecified"))
})

test_that("an analysis absent from the Analyses metadata falls back without a dataset", {
  expect_warning(
    res <- pop_code(anas_for("GHOST"), tibble::tibble(id = "AN1", dataset = "ADSL"), saf_set()),
    "missing an analysisSetId"
  )
  expect_match(res$code, "df_pop <- df_pop", fixed = TRUE)
})

test_that("absent AnalysisSets metadata warns and falls back", {
  analyses <- tibble::tibble(id = "AN1", dataset = "ADSL", analysisSetId = "AS1")
  expect_warning(
    res <- pop_code(anas_for("AN1"), analyses, NULL),
    "AnalysisSets metadata not supplied"
  )
  expect_match(res$code, "df_pop <- ADSL", fixed = TRUE)
})

test_that("an unknown analysis set id warns and falls back", {
  analyses <- tibble::tibble(id = "AN1", dataset = "ADSL", analysisSetId = "AS_NOPE")
  expect_warning(
    pop_code(anas_for("AN1"), analyses, saf_set()),
    "AnalysisSet AS_NOPE not found"
  )
})

test_that("missing analysis set components warn the user and fall back", {
  analyses <- tibble::tibble(id = "AN1", dataset = "ADSL", analysisSetId = "AS1")
  expect_warning(
    res <- pop_code(anas_for("AN1"), analyses, saf_set(variable = NA_character_)),
    "missing condition variable"
  )
  expect_match(res$code, "df_pop <- ADSL", fixed = TRUE)
})

test_that("comparators are translated and odd values are preserved", {
  analyses <- tibble::tibble(id = "AN1", dataset = "ADSL", analysisSetId = "AS1")
  cmp <- function(comparator, value = "1") {
    pop_code(anas_for("AN1"), analyses,
             saf_set(variable = "X", comparator = comparator, value = value))$code
  }
  expect_match(cmp("GE"), "X >= '1'", fixed = TRUE)
  expect_match(cmp("GT"), "X > '1'", fixed = TRUE)
  expect_match(cmp("LE"), "X <= '1'", fixed = TRUE)
  expect_match(cmp("LT"), "X < '1'", fixed = TRUE)
  expect_match(cmp("NE"), "X != '1'", fixed = TRUE)
  # unknown comparators pass through unchanged
  expect_match(cmp("%in%"), "X %in% '1'", fixed = TRUE)
  # regex metacharacters are kept literally
  expect_match(cmp("EQ", "A (High)"), "X == 'A (High)'", fixed = TRUE)
  # a missing value becomes an empty string
  expect_match(cmp("EQ", NA_character_), "X == ''", fixed = TRUE)
})

test_that("a NULL list-column condition value and a nameless set are handled", {
  sets <- tibble::tibble(
    id = c("AS1", "AS2"),
    condition_dataset = "ADSL",
    condition_variable = c("SAFFL", "ITTFL"),
    condition_comparator = "EQ",
    condition_value = list(NULL, "Y")
  )
  analyses <- tibble::tibble(
    id = c("AN1", "AN2"), dataset = "ADSL", analysisSetId = c("AS1", "AS2")
  )

  res <- pop_code(anas_for(analyses$id), analyses, sets)
  expect_match(res$code, "SAFFL == ''", fixed = TRUE)
  expect_match(res$code, "# Analysis set: AS2\n", fixed = TRUE)
})
