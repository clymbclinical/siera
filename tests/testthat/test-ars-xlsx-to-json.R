# Tests for ars_xlsx_to_json() -------------------------------------------------

# Generate ARD scripts from an ARS file and return each script's normalised text
# (timestamp line dropped, carriage returns and blank lines removed) keyed by
# file name. The JSON reader strips carriage returns from templateCode while the
# xlsx reader does not, so blank-line normalisation is required to compare the
# two paths (see metadata.R codeTemplate handling).
.gen_scripts <- function(ars_path, adam_path) {
  d <- withr::local_tempdir()
  suppressMessages(suppressWarnings(
    readARS(ars_path, output_path = d, adam_path = adam_path)
  ))
  fs <- sort(list.files(d, pattern = "\\.R$", full.names = TRUE))
  stats::setNames(lapply(fs, function(f) {
    ls <- readLines(f, warn = FALSE)
    ls <- ls[!grepl("^# Date created:", ls)]
    ls <- gsub("\r", "", ls)
    ls <- ls[nzchar(trimws(ls))]
    paste(ls, collapse = "\n")
  }), basename(fs))
}

test_that("converted JSON generates identical ARD scripts to the source xlsx", {
  skip_on_cran()
  for (base in c("exampleARS_2", "exampleARS_6")) {
    xlsx  <- ARS_example(paste0(base, ".xlsx"))
    jfile <- withr::local_tempfile(fileext = ".json")
    expect_message(ars_xlsx_to_json(xlsx, jfile), "Wrote ARS JSON")

    adam <- withr::local_tempdir()
    from_xlsx <- .gen_scripts(xlsx, adam)
    from_json <- .gen_scripts(jfile, adam)
    expect_identical(names(from_json), names(from_xlsx))
    for (f in names(from_xlsx)) {
      expect_identical(from_json[[f]], from_xlsx[[f]],
                       info = paste(base, f))
    }
  }
})

test_that("converted JSON is readable by siera's JSON reader", {
  xlsx  <- ARS_example("exampleARS_2.xlsx")
  jfile <- withr::local_tempfile(fileext = ".json")
  ars_xlsx_to_json(xlsx, jfile)

  meta <- siera:::.read_ars_json_metadata(jfile)
  expect_type(meta, "list")
  expect_true(all(c("Lopo", "Lopa", "AnalysisSets", "AnalysisGroupings",
                    "Analyses", "AnalysisMethods") %in% names(meta)))
  # exampleARS_2 has 3 outputs and 26 analyses.
  expect_equal(nrow(meta$Lopo), 3L)
  expect_equal(nrow(meta$Analyses), 26L)
})

test_that("referenced analysis operations survive conversion (numerator/denominator)", {
  xlsx  <- ARS_example("exampleARS_3.xlsx")
  jfile <- withr::local_tempfile(fileext = ".json")
  ars_xlsx_to_json(xlsx, jfile)
  j <- jsonlite::fromJSON(jfile, simplifyVector = FALSE)
  an04 <- Filter(function(a) identical(a$id, "An_04"), j$analyses)[[1]]
  rao  <- an04$referencedAnalysisOperations
  expect_length(rao, 2L)
  dens <- vapply(rao, function(r) r$analysisId, character(1))
  rels <- vapply(rao, function(r) r$referencedOperationRelationshipId, character(1))
  expect_true("An_02" %in% dens)                 # denominator analysis
  expect_true(any(grepl("DEN$", rels)))          # denominator relationship
})

test_that("default json_path is written beside the workbook", {
  src <- ARS_example("exampleARS_6.xlsx")
  dir <- withr::local_tempdir()
  xlsx <- file.path(dir, "study.xlsx")
  file.copy(src, xlsx)
  out <- ars_xlsx_to_json(xlsx)
  expect_equal(out, file.path(dir, "study.json"))
  expect_true(file.exists(out))
})

# --- error guards -----------------------------------------------------------

test_that("input validation errors are raised", {
  xlsx <- ARS_example("exampleARS_2.xlsx")
  expect_error(ars_xlsx_to_json(NULL), "single, non-empty")
  expect_error(ars_xlsx_to_json(c("a", "b")), "single, non-empty")
  expect_error(ars_xlsx_to_json(NA_character_), "single, non-empty")
  expect_error(ars_xlsx_to_json(""), "single, non-empty")
  expect_error(ars_xlsx_to_json("study.json"), "\\.xlsx")
  expect_error(ars_xlsx_to_json("does-not-exist.xlsx"), "not found")
})

test_that("missing required sheets abort", {
  skip_if_not_installed("openxlsx")
  bad <- withr::local_tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(list(ReportingEvent = data.frame(id = "R", name = "x")), bad)
  expect_error(ars_xlsx_to_json(bad), "missing required sheet")
})

test_that("an empty ReportingEvent sheet aborts", {
  skip_if_not_installed("openxlsx")
  bad <- withr::local_tempfile(fileext = ".xlsx")
  sheets <- c("ReportingEvent", "MainListOfContents", "OtherListsOfContents",
              "AnalysisSets", "AnalysisGroupings", "DataSubsets", "Analyses",
              "AnalysisMethods")
  wb <- stats::setNames(lapply(sheets, function(s) data.frame()), sheets)
  wb$ReportingEvent <- data.frame(id = character(0), name = character(0))
  openxlsx::write.xlsx(wb, bad)
  expect_error(ars_xlsx_to_json(bad), "no data row")
})

# --- pure helper units ------------------------------------------------------

test_that(".x2a_term classifies controlled vs sponsor terms", {
  expect_null(siera:::.x2a_term(NULL, "OperationRole"))
  expect_identical(siera:::.x2a_term("NUMERATOR", "OperationRole"),
                   list(controlledTerm = "NUMERATOR"))
  expect_identical(siera:::.x2a_term("MY_ROLE", "OperationRole"),
                   list(sponsorTermId = "MY_ROLE"))
  expect_identical(siera:::.x2a_term("DATA DRIVEN", "AnalysisReason"),
                   list(controlledTerm = "DATA DRIVEN"))
})

test_that(".x2a_pageref dispatches on refType", {
  expect_null(siera:::.x2a_pageref(NULL, "1", "l"))
  # NamedDestination, multi-value
  nd <- siera:::.x2a_pageref("NamedDestination", "a|b", "lbl")
  expect_identical(nd$pageNames, list("a", "b"))
  expect_identical(nd$label, "lbl")
  # NamedDestination, single value stays scalar
  expect_identical(siera:::.x2a_pageref("NamedDestination", "a", NULL)$pageNames, "a")
  # PhysicalRef page range
  rng <- siera:::.x2a_pageref("PhysicalRef", "3-7", NULL)
  expect_identical(rng$firstPage, "3"); expect_identical(rng$lastPage, "7")
  # PhysicalRef page list
  lst <- siera:::.x2a_pageref("PhysicalRef", "3|5", NULL)
  expect_identical(lst$pageNumbers, list("3", "5"))
  # invalid page spec / unknown refType warn and return NULL
  expect_warning(expect_null(siera:::.x2a_pageref("PhysicalRef", "abc", NULL)),
                 "Invalid")
  expect_warning(expect_null(siera:::.x2a_pageref("Mystery", "1", NULL)), "Invalid")
})

test_that(".x2a scalar helpers coerce and NULL-guard", {
  expect_null(siera:::.x2a_str(NA)); expect_identical(siera:::.x2a_str(5), "5")
  expect_null(siera:::.x2a_num(NA)); expect_null(siera:::.x2a_num("x"))
  expect_equal(siera:::.x2a_num("3.5"), 3.5); expect_equal(siera:::.x2a_num(2L), 2L)
  expect_null(siera:::.x2a_bool(NA)); expect_true(siera:::.x2a_bool(TRUE))
  expect_true(siera:::.x2a_bool("TRUE"))
  expect_null(siera:::.x2a_split(NA, " | "))
  expect_identical(siera:::.x2a_split("a", " | "), "a")
  expect_identical(siera:::.x2a_split("a | b", " | "), list("a", "b"))
})

# --- synthetic workbook: advanced branches ----------------------------------
# The bundled fixtures leave the reference-document, terminology, results,
# output-file and document-ref sheets empty, so a purpose-built workbook is
# needed to exercise those code paths.

.write_advanced_wb <- function(path) {
  sh <- list(
    ReportingEvent = data.frame(id = "RE", version = 1, name = "Adv",
                                description = "d", label = "l"),
    About = data.frame(generatedUsing = "test", version = "1.0", note = "n"),
    StudyInfo = data.frame(studyId = "S1", studyTitle = "T",
                           phase = "Phase I | Phase II", compoundUnderStudy = "C",
                           description = "", diseaseArea = "D", therapeuticArea = "TA"),
    ReferenceDocuments = data.frame(id = "RD1", name = "SAP", description = NA,
                                    label = NA, location = "sap.pdf"),
    Categorizations = data.frame(
      id = c("CAT", "CATSUB"), label = c("Cat", "Sub"),
      parent_category_id = c(NA, "CAT_01"),
      category_id = c("CAT_01", "CATSUB_01"),
      category_label = c("C1", "S1")),
    MainListOfContents = data.frame(
      name = "LOPA", listItem_level = c(1, 2),
      listItem_name = c("Out 1", "An 1"), listItem_order = c(1, 1),
      listItem_analysisId = c(NA, "An_S1"), listItem_outputId = c("O1", NA)),
    OtherListsOfContents = data.frame(
      name = "LOPO", listItem_level = 1, listItem_name = "Out 1",
      listItem_order = 1, listItem_analysisId = NA, listItem_outputId = "O1"),
    GlobalDisplaySections = data.frame(
      shellType = c("Table", "Table"), sectionType = c("Title", "Title"),
      subSection_id = c("G1", "G2"), subSection_text = c("t1", "t2")),
    TerminologyExtensions = data.frame(
      id = c("TE", "TE"), enumeration = c("ENUM", "ENUM"),
      sponsorTerm_id = c("SP1", "SP2"),
      sponsorTerm_submissionValue = c("v1", "v2"),
      sponsorTerm_description = c("d1", "d2")),
    # AS1: three-level nested compound AND( OR(cond, subClause), cond ).
    # AS2: single condition with a multi-value IN.
    # AS3: malformed level-1 row (no operator/condition/subclause).
    # AS4: malformed row that starts at level 2 (no level-1 root).
    AnalysisSets = data.frame(
      id = c("AS1", "AS1", "AS1", "AS1", "AS1", "AS2", "AS3", "AS4"),
      name = c("Set1", NA, NA, NA, NA, "Set2", "Set3", "Set4"), description = NA,
      level = c(1, 2, 3, 3, 2, 1, 1, 2), order = c(1, 1, 1, 2, 2, 1, 1, 1),
      compoundExpression_logicalOperator = c("AND", "OR", NA, NA, NA, NA, NA, NA),
      compoundExpression_subClauseId = c(NA, NA, NA, "SUB9", NA, NA, NA, NA),
      condition_dataset = c(NA, NA, "ADSL", NA, "ADSL", "ADSL", NA, "ADSL"),
      condition_variable = c(NA, NA, "SAFFL", NA, "ITTFL", "ITTFL", NA, "SAFFL"),
      condition_comparator = c(NA, NA, "EQ", NA, "EQ", "IN", NA, "EQ"),
      condition_value = c(NA, NA, "Y", NA, "Y", "Y | N", NA, "Y")),
    AnalysisGroupings = data.frame(
      id = c("G_TRT", "G_DD"),
      name = c("Treatment", "DataDriven"), label = c(NA, NA),
      groupingDataset = c("ADSL", "ADAE"),
      groupingVariable = c("TRT01A", "AEDECOD"),
      dataDriven = c(FALSE, TRUE),
      group_id = c("G_TRT_1", NA), group_name = c("Drug", NA),
      group_label = c(NA, NA), group_level = c(1, NA), group_order = c(1, NA),
      group_compoundExpression_logicalOperator = c(NA, NA),
      group_condition_dataset = c("ADSL", NA),
      group_condition_variable = c("TRT01A", NA),
      group_condition_comparator = c("EQ", NA),
      group_condition_value = c("Drug", NA)),
    DataSubsets = data.frame(
      id = "DS1", name = "Sub", description = NA, label = "Sub",
      level = 1, order = 1, compoundExpression_logicalOperator = NA,
      condition_dataset = "ADAE", condition_variable = "TRTEMFL",
      condition_comparator = "EQ", condition_value = "Y"),
    Analyses = data.frame(
      id = c("An_S1", "An_S2"), version = c(1, 1),
      name = c("A1", "A2"), categoryIds = c("CAT_01 | CATSUB_01", "CAT_01"),
      reason = c("DATA DRIVEN", "MY REASON"),
      purpose = c("PRIMARY OUTCOME MEASURE", "MY PURPOSE"),
      analysisSetId = c("AS1", "AS2"),
      groupingId1 = c("G_TRT", NA), resultsByGroup1 = c(TRUE, NA),
      dataSubsetId = c("DS1", NA), dataset = c("ADSL", "ADSL"),
      variable = c("USUBJID", "AVAL"), method_id = c("M1", "M1"),
      referencedAnalysisOperations_referencedOperationId1 = c("M1_02_%_NUM", NA),
      referencedAnalysisOperations_analysisId1 = c("An_S1", NA)),
    AnalysisMethods = data.frame(
      id = c("M1", "M2", "M3"),
      name = c("Count", "DocRef", "Bogus"), label = c("n", NA, NA),
      description = c("count", NA, NA),
      operation_id = c("M1_01_n", "M2_01", "M3_01"),
      operation_name = c("n", "o", "o"), operation_order = c(1, 1, 1),
      operation_label = c("n", NA, NA), operation_resultPattern = c("xx", NA, NA),
      operation_referencedResultRelationships1_id = c("M1_02_%_NUM", NA, NA),
      operation_referencedResultRelationships1_referencedOperationRole =
        c("NUMERATOR", NA, NA),
      operation_referencedResultRelationships1_operationId = c("M1_01_n", NA, NA),
      operation_referencedResultRelationships1_description = c("num", NA, NA),
      operation_referencedResultRelationships2_id = c("M1_02_%_SPN", NA, NA),
      operation_referencedResultRelationships2_referencedOperationRole =
        c("SPONSORROLE", NA, NA),
      operation_referencedResultRelationships2_operationId = c("M1_01_n", NA, NA),
      operation_referencedResultRelationships2_description = c("spn", NA, NA)),
    # M2 code by document reference (resolved); M3 invalid specifiedAs (warn);
    # M4 document reference with no matching ProgrammingCode docref (warn).
    AnalysisMethodCodeTemplate = data.frame(
      method_id = c("M1", "M2", "M3", "M4"),
      context = c("R (siera)", "R (siera)", "R", "R"),
      specifiedAs = c("Code", "DocumentRef", "Bogus", "DocumentRef"),
      templateCode = c("x <- 1", NA, NA, NA)),
    AnalysisMethodCodeParameters = data.frame(
      method_id = "M1", parameter_name = "anavarhere",
      parameter_description = "var", parameter_label = "v",
      parameter_valueSource = "ana_var", parameter_value = NA),
    # Two Documentation refs for M1 (same refDoc, accumulated pageRefs), one
    # ProgrammingCode ref for M2, and a row with no referenceType (skipped).
    AnalysisMethodDocumentRefs = data.frame(
      method_id = c("M1", "M1", "M2", "M1"),
      referenceType = c("Documentation", "Documentation", "ProgrammingCode", NA),
      refDocumentId = c("RD1", "RD1", "RD1", "RD1"),
      pageRef_refType = c("PhysicalRef", "PhysicalRef", "NamedDestination", NA),
      pageRef_label = c(NA, NA, NA, NA),
      pageRef_pages = c("1-3", "5|6", "dest1", NA)),
    # resultGroup2 columns present but empty, exercising the null-group skip.
    AnalysisResults = data.frame(
      id = c("An_S1", "An_S1"),
      operation_id = c("M1_01_n", "M1_01_n"),
      resultGroup1_groupingId = c("G_TRT", "G_TRT"),
      resultGroup1_groupId = c("G_TRT_1", "G_TRT_1"),
      resultGroup1_groupValue = c(NA, NA),
      resultGroup2_groupingId = c(NA, NA),
      resultGroup2_groupId = c(NA, NA),
      resultGroup2_groupValue = c(NA, NA),
      rawValue = c("10", "20"), formattedValue = c("10", "20")),
    OutputCodeParameters = data.frame(
      output_id = "O1", parameter_name = "p", parameter_description = "d",
      parameter_label = "l", parameter_value = "v"),
    OutputProgrammingCode = data.frame(
      output_id = "O1", context = "R", specifiedAs = "Code", code = "y <- 2"),
    Displays = data.frame(
      id = "D1", name = "Disp", version = 1, displayTitle = "Title",
      displaySection_sectionType = c("Title", "Body"),
      displaySection_orderedSubSection_order = c(1, 1),
      displaySection_subSection_id = c("SS1", "SS2"),
      displaySection_subSection_text = c("shown", NA)),
    Outputs = data.frame(
      id = "O1", version = 1, name = "Output 1", categoryIds = "CAT_01",
      display1_Id = "D1"),
    OutputFiles = data.frame(
      output_id = c("O1", "O1"), name = c("f1", "f2"),
      description = c(NA, NA), label = c(NA, NA),
      location = c("f1.pdf", "f2.docx"), fileType = c("pdf", "docx"))
  )
  openxlsx::write.xlsx(sh, path)
}

test_that("advanced sheets convert (docrefs, results, sponsor terms, files)", {
  skip_if_not_installed("openxlsx")
  wb <- withr::local_tempfile(fileext = ".xlsx")
  .write_advanced_wb(wb)
  jfile <- withr::local_tempfile(fileext = ".json")
  # Invalid specifiedAs ("Bogus") and an unresolved DocumentRef both warn.
  w <- testthat::capture_warnings(ars_xlsx_to_json(wb, jfile))
  expect_true(any(grepl("specifiedAs", w)))
  expect_true(any(grepl("ProgrammingCode document reference", w)))
  j <- jsonlite::fromJSON(jfile, simplifyVector = FALSE)

  # about / studyInfo (phase split)
  expect_identical(j$about$generatedUsing, "test")
  expect_identical(j$studyInfo$phase, list("Phase I", "Phase II"))

  # reference documents + terminology extensions (two sponsor terms)
  expect_identical(j$referenceDocuments[[1]]$location, "sap.pdf")
  expect_length(j$terminologyExtensions[[1]]$sponsorTerms, 2L)

  # nested categorization
  expect_identical(j$analysisOutputCategorizations[[1]]$id, "CAT")
  expect_identical(
    j$analysisOutputCategorizations[[1]]$subCategorizations[[1]]$id, "CATSUB")

  # global display sections merged under one sectionType
  expect_length(j$globalDisplaySections[[1]]$subSections, 2L)

  # analysis set: three-level nested compound with a nested subClause
  as1 <- Filter(function(x) x$id == "AS1", j$analysisSets)[[1]]
  expect_identical(as1$compoundExpression$logicalOperator, "AND")
  has_subclause <- function(node) {
    if (is.null(node)) return(FALSE)
    if (!is.null(node$subClauseId)) return(TRUE)
    wcs <- node$compoundExpression$whereClauses
    if (is.null(wcs)) return(FALSE)
    any(vapply(wcs, has_subclause, logical(1)))
  }
  expect_true(has_subclause(as1))
  # multi-value IN condition splits into an array
  as2 <- Filter(function(x) x$id == "AS2", j$analysisSets)[[1]]
  expect_identical(as2$condition$value, list("Y", "N"))

  # data-driven grouping has empty groups
  gdd <- Filter(function(x) x$id == "G_DD", j$analysisGroupings)[[1]]
  expect_true(gdd$dataDriven)
  expect_length(gdd$groups, 0L)

  # sponsor vs controlled reason/purpose
  a1 <- Filter(function(x) x$id == "An_S1", j$analyses)[[1]]
  a2 <- Filter(function(x) x$id == "An_S2", j$analyses)[[1]]
  expect_identical(a1$reason, list(controlledTerm = "DATA DRIVEN"))
  expect_identical(a2$reason, list(sponsorTermId = "MY REASON"))
  expect_identical(a2$purpose, list(sponsorTermId = "MY PURPOSE"))

  # operation role: controlled + sponsor
  ops <- Filter(function(x) x$id == "M1", j$methods)[[1]]$operations[[1]]
  roles <- ops$referencedOperationRelationships
  expect_identical(roles[[1]]$referencedOperationRole, list(controlledTerm = "NUMERATOR"))
  expect_identical(roles[[2]]$referencedOperationRole, list(sponsorTermId = "SPONSORROLE"))

  # method M1 documentation refs: one refDoc with two accumulated pageRefs
  m1 <- Filter(function(x) x$id == "M1", j$methods)[[1]]
  expect_length(m1$documentRefs, 1L)
  expect_length(m1$documentRefs[[1]]$pageRefs, 2L)

  # method M2 code specified by document reference
  m2 <- Filter(function(x) x$id == "M2", j$methods)[[1]]
  expect_false(is.null(m2$codeTemplate$documentRef))
  # method M3 had an invalid specifiedAs, so it carries no code template
  m3 <- Filter(function(x) x$id == "M3", j$methods)[[1]]
  expect_null(m3$codeTemplate)

  # analysis results attached
  expect_length(a1$results, 2L)
  expect_identical(a1$results[[1]]$resultGroups[[1]]$groupingId, "G_TRT")

  # output files: controlled (pdf) + sponsor (docx) file types
  o1 <- j$outputs[[1]]
  expect_length(o1$fileSpecifications, 2L)
  expect_identical(o1$fileSpecifications[[1]]$fileType, list(controlledTerm = "pdf"))
  expect_identical(o1$fileSpecifications[[2]]$fileType, list(sponsorTermId = "docx"))
  # display inlined under the output
  expect_identical(o1$displays[[1]]$display$id, "D1")
  # output programming code (inline) with its parameters
  expect_identical(o1$programmingCode$code, "y <- 2")
  expect_identical(o1$programmingCode$parameters[[1]]$value, "v")
})

test_that("present-but-empty optional sheets convert without error", {
  skip_if_not_installed("openxlsx")
  req <- c("AnalysisSets", "AnalysisGroupings", "DataSubsets", "Analyses",
           "AnalysisMethods")
  sh <- c(
    list(
      ReportingEvent = data.frame(id = "RE", name = "Empty"),
      # main list carries a level jump (1 -> 3) exercising the orphan-row skip
      MainListOfContents = data.frame(
        name = c("M", "M"), listItem_level = c(1, 3),
        listItem_name = c("Out", "Orphan"), listItem_order = c(1, 1),
        listItem_analysisId = c(NA, NA), listItem_outputId = c("O1", NA)),
      OtherListsOfContents = data.frame(
        name = character(0), listItem_level = numeric(0),
        listItem_name = character(0), listItem_order = numeric(0),
        listItem_outputId = character(0)),
      About = data.frame(generatedUsing = character(0), version = character(0)),
      StudyInfo = data.frame(studyId = character(0), studyTitle = character(0)),
      Categorizations = data.frame(id = character(0), category_id = character(0))
    ),
    stats::setNames(lapply(req, function(s) data.frame(id = character(0))), req)
  )
  wb <- withr::local_tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(sh, wb)
  jfile <- withr::local_tempfile(fileext = ".json")
  expect_message(ars_xlsx_to_json(wb, jfile))
  j <- jsonlite::fromJSON(jfile, simplifyVector = FALSE)
  expect_identical(j[["@type"]], "ReportingEvent")
  expect_null(j$about)
  expect_null(j$studyInfo)
  expect_length(j$otherListsOfContents, 0L)
  expect_length(j$analysisOutputCategorizations, 0L)
})
