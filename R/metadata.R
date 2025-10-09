.read_ars_metadata <- function(ARS_path) {
  file_ext <- tolower(tools::file_ext(ARS_path))

  if (!file_ext %in% c("json", "xlsx")) {
    cli::cli_warn(
      "Input ARS file must be JSON or xlsx; {.path {ARS_path}} was received"
    )
    return(NULL)
  }

  metadata <- if (file_ext == "json") {
    .read_ars_json_metadata(ARS_path)
  } else {
    .read_ars_xlsx_metadata(ARS_path)
  }

  if (is.null(metadata)) {
    return(NULL)
  }

  c(list(file_ext = file_ext), metadata)
}

.read_ars_json_metadata <- function(ARS_path) {
  json_from <- jsonlite::fromJSON(ARS_path)

  required_json_sections <- c(
    "otherListsOfContents",
    "mainListOfContents",
    "dataSubsets",
    "analysisGroupings",
    "analyses",
    "methods"
  )

  missing_sections <- setdiff(required_json_sections, names(json_from))

  if (length(missing_sections) > 0) {
    cli::cli_warn(
      "Input ARS file is missing required metadata sections: {paste(missing_sections, collapse = ', ')}"
    )
    return(NULL)
  }

  otherListsOfContents <- json_from$otherListsOfContents$contentsList$listItems[[1]]
  Lopo <- otherListsOfContents |>
    dplyr::rename(
      listItem_outputId = outputId,
      listItem_name = name,
      listItem_order = order,
      listItem_level = level
    )

  mainListOfContents <- json_from$mainListOfContents$contentsList$listItems

  Lopa <- data.frame()
  for (a in seq_len(nrow(otherListsOfContents))) {
    tmp_PO <- otherListsOfContents[a, ]
    tmp_json_Lopa <- mainListOfContents$sublist$listItems[[a]]

    anaIds <- tmp_json_Lopa$analysisId |>
      tibble::as_tibble() |>
      dplyr::mutate(listItem_outputId = tmp_PO$outputId)

    if (nrow(anaIds) > 0) {
      anaIds <- anaIds |>
        dplyr::rename(listItem_analysisId = value) |>
        dplyr::filter(!is.na(listItem_analysisId))
    }

    Lopa <- rbind(Lopa, anaIds)

    if ("sublist" %in% names(tmp_json_Lopa)) {
      tmp_json_lopa_sub <- tmp_json_Lopa$sublist$listItems

      forend <- length(tmp_json_lopa_sub)
      subana_dset <- data.frame()
      for (b in seq_len(forend)) {
        if (!is.null(tmp_json_lopa_sub[[b]])) {
          ana_ids <- tmp_json_lopa_sub[[b]]$analysisId |>
            tibble::as_tibble() |>
            dplyr::mutate(listItem_outputId = tmp_PO$outputId) |>
            dplyr::rename(listItem_analysisId = value)

          subana_dset <- rbind(subana_dset, ana_ids)
        }
      }
      Lopa <- rbind(Lopa, subana_dset)
    }
  }

  JSON_DataSubsets <- json_from$dataSubsets
  JSONDSL1 <- tibble::tibble(
    id = json_from$dataSubsets[["id"]],
    name = json_from$dataSubsets[["name"]],
    label = json_from$dataSubsets[["label"]],
    order = json_from$dataSubsets[["order"]],
    level = json_from$dataSubsets[["level"]],
    condition_dataset = json_from[["dataSubsets"]][["condition"]][["dataset"]],
    condition_variable = json_from[["dataSubsets"]][["condition"]][["variable"]],
    condition_comparator = json_from[["dataSubsets"]][["condition"]][["comparator"]],
    condition_value = json_from[["dataSubsets"]][["condition"]][["value"]],
    compoundExpression_logicalOperator = json_from[["dataSubsets"]][["compoundExpression"]][["logicalOperator"]]
  )

  whereClauses <- JSON_DataSubsets[["compoundExpression"]][["whereClauses"]]
  JSONDSL2 <- data.frame()
  JSONDSL3 <- data.frame()
  for (c in seq_len(nrow(JSON_DataSubsets))) {
    tmp_DSID <- JSON_DataSubsets[c, "id"]
    tmp_DSname <- JSON_DataSubsets[c, "name"]
    tmp_DSlabel <- JSON_DataSubsets[c, "label"]

    if (!is.null(whereClauses[[c]])) {
      tmp_DS <- tibble::tibble(
        level = whereClauses[[c]][["level"]],
        order = whereClauses[[c]][["order"]],
        condition_dataset = whereClauses[[c]][["condition"]][["dataset"]],
        condition_variable = whereClauses[[c]][["condition"]][["variable"]],
        condition_comparator = whereClauses[[c]][["condition"]][["comparator"]],
        condition_value = whereClauses[[c]][["condition"]][["value"]],
        compoundExpression_logicalOperator = whereClauses[[c]]$compoundExpression$logicalOperator,
        id = tmp_DSID,
        name = tmp_DSname,
        label = tmp_DSlabel
      )
      JSONDSL2 <- dplyr::bind_rows(JSONDSL2, tmp_DS)

      whereClausesL2 <- whereClauses[[c]][["compoundExpression"]][["whereClauses"]]
      for (d in seq_len(nrow(tmp_DS))) {
        if (!is.null(whereClausesL2[[d]])) {
          tmp_DSL2 <- tibble::tibble(
            level = whereClausesL2[[d]][["level"]],
            order = whereClausesL2[[d]][["order"]],
            condition_dataset = whereClausesL2[[d]][["condition"]][["dataset"]],
            condition_variable = whereClausesL2[[d]][["condition"]][["variable"]],
            condition_comparator = whereClausesL2[[d]][["condition"]][["comparator"]],
            condition_value = whereClausesL2[[d]][["condition"]][["value"]],
            id = tmp_DSID,
            name = tmp_DSname,
            label = tmp_DSlabel
          )
          JSONDSL3 <- dplyr::bind_rows(JSONDSL3, tmp_DSL2)
        }
      }
    }
  }

  DataSubsets <- dplyr::bind_rows(JSONDSL1, JSONDSL2, JSONDSL3) |>
    dplyr::arrange(id, level, order)
  DataSubsets$condition_value[DataSubsets$condition_value == "NULL"] <- NA

  AnalysisSets <- tibble::tibble(
    id = json_from$analysisSets$id,
    label = json_from$analysisSets$label,
    name = json_from$analysisSets$name,
    level = json_from$analysisSets$level,
    order = json_from$analysisSets$order,
    condition_dataset = json_from$analysisSets$condition[["dataset"]],
    condition_variable = json_from$analysisSets$condition[["variable"]],
    condition_comparator = json_from$analysisSets$condition[["comparator"]],
    condition_value = json_from$analysisSets$condition[["value"]]
  )

  JSON_AnalysisGroupings <- json_from$analysisGroupings
  JSON_AG_1 <- tibble::tibble(
    id = json_from$analysisGroupings$id,
    name = json_from$analysisGroupings$name,
    groupingDataset = json_from$analysisGroupings$groupingDataset,
    groupingVariable = json_from$analysisGroupings$groupingVariable,
    dataDriven = json_from$analysisGroupings$dataDriven
  )

  JSON_AG <- data.frame()
  for (e in seq_len(nrow(JSON_AG_1))) {
    AG_ID <- as.character(JSON_AG_1[e, "id"])
    AG_name <- as.character(JSON_AG_1[e, "name"])
    AG_groupingVariable <- as.character(JSON_AG_1[e, "groupingVariable"])
    AG_groupingDataset <- as.character(JSON_AG_1[e, "groupingDataset"])
    AG_dataDriven <- as.character(JSON_AG_1[e, "dataDriven"])

    tmp_AG <- tibble::tibble(
      group_id = JSON_AnalysisGroupings[["groups"]][[e]]$id,
      group_name = JSON_AnalysisGroupings[["groups"]][[e]]$name,
      group_level = JSON_AnalysisGroupings[["groups"]][[e]]$level,
      group_order = JSON_AnalysisGroupings[["groups"]][[e]]$order,
      group_condition_dataset = JSON_AnalysisGroupings[["groups"]][[e]]$condition[["dataset"]],
      group_condition_variable = JSON_AnalysisGroupings[["groups"]][[e]]$condition[["variable"]],
      group_condition_comparator = JSON_AnalysisGroupings[["groups"]][[e]]$condition[["comparator"]],
      group_condition_value = JSON_AnalysisGroupings[["groups"]][[e]]$condition[["value"]],
      id = AG_ID,
      name = AG_name,
      groupingVariable = AG_groupingVariable,
      groupingDataset = AG_groupingDataset,
      dataDriven = AG_dataDriven
    )

    JSON_AG <- dplyr::bind_rows(JSON_AG, tmp_AG)
  }

  AnalysisGroupings <- dplyr::bind_rows(JSON_AG)

  JSON_AN <- json_from$analyses

  JSON_AnalysesL1 <- tibble::tibble(
    id = JSON_AN$id,
    name = JSON_AN$name,
    label = JSON_AN$label,
    version = JSON_AN$version,
    categoryIds = JSON_AN$categoryIds,
    method_id = JSON_AN$methodId,
    analysisSetId = JSON_AN$analysisSetId,
    dataset = JSON_AN$dataset,
    variable = JSON_AN$variable,
    dataSubsetId = JSON_AN$dataSubsetId
  )

  AN_groupings <- data.frame()
  for (g in seq_len(nrow(JSON_AnalysesL1))) {
    tmp_id <- as.character(JSON_AN[g, ]$id)

    if (nrow(JSON_AN[["orderedGroupings"]][[g]]) > 0) {
      tmp <- JSON_AN[["orderedGroupings"]][[g]] %>%
        tidyr::pivot_wider(
          names_from = order,
          values_from = c(resultsByGroup, groupingId),
          names_glue = "{.value}{order}"
        ) %>%
        dplyr::mutate(id = tmp_id)
    } else {
      tmp <- tibble::tibble(
        resultByGroup = NA,
        groupingId1 = NA,
        id = tmp_id
      )
    }

    AN_groupings <- dplyr::bind_rows(AN_groupings, tmp)
  }

  AN_refs <- data.frame()
  for (h in seq_len(nrow(JSON_AnalysesL1))) {
    tmp_id <- as.character(JSON_AN[h, ]$id)

    if (!is.null(JSON_AN[["referencedAnalysisOperations"]][[h]])) {
      tmp_ref <- JSON_AN[["referencedAnalysisOperations"]][[h]] %>%
        dplyr::mutate(order = dplyr::row_number()) %>%
        tidyr::pivot_wider(
          names_from = order,
          values_from = c(referencedOperationRelationshipId, analysisId),
          names_glue = "{'referencedAnalysisOperations_'}{.value}{order}"
        ) %>%
        dplyr::mutate(id = tmp_id)

      AN_refs <- dplyr::bind_rows(AN_refs, tmp_ref)
    }
  }
  colnames(AN_refs) <- gsub("Relationship", "", colnames(AN_refs))

  Analyses <- merge(
    JSON_AnalysesL1,
    AN_refs,
    by = "id",
    all.x = TRUE
  ) %>%
    merge(
      AN_groupings,
      by = "id",
      all.x = TRUE
    ) %>%
    dplyr::filter(!is.na(method_id), method_id != "")

  JSONAML1 <- tibble::tibble(
    id = json_from$methods$id,
    name = json_from$methods$name,
    description = json_from$methods$description,
    label = json_from$methods$label
  )

  JSONAML2 <- data.frame()
  JSONAML3 <- data.frame()
  for (i in seq_len(nrow(JSONAML1))) {
    tmp_l2 <- tibble::tibble(
      operation_id = json_from$methods$operations[[i]]$id,
      operation_name = json_from$methods$operations[[i]]$name,
      operation_resultPattern = json_from$methods$operations[[i]]$resultPattern,
      operation_label = json_from$methods$operations[[i]]$label,
      operation_order = json_from$methods$operations[[i]]$order,
      id = as.character(JSONAML1[i, ]$id)
    )
    JSONAML2 <- dplyr::bind_rows(JSONAML2, tmp_l2)

    rOF <- json_from$methods$operations[[i]]$referencedOperationRelationships
    if (!is.null(rOF)) {
      lenrOF <- length(rOF)

      for (j in seq_len(lenrOF)) {
        if (!is.null(rOF[[j]])) {
          tmp_l3 <- tibble::tibble(
            id = rOF[[j]]$id,
            operationId = rOF[[j]]$operationId,
            description = rOF[[j]]$description,
            referencedOperationRole = rOF[[j]]$referencedOperationRole$controlledTerm
          )

          tmp_l3_fin <- tmp_l3 %>%
            dplyr::mutate(order = dplyr::row_number()) %>%
            tidyr::pivot_wider(
              names_from = order,
              values_from = c(id, operationId, description, referencedOperationRole),
              names_glue = "{'operation_referencedResultRelationships'}{order}{'_'}{.value}"
            ) %>%
            dplyr::mutate(operation_id = json_from[["methods"]][["operations"]][[i]][["id"]][[j]])

          JSONAML3 <- dplyr::bind_rows(JSONAML3, tmp_l3_fin)
        }
      }
    }
  }

  AnalysisMethods <- merge(
    JSONAML1,
    JSONAML2,
    by = "id",
    all = TRUE
  ) %>%
    merge(
      JSONAML3,
      by = "operation_id",
      all = TRUE
    )

  AnalysisMethodCodeTemplate <- tibble::tibble(
    method_id = json_from$methods$id,
    context = json_from$methods$codeTemplate$context,
    specifiedAs = "Code",
    templateCode = json_from$methods$codeTemplate$code
  )

  AnalysisMethodCodeParameters <- data.frame()
  for (i in seq_len(nrow(JSONAML1))) {
    id <- as.character(JSONAML1[i, ]$id)
    tmp_AMCP <- tibble::tibble(
      method_id = id,
      parameter_name = json_from$methods$codeTemplate$parameters[[i]]$name,
      parameter_description = json_from$methods$codeTemplate$parameters[[i]]$description,
      parameter_valueSource = json_from$methods$codeTemplate$parameters[[i]]$valueSource
    )

    AnalysisMethodCodeParameters <- dplyr::bind_rows(
      AnalysisMethodCodeParameters,
      tmp_AMCP
    )
  }

  list(
    Lopo = Lopo,
    Lopa = Lopa,
    DataSubsets = DataSubsets,
    AnalysisSets = AnalysisSets,
    AnalysisGroupings = AnalysisGroupings,
    Analyses = Analyses,
    AnalysisMethods = AnalysisMethods,
    AnalysisMethodCodeTemplate = AnalysisMethodCodeTemplate,
    AnalysisMethodCodeParameters = AnalysisMethodCodeParameters
  )
}


.read_ars_xlsx_metadata <- function(ARS_path) {
  ws <- readxl::excel_sheets(ARS_path)

  required_sheets <- c(
    "OtherListsOfContents",
    "MainListOfContents",
    "DataSubsets",
    "AnalysisGroupings",
    "Analyses",
    "AnalysisMethods"
  )

  missing_sheets <- setdiff(required_sheets, ws)

  if (length(missing_sheets) > 0) {
    cli::cli_warn(
      "Input ARS workbook is missing required sheets: {paste(missing_sheets, collapse = ', ')}"
    )
    return(NULL)
  }

  ARS_xlsx <- ARS_path
  mainListOfContents <- readxl::read_excel(ARS_xlsx, sheet = "MainListOfContents")
  otherListsOfContents <- readxl::read_excel(ARS_xlsx, sheet = "OtherListsOfContents")
  DataSubsets <- readxl::read_excel(ARS_xlsx, sheet = "DataSubsets")
  AnalysisSets <- readxl::read_excel(ARS_xlsx, sheet = "AnalysisSets")
  AnalysisGroupings <- readxl::read_excel(ARS_xlsx, sheet = "AnalysisGroupings")
  Analyses <- readxl::read_excel(ARS_xlsx, sheet = "Analyses") %>%
    dplyr::filter(!is.na(method_id))
  AnalysisMethods <- readxl::read_excel(ARS_xlsx, sheet = "AnalysisMethods")
  AnalysisMethodCodeTemplate <- readxl::read_excel(ARS_xlsx, sheet = "AnalysisMethodCodeTemplate")
  AnalysisMethodCodeParameters <- readxl::read_excel(ARS_xlsx, sheet = "AnalysisMethodCodeParameters")

  Lopo <- otherListsOfContents

  Lopa <- mainListOfContents %>%
    tidyr::fill(listItem_outputId) %>%
    dplyr::filter(!is.na(listItem_analysisId)) %>%
    dplyr::select(listItem_analysisId, listItem_outputId)

  list(
    Lopo = Lopo,
    Lopa = Lopa,
    DataSubsets = DataSubsets,
    AnalysisSets = AnalysisSets,
    AnalysisGroupings = AnalysisGroupings,
    Analyses = Analyses,
    AnalysisMethods = AnalysisMethods,
    AnalysisMethodCodeTemplate = AnalysisMethodCodeTemplate,
    AnalysisMethodCodeParameters = AnalysisMethodCodeParameters
  )
}
