# Convert an ARS Excel workbook to ARS JSON ----
#
# R-native reimplementation of CDISC's Python `excel2ars.py`
# (https://github.com/cdisc-org/analysis-results-standard/blob/main/utilities/python/excel2ars.py).
# siera keeps a single JSON ingestion path in `readARS()`; this converter is the
# supported way to bring an ARS Excel workbook (as emitted by TFL Designer or the
# CDISC ARS template) onto that path. It is a faithful whole-workbook conversion:
# every ARS worksheet is mapped, not just the sheets siera itself consumes.

# --- low-level cell helpers -------------------------------------------------

# A single cell, or NULL when the cell is missing/blank. NULL fields are dropped
# from the assembled objects so they are omitted from the JSON (matching how the
# LinkML dumper omits unset slots).
.x2a_str <- function(v) {
  if (length(v) == 0L || is.na(v)) return(NULL)
  as.character(v)
}

.x2a_num <- function(v) {
  if (length(v) == 0L || is.na(v)) return(NULL)
  if (is.character(v)) {
    n <- suppressWarnings(as.numeric(v))
    if (is.na(n)) return(NULL)
    return(n)
  }
  v
}

.x2a_bool <- function(v) {
  if (length(v) == 0L || is.na(v)) return(NULL)
  if (is.logical(v)) return(v)
  as.logical(v)
}

# Build a named list, dropping NULL members (so unset ARS slots are omitted).
.x2a_obj <- function(...) {
  x <- list(...)
  x[!vapply(x, is.null, logical(1L))]
}

# Split a delimited multi-value cell into an unnamed list (JSON array); return a
# scalar for a single value so `jsonlite`'s auto_unbox keeps it a plain string.
.x2a_split <- function(v, sep) {
  if (length(v) == 0L || is.na(v)) return(NULL)
  s <- as.character(v)
  if (grepl(sep, s, fixed = TRUE)) as.list(strsplit(s, sep, fixed = TRUE)[[1]]) else s
}

# A row accessor for one sheet: returns the cell in `col` at row `i`, or NA when
# the column is absent (mirrors the Python `"col" in map` guards).
.x2a_rowget <- function(df) {
  function(col, i) if (col %in% names(df)) df[[col]][[i]] else NA
}

# TRUE for a sheet row where every cell is NA (the Python skips all-None rows).
.x2a_blank_row <- function(df, i) {
  all(vapply(names(df), function(col) {
    v <- df[[col]][[i]]
    length(v) == 0L || is.na(v)
  }, logical(1L)))
}

# Row indices of a sheet with all-blank rows skipped (centralises the Python
# all-None-row skip used by every section loop).
.x2a_nonblank_rows <- function(df) {
  if (nrow(df) == 0L) return(integer(0))
  Filter(function(i) !.x2a_blank_row(df, i), seq_len(nrow(df)))
}

# Read a worksheet, or an empty tibble when the (optional) sheet is absent, so
# downstream section loops simply no-op rather than erroring.
.x2a_read_sheet <- function(path, sheet) {
  if (!sheet %in% readxl::excel_sheets(path)) return(tibble::tibble())
  suppressMessages(readxl::read_excel(path, sheet = sheet))
}

# --- controlled terminology -------------------------------------------------

# ARS controlled-term enumerations, mirrored from the CDISC ARS LinkML model so
# the converter can reproduce excel2ars.py's controlled-term-vs-sponsor-term
# branching. Keep in sync with the ARS model on each ARS version bump.
.x2a_enums <- list(
  OperationRole   = c("NUMERATOR", "DENOMINATOR"),
  AnalysisReason  = c("SPECIFIED IN PROTOCOL", "SPECIFIED IN SAP",
                      "DATA DRIVEN", "REQUESTED BY REGULATORY AGENCY"),
  AnalysisPurpose = c("PRIMARY OUTCOME MEASURE", "SECONDARY OUTCOME MEASURE",
                      "EXPLORATORY OUTCOME MEASURE"),
  OutputFileType  = c("pdf", "rtf", "txt")
)

# A term object: `controlledTerm` when the value is in the ARS enumeration,
# otherwise a sponsor-defined `sponsorTermId` (mirrors excel2ars.py).
.x2a_term <- function(v, enum_name) {
  if (is.null(v)) return(NULL)
  if (v %in% .x2a_enums[[enum_name]]) list(controlledTerm = v) else list(sponsorTermId = v)
}

# --- document references (get_docrefs) --------------------------------------

# A single page reference, dispatched on refType exactly as excel2ars.py does.
.x2a_pageref <- function(reftype, pages, label) {
  if (is.null(reftype)) return(NULL)
  if (identical(reftype, "NamedDestination")) {
    return(.x2a_obj(refType = reftype, label = label,
                    pageNames = .x2a_split(pages, "|")))
  }
  if (identical(reftype, "PhysicalRef")) {
    ps <- if (is.null(pages)) "" else as.character(pages)
    if (grepl("^[0-9]+-[0-9]+$", ps)) {
      parts <- strsplit(ps, "-", fixed = TRUE)[[1]]
      return(.x2a_obj(refType = reftype, label = label,
                      firstPage = parts[1], lastPage = parts[2]))
    }
    if (grepl("^[0-9]+([|][0-9]+)*$", ps)) {
      return(.x2a_obj(refType = reftype, label = label,
                      pageNumbers = .x2a_split(pages, "|")))
    }
    cli::cli_warn("Invalid {.field pageRef_pages} value: {.val {ps}}")
    return(NULL)
  }
  cli::cli_warn("Invalid {.field pageRef_refType} value: {.val {reftype}}")
  NULL
}

# Document references from a *DocumentRefs sheet, indexed by referenceType
# ("Documentation"/"ProgrammingCode") then owner id. Repeated rows for the same
# owner+refDocumentId accumulate pageRefs onto a single DocumentReference.
.x2a_docrefs <- function(path, sheet, id_col) {
  res <- list(Documentation = list(), ProgrammingCode = list())
  if (!sheet %in% readxl::excel_sheets(path)) return(res)
  df <- .x2a_read_sheet(path, sheet)
  for (i in .x2a_nonblank_rows(df)) {
    g <- .x2a_rowget(df)
    reftype <- .x2a_str(g("referenceType", i))
    owner   <- .x2a_str(g(id_col, i))
    refdoc  <- .x2a_str(g("refDocumentId", i))
    if (is.null(reftype) || is.null(owner)) next
    pageref <- .x2a_pageref(.x2a_str(g("pageRef_refType", i)),
                            .x2a_str(g("pageRef_pages", i)),
                            .x2a_str(g("pageRef_label", i)))
    lst <- res[[reftype]][[owner]]
    if (is.null(lst)) lst <- list()
    # Find an existing DocumentReference for this refDocumentId.
    pos <- Find(function(k) identical(lst[[k]]$referenceDocumentId, refdoc),
                seq_along(lst))
    if (is.null(pos)) {
      lst[[length(lst) + 1L]] <- .x2a_obj(
        referenceDocumentId = refdoc,
        pageRefs = if (!is.null(pageref)) list(pageref) else NULL
      )
    } else if (!is.null(pageref)) {
      lst[[pos]]$pageRefs <- c(lst[[pos]]$pageRefs, list(pageref))
    }
    res[[reftype]][[owner]] <- lst
  }
  res
}

# --- nested list of contents ------------------------------------------------

# Recursively build the nested `listItems` tree for a run of content-list rows
# (already ordered as they appear on the sheet). `level` is the ARS table
# indentation depth; children are the following rows with a deeper level.
.x2a_build_items <- function(rows, start, level) {
  g <- .x2a_rowget(rows)
  items <- list()
  i <- start
  n <- nrow(rows)
  while (i <= n) {
    lvl <- .x2a_num(g("listItem_level", i))
    if (is.null(lvl) || lvl < level) break
    if (lvl == level) {
      item <- .x2a_obj(
        level       = .x2a_num(g("listItem_level", i)),
        name        = .x2a_str(g("listItem_name", i)),
        description = .x2a_str(g("listItem_description", i)),
        label       = .x2a_str(g("listItem_label", i)),
        order       = .x2a_num(g("listItem_order", i)),
        analysisId  = .x2a_str(g("listItem_analysisId", i)),
        outputId    = .x2a_str(g("listItem_outputId", i))
      )
      child <- .x2a_build_items(rows, i + 1L, level + 1L)
      if (length(child$items) > 0L) {
        item$sublist <- list(listItems = child$items)
      }
      items[[length(items) + 1L]] <- item
      i <- child$next_i
    } else {
      i <- i + 1L
    }
  }
  list(items = items, next_i = i)
}

# Assemble the `mainListOfContents` object and the `otherListsOfContents` array
# from the two content-list sheets. Rows are grouped into named content lists by
# their `name` column; the main sheet yields one object, the other sheet an array.
.x2a_content_lists <- function(path) {
  main_obj <- NULL
  other <- list()
  for (sheet in c("MainListOfContents", "OtherListsOfContents")) {
    df <- .x2a_read_sheet(path, sheet)
    if (nrow(df) == 0L) next
    # Preserve first-seen order of the content-list names on this sheet; an
    # all-blank sheet leaves no rows, so the name loop below simply no-ops.
    df <- df[.x2a_nonblank_rows(df), , drop = FALSE]
    names_col <- as.character(df[["name"]])
    for (nm in unique(names_col)) {
      sub <- df[names_col == nm, , drop = FALSE]
      built <- .x2a_build_items(sub, 1L, 1L)
      g <- .x2a_rowget(sub)
      clist <- .x2a_obj(
        name        = nm,
        description = .x2a_str(g("description", 1L)),
        label       = .x2a_str(g("label", 1L)),
        contentsList = list(listItems = built$items)
      )
      if (sheet == "MainListOfContents") {
        if (is.null(main_obj)) main_obj <- clist
      } else {
        other[[length(other) + 1L]] <- clist
      }
    }
  }
  list(main = main_obj, other = other)
}

# --- where-clause trees (analysis sets, data subsets, groups) ---------------

# Recursively build a run of where-clauses at `level` from consecutive rows,
# using the given column prefix ("" for sets/subsets, "group_" for groups).
.x2a_build_where <- function(rows, start, level, pfx) {
  g <- .x2a_rowget(rows)
  lv  <- function(i) .x2a_num(g(paste0(pfx, "level"), i))
  clauses <- list()
  i <- start
  n <- nrow(rows)
  while (i <= n) {
    lvl <- lv(i)
    if (is.null(lvl) || lvl < level) break
    if (lvl == level) {
      op <- .x2a_str(g(paste0(pfx, "compoundExpression_logicalOperator"), i))
      cond_var  <- .x2a_str(g(paste0(pfx, "condition_variable"), i))
      subclause <- .x2a_str(g(paste0(pfx, "compoundExpression_subClauseId"), i))
      if (!is.null(op)) {
        child <- .x2a_build_where(rows, i + 1L, level + 1L, pfx)
        wc <- .x2a_obj(
          level = .x2a_num(g(paste0(pfx, "level"), i)),
          order = .x2a_num(g(paste0(pfx, "order"), i)),
          compoundExpression = list(logicalOperator = op, whereClauses = child$clauses)
        )
        i <- child$next_i
      } else if (!is.null(cond_var)) {
        wc <- .x2a_obj(
          level = .x2a_num(g(paste0(pfx, "level"), i)),
          order = .x2a_num(g(paste0(pfx, "order"), i)),
          condition = .x2a_obj(
            dataset    = .x2a_str(g(paste0(pfx, "condition_dataset"), i)),
            variable   = cond_var,
            comparator = .x2a_str(g(paste0(pfx, "condition_comparator"), i)),
            value      = .x2a_split(g(paste0(pfx, "condition_value"), i), " | ")
          )
        )
        i <- i + 1L
      } else if (!is.null(subclause)) {
        wc <- .x2a_obj(
          level = .x2a_num(g(paste0(pfx, "level"), i)),
          order = .x2a_num(g(paste0(pfx, "order"), i)),
          subClauseId = subclause
        )
        i <- i + 1L
      } else {
        i <- i + 1L
        next
      }
      clauses[[length(clauses) + 1L]] <- wc
    } else {
      i <- i + 1L
    }
  }
  list(clauses = clauses, next_i = i)
}

# Collapse an entity's where-clause rows into the level/order/condition/
# compoundExpression fields carried directly on the entity (the level-1 root).
.x2a_where_root <- function(rows, pfx) {
  built <- .x2a_build_where(rows, 1L, 1L, pfx)
  if (length(built$clauses) == 0L) return(list())
  root <- built$clauses[[1L]]
  .x2a_obj(
    level = root$level,
    order = root$order,
    condition = root$condition,
    compoundExpression = root$compoundExpression
  )
}

# --- individual sections ----------------------------------------------------

.x2a_about <- function(path, sheets) {
  if (!"About" %in% sheets) return(NULL)
  df <- .x2a_read_sheet(path, "About")
  if (nrow(df) == 0L) return(NULL)
  g <- .x2a_rowget(df)
  .x2a_obj(
    generatedUsing = .x2a_str(g("generatedUsing", 1L)),
    version        = .x2a_str(g("version", 1L)),
    note           = .x2a_str(g("note", 1L))
  )
}

.x2a_study_info <- function(path, sheets) {
  if (!"StudyInfo" %in% sheets) return(NULL)
  df <- .x2a_read_sheet(path, "StudyInfo")
  if (nrow(df) == 0L) return(NULL)
  g <- .x2a_rowget(df)
  .x2a_obj(
    studyId            = .x2a_str(g("studyId", 1L)),
    studyTitle         = .x2a_str(g("studyTitle", 1L)),
    phase              = .x2a_split(g("phase", 1L), " | "),
    compoundUnderStudy = .x2a_str(g("compoundUnderStudy", 1L)),
    description        = .x2a_str(g("description", 1L)),
    diseaseArea        = .x2a_str(g("diseaseArea", 1L)),
    therapeuticArea    = .x2a_str(g("therapeuticArea", 1L))
  )
}

.x2a_reference_documents <- function(path) {
  df <- .x2a_read_sheet(path, "ReferenceDocuments")
  out <- list()
  for (i in .x2a_nonblank_rows(df)) {
    g <- .x2a_rowget(df)
    out[[length(out) + 1L]] <- .x2a_obj(
      id          = .x2a_str(g("id", i)),
      name        = .x2a_str(g("name", i)),
      description = .x2a_str(g("description", i)),
      label       = .x2a_str(g("label", i)),
      location    = .x2a_str(g("location", i))
    )
  }
  out
}

.x2a_terminology_extensions <- function(path) {
  df <- .x2a_read_sheet(path, "TerminologyExtensions")
  idx <- list()
  order <- character(0)
  for (i in .x2a_nonblank_rows(df)) {
    g <- .x2a_rowget(df)
    id <- .x2a_str(g("id", i))
    term <- .x2a_obj(
      id              = .x2a_str(g("sponsorTerm_id", i)),
      submissionValue = .x2a_str(g("sponsorTerm_submissionValue", i)),
      description     = .x2a_str(g("sponsorTerm_description", i))
    )
    if (is.null(idx[[id]])) {
      idx[[id]] <- .x2a_obj(
        id           = id,
        enumeration  = .x2a_str(g("enumeration", i)),
        sponsorTerms = list(term)
      )
      order <- c(order, id)
    } else {
      idx[[id]]$sponsorTerms <- c(idx[[id]]$sponsorTerms, list(term))
    }
  }
  unname(idx[order])
}

.x2a_categorizations <- function(path) {
  df <- .x2a_read_sheet(path, "Categorizations")
  # Categorization tree: top-level categorizations plus nested
  # subCategorizations. A categorization whose `parent_category_id` names a
  # category id nests under the categorization that owns that category.
  cat_owner <- list()      # category id -> owning categorization id
  cat_defs <- list()       # categorization id -> list(id,label,parent,categories)
  order <- character(0)
  for (i in .x2a_nonblank_rows(df)) {
    g <- .x2a_rowget(df)
    id <- .x2a_str(g("id", i))
    category_id <- .x2a_str(g("category_id", i))
    if (is.null(cat_defs[[id]])) {
      cat_defs[[id]] <- list(id = id, label = .x2a_str(g("label", i)),
                             parent = .x2a_str(g("parent_category_id", i)),
                             categories = list())
      order <- c(order, id)
    }
    cat_defs[[id]]$categories[[length(cat_defs[[id]]$categories) + 1L]] <-
      .x2a_obj(id = category_id, label = .x2a_str(g("category_label", i)))
    cat_owner[[category_id]] <- id
  }
  if (length(order) == 0L) return(list())

  # Recursively build a categorization object with its subCategorizations.
  build <- function(id) {
    d <- cat_defs[[id]]
    subs <- list()
    for (cid in order) {
      p <- cat_defs[[cid]]$parent
      if (!is.null(p) && !is.null(cat_owner[[p]]) && identical(cat_owner[[p]], id)) {
        subs[[length(subs) + 1L]] <- build(cid)
      }
    }
    obj <- list(id = d$id)
    if (length(subs) > 0L) obj$subCategorizations <- subs
    if (!is.null(d$label)) obj$label <- d$label
    obj$categories <- d$categories
    obj
  }

  tops <- Filter(function(id) is.null(cat_defs[[id]]$parent), order)
  lapply(tops, build)
}

.x2a_global_display_sections <- function(path) {
  df <- .x2a_read_sheet(path, "GlobalDisplaySections")
  idx <- list()
  order <- character(0)
  for (i in .x2a_nonblank_rows(df)) {
    g <- .x2a_rowget(df)
    st <- .x2a_str(g("sectionType", i))
    ss <- .x2a_obj(
      id   = .x2a_str(g("subSection_id", i)),
      text = .x2a_str(g("subSection_text", i))
    )
    if (is.null(idx[[st]])) {
      idx[[st]] <- .x2a_obj(
        sectionType = st,
        shellType   = .x2a_str(g("shellType", i)),
        subSections = list(ss)
      )
      order <- c(order, st)
    } else {
      idx[[st]]$subSections <- c(idx[[st]]$subSections, list(ss))
    }
  }
  # Match the twin field order: sectionType, subSections, shellType.
  lapply(order, function(st) {
    s <- idx[[st]]
    .x2a_obj(sectionType = s$sectionType, subSections = s$subSections,
             shellType = s$shellType)
  })
}

.x2a_analysis_sets <- function(path) {
  df <- .x2a_read_sheet(path, "AnalysisSets")
  ids <- unique(as.character(df[["id"]][!is.na(df[["id"]])]))
  out <- list()
  for (id in ids) {
    sub <- df[!is.na(df[["id"]]) & as.character(df[["id"]]) == id, , drop = FALSE]
    g <- .x2a_rowget(sub)
    root <- .x2a_where_root(sub, "")
    out[[length(out) + 1L]] <- .x2a_obj(
      level       = root$level,
      order       = root$order,
      id          = id,
      name        = .x2a_str(g("name", 1L)),
      label       = .x2a_str(g("label", 1L)),
      description = .x2a_str(g("description", 1L)),
      condition   = root$condition,
      compoundExpression = root$compoundExpression
    )
  }
  out
}

.x2a_data_subsets <- function(path) {
  df <- .x2a_read_sheet(path, "DataSubsets")
  ids <- unique(as.character(df[["id"]][!is.na(df[["id"]])]))
  out <- list()
  for (id in ids) {
    sub <- df[!is.na(df[["id"]]) & as.character(df[["id"]]) == id, , drop = FALSE]
    g <- .x2a_rowget(sub)
    root <- .x2a_where_root(sub, "")
    out[[length(out) + 1L]] <- .x2a_obj(
      level       = root$level,
      order       = root$order,
      id          = id,
      name        = .x2a_str(g("name", 1L)),
      label       = .x2a_str(g("label", 1L)),
      description = .x2a_str(g("description", 1L)),
      condition   = root$condition,
      compoundExpression = root$compoundExpression
    )
  }
  out
}

.x2a_analysis_groupings <- function(path) {
  df <- .x2a_read_sheet(path, "AnalysisGroupings")
  ids <- unique(as.character(df[["id"]][!is.na(df[["id"]])]))
  out <- list()
  for (id in ids) {
    sub <- df[!is.na(df[["id"]]) & as.character(df[["id"]]) == id, , drop = FALSE]
    g <- .x2a_rowget(sub)
    # Groups: one per distinct group_id; each group's where-clause tree is built
    # from that group's rows using the "group_" column prefix.
    groups <- list()
    if ("group_id" %in% names(sub)) {
      gids <- unique(as.character(sub[["group_id"]][!is.na(sub[["group_id"]])]))
      for (gid in gids) {
        grows <- sub[!is.na(sub[["group_id"]]) &
                       as.character(sub[["group_id"]]) == gid, , drop = FALSE]
        gg <- .x2a_rowget(grows)
        root <- .x2a_where_root(grows, "group_")
        groups[[length(groups) + 1L]] <- .x2a_obj(
          level       = root$level,
          order       = root$order,
          id          = gid,
          name        = .x2a_str(gg("group_name", 1L)),
          label       = .x2a_str(gg("group_label", 1L)),
          description = .x2a_str(gg("group_description", 1L)),
          condition   = root$condition,
          compoundExpression = root$compoundExpression
        )
      }
    }
    out[[length(out) + 1L]] <- .x2a_obj(
      name             = .x2a_str(g("name", 1L)),
      id               = id,
      label            = .x2a_str(g("label", 1L)),
      description      = .x2a_str(g("description", 1L)),
      dataDriven       = .x2a_bool(g("dataDriven", 1L)),
      groupingDataset  = .x2a_str(g("groupingDataset", 1L)),
      groupingVariable = .x2a_str(g("groupingVariable", 1L)),
      groups           = groups
    )
  }
  out
}

# Shared: parameters keyed by owner id, from a *CodeParameters sheet.
.x2a_params <- function(path, sheet, id_col, template) {
  df <- .x2a_read_sheet(path, sheet)
  params <- list()
  for (i in .x2a_nonblank_rows(df)) {
    g <- .x2a_rowget(df)
    owner <- .x2a_str(g(id_col, i))
    if (template) {
      p <- .x2a_obj(
        name        = .x2a_str(g("parameter_name", i)),
        description = .x2a_str(g("parameter_description", i)),
        label       = .x2a_str(g("parameter_label", i)),
        valueSource = .x2a_str(g("parameter_valueSource", i)),
        value       = .x2a_split(g("parameter_value", i), "|")
      )
    } else {
      p <- .x2a_obj(
        name        = .x2a_str(g("parameter_name", i)),
        description = .x2a_str(g("parameter_description", i)),
        label       = .x2a_str(g("parameter_label", i)),
        value       = .x2a_str(g("parameter_value", i))
      )
    }
    params[[owner]] <- c(params[[owner]], list(p))
  }
  params
}

# Shared: programming code / code templates keyed by owner id. Code specified
# inline ("Code") or by document reference ("DocumentRef"), mirroring
# excel2ars.py; DocumentRef pulls the owner's ProgrammingCode document reference.
.x2a_progcode <- function(path, sheet, id_col, params, docrefs, template) {
  df <- .x2a_read_sheet(path, sheet)
  out <- list()
  for (i in .x2a_nonblank_rows(df)) {
    g <- .x2a_rowget(df)
    owner <- .x2a_str(g(id_col, i))
    spec <- .x2a_str(g("specifiedAs", i))
    code_col <- if (template) "templateCode" else "code"
    params_i <- if (!is.null(params[[owner]])) params[[owner]] else NULL
    if (identical(spec, "Code")) {
      out[[owner]] <- .x2a_obj(
        context    = .x2a_str(g("context", i)),
        code       = .x2a_str(g(code_col, i)),
        parameters = params_i
      )
    } else if (identical(spec, "DocumentRef")) {
      ref <- docrefs$ProgrammingCode[[owner]]
      if (!is.null(ref)) {
        out[[owner]] <- .x2a_obj(
          context     = .x2a_str(g("context", i)),
          documentRef = ref[[1L]],
          parameters  = params_i
        )
      } else {
        cli::cli_warn(
          "Programming code for {.val {owner}} specified as DocumentRef on \\
          {.field {sheet}}, but no matching ProgrammingCode document reference \\
          was found."
        )
      }
    } else {
      cli::cli_warn("Invalid {.field specifiedAs} on {.field {sheet}}: {.val {spec}}")
    }
  }
  out
}

.x2a_methods <- function(path) {
  df <- .x2a_read_sheet(path, "AnalysisMethods")
  mparams  <- .x2a_params(path, "AnalysisMethodCodeParameters", "method_id", TRUE)
  mdocrefs <- .x2a_docrefs(path, "AnalysisMethodDocumentRefs", "method_id")
  mprog    <- .x2a_progcode(path, "AnalysisMethodCodeTemplate", "method_id",
                            mparams, mdocrefs, TRUE)
  ids <- unique(as.character(df[["id"]][!is.na(df[["id"]])]))
  out <- list()
  for (id in ids) {
    sub <- df[!is.na(df[["id"]]) & as.character(df[["id"]]) == id, , drop = FALSE]
    g <- .x2a_rowget(sub)
    operations <- list()
    for (i in .x2a_nonblank_rows(sub)) {
      # Up to N referencedOperationRelationships per operation
      # (operation_referencedResultRelationships{n}_*).
      rels <- list()
      n <- 1L
      repeat {
        idcol <- paste0("operation_referencedResultRelationships", n, "_id")
        if (!(idcol %in% names(sub)) || is.null(.x2a_str(g(idcol, i)))) break
        role <- .x2a_str(g(paste0("operation_referencedResultRelationships", n,
                                  "_referencedOperationRole"), i))
        rels[[length(rels) + 1L]] <- .x2a_obj(
          id = .x2a_str(g(idcol, i)),
          referencedOperationRole = .x2a_term(role, "OperationRole"),
          operationId = .x2a_str(g(paste0("operation_referencedResultRelationships", n,
                                          "_operationId"), i)),
          analysisId  = .x2a_str(g(paste0("operation_referencedResultRelationships", n,
                                          "_analysisId"), i)),
          description = .x2a_str(g(paste0("operation_referencedResultRelationships", n,
                                          "_description"), i))
        )
        n <- n + 1L
      }
      operations[[length(operations) + 1L]] <- .x2a_obj(
        name          = .x2a_str(g("operation_name", i)),
        label         = .x2a_str(g("operation_label", i)),
        id            = .x2a_str(g("operation_id", i)),
        order         = .x2a_num(g("operation_order", i)),
        description   = .x2a_str(g("operation_description", i)),
        resultPattern = .x2a_str(g("operation_resultPattern", i)),
        referencedOperationRelationships = if (length(rels) > 0L) rels else NULL
      )
    }
    out[[length(out) + 1L]] <- .x2a_obj(
      name         = .x2a_str(g("name", 1L)),
      description  = .x2a_str(g("description", 1L)),
      label        = .x2a_str(g("label", 1L)),
      id           = id,
      operations   = operations,
      documentRefs = mdocrefs$Documentation[[id]],
      codeTemplate = if (!is.null(mprog[[id]])) mprog[[id]] else NULL
    )
  }
  out
}

# Per-analysis results from the optional AnalysisResults sheet, keyed by
# analysis id. Each row is one OperationResult with 1+ result groups.
.x2a_analysis_results <- function(path) {
  if (!"AnalysisResults" %in% readxl::excel_sheets(path)) return(list())
  df <- .x2a_read_sheet(path, "AnalysisResults")
  # Distinct resultGroup{n}_ prefixes present on the sheet.
  rg_prefixes <- sort(unique(sub("_.*$", "",
    grep("^resultGroup[0-9]+_", names(df), value = TRUE))))
  out <- list()
  for (i in .x2a_nonblank_rows(df)) {
    g <- .x2a_rowget(df)
    id <- .x2a_str(g("id", i))
    groups <- list()
    for (rg in rg_prefixes) {
      gid <- .x2a_str(g(paste0(rg, "_groupingId"), i))
      if (is.null(gid)) next
      groups[[length(groups) + 1L]] <- .x2a_obj(
        groupingId = gid,
        groupId    = .x2a_str(g(paste0(rg, "_groupId"), i)),
        groupValue = .x2a_str(g(paste0(rg, "_groupValue"), i))
      )
    }
    res <- .x2a_obj(
      operationId    = .x2a_str(g("operation_id", i)),
      rawValue       = .x2a_str(g("rawValue", i)),
      formattedValue = .x2a_str(g("formattedValue", i)),
      resultGroups   = if (length(groups) > 0L) groups else NULL
    )
    out[[id]] <- c(out[[id]], list(res))
  }
  out
}

.x2a_analyses <- function(path) {
  df <- .x2a_read_sheet(path, "Analyses")
  aparams  <- .x2a_params(path, "AnalysisCodeParameters", "analysis_id", FALSE)
  adocrefs <- .x2a_docrefs(path, "AnalysisDocumentRefs", "analysis_id")
  aprog    <- .x2a_progcode(path, "AnalysisProgrammingCode", "analysis_id",
                            aparams, adocrefs, FALSE)
  aresults <- .x2a_analysis_results(path)
  out <- list()
  for (i in .x2a_nonblank_rows(df)) {
    g <- .x2a_rowget(df)
    id <- .x2a_str(g("id", i))

    # orderedGroupings from groupingId{n}/resultsByGroup{n} column pairs.
    ogs <- list()
    n <- 1L
    repeat {
      col <- paste0("groupingId", n)
      gid <- .x2a_str(g(col, i))
      if (is.null(gid)) break
      ogs[[length(ogs) + 1L]] <- .x2a_obj(
        order          = n,
        resultsByGroup = .x2a_bool(g(paste0("resultsByGroup", n), i)),
        groupingId     = gid
      )
      n <- n + 1L
    }

    # referencedAnalysisOperations from referencedOperationId{n}/analysisId{n}.
    # Include every populated pair (do not stop at the first gap, matching
    # excel2ars.py which iterates all matching columns).
    raos <- list()
    rao_cols <- grep("^referencedAnalysisOperations_referencedOperationId[0-9]+$",
                     names(df), value = TRUE)
    rao_cols <- rao_cols[order(as.integer(
      sub("^.*referencedOperationId([0-9]+)$", "\\1", rao_cols)))]
    for (col in rao_cols) {
      rid <- .x2a_str(g(col, i))
      if (is.null(rid)) next
      k <- sub("^.*referencedOperationId([0-9]+)$", "\\1", col)
      raos[[length(raos) + 1L]] <- .x2a_obj(
        referencedOperationRelationshipId = rid,
        analysisId = .x2a_str(g(paste0("referencedAnalysisOperations_analysisId", k), i))
      )
    }

    out[[length(out) + 1L]] <- .x2a_obj(
      name          = .x2a_str(g("name", i)),
      id            = id,
      reason        = .x2a_term(.x2a_str(g("reason", i)), "AnalysisReason"),
      purpose       = .x2a_term(.x2a_str(g("purpose", i)), "AnalysisPurpose"),
      documentRefs  = adocrefs$Documentation[[id]],
      methodId      = .x2a_str(g("method_id", i)),
      version       = .x2a_num(g("version", i)),
      categoryIds   = .x2a_split(g("categoryIds", i), " | "),
      dataset       = .x2a_str(g("dataset", i)),
      variable      = .x2a_str(g("variable", i)),
      analysisSetId = .x2a_str(g("analysisSetId", i)),
      dataSubsetId  = .x2a_str(g("dataSubsetId", i)),
      orderedGroupings = if (length(ogs) > 0L) ogs else NULL,
      referencedAnalysisOperations = if (length(raos) > 0L) raos else NULL,
      programmingCode = if (!is.null(aprog[[id]])) aprog[[id]] else NULL,
      results = if (!is.null(aresults[[id]])) aresults[[id]] else NULL
    )
  }
  out
}

# Displays keyed by display id, with nested displaySections/orderedSubSections.
.x2a_displays <- function(path) {
  df <- .x2a_read_sheet(path, "Displays")
  displays <- list()
  order <- character(0)
  cur_id <- NULL
  cur <- NULL
  cur_sect <- NULL
  flush <- function() {
    if (!is.null(cur_id)) {
      displays[[cur_id]] <<- cur
      if (!(cur_id %in% order)) order <<- c(order, cur_id)
    }
  }
  for (i in .x2a_nonblank_rows(df)) {
    g <- .x2a_rowget(df)
    id <- .x2a_str(g("id", i))
    sst <- .x2a_str(g("displaySection_sectionType", i))
    ss_text <- .x2a_str(g("displaySection_subSection_text", i))
    ss_id   <- .x2a_str(g("displaySection_subSection_id", i))
    ss_ord  <- .x2a_num(g("displaySection_orderedSubSection_order", i))
    ss <- if (is.null(ss_text)) {
      .x2a_obj(order = ss_ord, subSectionId = ss_id)
    } else {
      .x2a_obj(order = ss_ord, subSection = .x2a_obj(id = ss_id, text = ss_text))
    }

    if (!identical(id, cur_id)) {
      flush()
      cur_id <- id
      cur <- .x2a_obj(
        id           = id,
        name         = .x2a_str(g("name", i)),
        description  = .x2a_str(g("description", i)),
        label        = .x2a_str(g("label", i)),
        version      = .x2a_num(g("version", i)),
        displayTitle = .x2a_str(g("displayTitle", i)),
        displaySections = list(.x2a_obj(sectionType = sst, orderedSubSections = list(ss)))
      )
      cur_sect <- sst
    } else if (!identical(sst, cur_sect)) {
      cur$displaySections[[length(cur$displaySections) + 1L]] <-
        .x2a_obj(sectionType = sst, orderedSubSections = list(ss))
      cur_sect <- sst
    } else {
      k <- length(cur$displaySections)
      cur$displaySections[[k]]$orderedSubSections <-
        c(cur$displaySections[[k]]$orderedSubSections, list(ss))
    }
  }
  flush()
  list(displays = displays, order = order)
}

.x2a_output_files <- function(path, sheets) {
  if (!"OutputFiles" %in% sheets) return(list())
  df <- .x2a_read_sheet(path, "OutputFiles")
  files <- list()
  for (i in .x2a_nonblank_rows(df)) {
    g <- .x2a_rowget(df)
    owner <- .x2a_str(g("output_id", i))
    ftype <- .x2a_str(g("fileType", i))
    fs <- .x2a_obj(
      name        = .x2a_str(g("name", i)),
      description = .x2a_str(g("description", i)),
      label       = .x2a_str(g("label", i)),
      location    = .x2a_str(g("location", i)),
      fileType    = .x2a_term(ftype, "OutputFileType")
    )
    files[[owner]] <- c(files[[owner]], list(fs))
  }
  files
}

.x2a_outputs <- function(path) {
  df <- .x2a_read_sheet(path, "Outputs")
  disp <- .x2a_displays(path)
  ofiles   <- .x2a_output_files(path, readxl::excel_sheets(path))
  oparams  <- .x2a_params(path, "OutputCodeParameters", "output_id", FALSE)
  odocrefs <- .x2a_docrefs(path, "OutputDocumentRefs", "output_id")
  oprog    <- .x2a_progcode(path, "OutputProgrammingCode", "output_id",
                            oparams, odocrefs, FALSE)
  out <- list()
  for (i in .x2a_nonblank_rows(df)) {
    g <- .x2a_rowget(df)
    id <- .x2a_str(g("id", i))
    # displays: any column matching display{n}_[Ii]d references a display id.
    dcols <- grep("^display[0-9]+_[Ii]d$", names(df), value = TRUE)
    dcols <- dcols[order(as.integer(sub("^display([0-9]+)_.*$", "\\1", dcols)))]
    displays <- list()
    for (dc in dcols) {
      did <- .x2a_str(g(dc, i))
      if (is.null(did)) next
      ord <- as.integer(sub("^display([0-9]+)_.*$", "\\1", dc))
      displays[[length(displays) + 1L]] <- .x2a_obj(
        order = ord,
        display = disp$displays[[did]]
      )
    }
    out[[length(out) + 1L]] <- .x2a_obj(
      name    = .x2a_str(g("name", i)),
      id      = id,
      version = .x2a_num(g("version", i)),
      fileSpecifications = if (!is.null(ofiles[[id]])) ofiles[[id]] else NULL,
      displays    = if (length(displays) > 0L) displays else NULL,
      categoryIds = .x2a_split(g("categoryIds", i), " | "),
      documentRefs    = odocrefs$Documentation[[id]],
      programmingCode = if (!is.null(oprog[[id]])) oprog[[id]] else NULL
    )
  }
  out
}

# --- public entry point -----------------------------------------------------

#' Convert an ARS Excel workbook to ARS JSON
#'
#' Converts a CDISC Analysis Results Standard (ARS) metadata workbook (`.xlsx`)
#' into an ARS ReportingEvent JSON file. This is an R-native reimplementation of
#' CDISC's Python `excel2ars.py` utility and performs a faithful whole-workbook
#' conversion: every ARS worksheet is mapped, not only the sheets that
#' [readARS()] itself consumes.
#'
#' `readARS()` ingests ARS JSON. Use `ars_xlsx_to_json()` first to convert an
#' Excel workbook, then pass the resulting `.json` file to `readARS()`.
#'
#' @param xlsx_path Path to the ARS Excel workbook (`.xlsx`).
#' @param json_path Optional path for the output JSON file. When `NULL` (the
#'   default), the JSON is written beside `xlsx_path` with the same base name and
#'   a `.json` extension.
#'
#' @return The path to the written JSON file, invisibly.
#'
#' @details
#' The converter maps the full ARS object model: `about`, `studyInfo`, the main
#' and other lists of contents, reference documents, terminology extensions,
#' analysis output categorizations, analysis sets, analysis groupings, data
#' subsets, methods (with operations and code templates), analyses, global
#' display sections and outputs (with displays).
#'
#' Note that the ARS spec evolves; this converter targets the worksheet layout
#' emitted by TFL Designer and the CDISC ARS template and may need updating for
#' future ARS versions.
#'
#' @examples
#' \dontrun{
#' # Convert a workbook and generate ARD scripts from the result.
#' json <- ars_xlsx_to_json("study.xlsx")
#' readARS(json, output_path = tempdir(), adam_path = "adam")
#' }
#'
#' @export
ars_xlsx_to_json <- function(xlsx_path, json_path = NULL) {
  if (is.null(xlsx_path) || length(xlsx_path) != 1L || is.na(xlsx_path) ||
        !nzchar(xlsx_path)) {
    cli::cli_abort("{.arg xlsx_path} must be a single, non-empty file path.")
  }
  if (tolower(tools::file_ext(xlsx_path)) != "xlsx") {
    cli::cli_abort(
      "{.arg xlsx_path} must be an {.file .xlsx} file; got {.path {xlsx_path}}."
    )
  }
  if (!file.exists(xlsx_path)) {
    cli::cli_abort("ARS workbook not found: {.path {xlsx_path}}")
  }

  sheets <- readxl::excel_sheets(xlsx_path)
  required <- c(
    "ReportingEvent", "MainListOfContents", "OtherListsOfContents",
    "AnalysisSets", "AnalysisGroupings", "DataSubsets", "Analyses",
    "AnalysisMethods"
  )
  missing <- setdiff(required, sheets)
  if (length(missing) > 0L) {
    cli::cli_abort(
      "ARS workbook is missing required sheet{?s}: {.val {missing}}"
    )
  }

  if (is.null(json_path)) {
    json_path <- paste0(tools::file_path_sans_ext(xlsx_path), ".json")
  }

  # ReportingEvent header row.
  re <- .x2a_read_sheet(xlsx_path, "ReportingEvent")
  if (nrow(re) == 0L) {
    cli::cli_abort("The {.field ReportingEvent} sheet has no data row.")
  }
  reg <- .x2a_rowget(re)

  cl <- .x2a_content_lists(xlsx_path)

  rptevt <- .x2a_obj(
    about     = .x2a_about(xlsx_path, sheets),
    studyInfo = .x2a_study_info(xlsx_path, sheets),
    name      = .x2a_str(reg("name", 1L)),
    id        = .x2a_str(reg("id", 1L)),
    version   = .x2a_num(reg("version", 1L)),
    description = .x2a_str(reg("description", 1L)),
    label     = .x2a_str(reg("label", 1L)),
    mainListOfContents   = cl$main,
    otherListsOfContents = cl$other,
    referenceDocuments   = .x2a_reference_documents(xlsx_path),
    terminologyExtensions = .x2a_terminology_extensions(xlsx_path),
    analysisOutputCategorizations = .x2a_categorizations(xlsx_path),
    analysisSets      = .x2a_analysis_sets(xlsx_path),
    analysisGroupings = .x2a_analysis_groupings(xlsx_path),
    dataSubsets       = .x2a_data_subsets(xlsx_path),
    methods           = .x2a_methods(xlsx_path),
    analyses          = .x2a_analyses(xlsx_path),
    globalDisplaySections = .x2a_global_display_sections(xlsx_path),
    outputs           = .x2a_outputs(xlsx_path)
  )
  rptevt[["@type"]] <- "ReportingEvent"

  json <- jsonlite::toJSON(rptevt, auto_unbox = TRUE, pretty = TRUE, null = "null")
  writeLines(json, json_path)
  cli::cli_inform("Wrote ARS JSON: {.path {json_path}}")
  invisible(json_path)
}
