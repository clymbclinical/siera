#' Resolve an external method code-template reference (ARS `documentRef`)
#'
#' The ARS standard lets a method's `codeTemplate` point at an *external*
#' document instead of carrying the template text inline: `codeTemplate.documentRef`
#' is a `DocumentReference` whose `referenceDocumentId` resolves to an entry in the
#' reporting event's `referenceDocuments[]`, and whose optional `pageRefs`
#' (`refType = "NamedDestination"`, `pageNames = ["<method id>"]`) selects one
#' method out of a multi-method document. siera consumes that chain here so an ARS
#' author can *reference* a library method by id rather than copy-pasting its
#' `templateCode` (and parameters) inline - the copy-paste being exactly how the
#' legacy workbooks silently drifted out of sync.
#'
#' Two reference-document formats are supported, dispatched on what the
#' `ReferenceDocument.location` resolves to:
#'
#' * a **siera method manifest** (`.json`) - a superset of a per-method
#'   `method.json` that carries `templateCode` **and** `parameters` (and
#'   `operations`) for one or many methods. The bundled
#'   `inst/method-library/method-library.json` catalog is exactly this shape, so
#'   the whole method library ships as a referenceable document. A method is
#'   selected by `page_names` (the named destination = the method `id`); a
#'   single-method manifest needs no `page_names`.
#' * a **bare code file** (`.R` / `.txt`) - the template body only (the
#'   ARS-pure / Option A form). The ARS must then keep its `codeTemplate.parameters`
#'   inline; this resolver returns `parameters = NULL` for that case.
#'
#' Security / reproducibility: v1 resolves **local paths only** - relative
#' (against the directory of the ARS input file) or absolute. Remote URIs
#' (`http(s)://`, etc.) are rejected with an error.
#'
#' @param reference_document_id Scalar character; the `documentRef.referenceDocumentId`.
#' @param page_names Character vector of `pageRefs` named-destination page names
#'   (method ids). May be `NULL`/empty for a single-method document.
#' @param reference_documents A data frame / tibble of the reporting event's
#'   `referenceDocuments` with at least `id` and `location` columns.
#' @param ars_dir Directory of the ARS input file; relative `location`s resolve
#'   against it.
#'
#' @return A list with `templateCode` (scalar character) and `parameters` (a
#'   tibble with `name`, `description`, `valueSource`, or `NULL` for a bare code
#'   file).
#' @keywords internal
.resolve_method_documentref <- function(reference_document_id,
                                        page_names = NULL,
                                        reference_documents,
                                        ars_dir) {
  if (is.null(reference_document_id) ||
      length(reference_document_id) == 0 ||
      is.na(reference_document_id) ||
      !nzchar(reference_document_id)) {
    cli::cli_abort("A method {.field codeTemplate.documentRef} has an empty {.field referenceDocumentId}.")
  }
  reference_document_id <- as.character(reference_document_id)[1]

  if (is.null(reference_documents) ||
      nrow(reference_documents) == 0 ||
      !"id" %in% names(reference_documents) ||
      !reference_document_id %in% reference_documents$id) {
    cli::cli_abort(c(
      "Method {.field codeTemplate.documentRef} points at reference document {.val {reference_document_id}}, which is not declared in {.field referenceDocuments}.",
      "i" = "Add a {.field referenceDocuments} entry with that {.field id} and a {.field location}."
    ))
  }

  location <- reference_documents$location[match(reference_document_id, reference_documents$id)]
  if (is.null(location) || is.na(location) || !nzchar(location)) {
    cli::cli_abort("Reference document {.val {reference_document_id}} has no {.field location}.")
  }

  if (.is_remote_uri(location)) {
    cli::cli_abort(c(
      "Reference document {.val {reference_document_id}} has a remote {.field location} ({.url {location}}).",
      "i" = "siera resolves local method-template references only; remote fetching is not supported."
    ))
  }

  path <- if (.is_absolute_path(location)) location else file.path(ars_dir, location)
  if (!file.exists(path)) {
    cli::cli_abort(c(
      "Reference document {.val {reference_document_id}} location does not exist: {.path {path}}.",
      "i" = "Relative locations resolve against the directory of the ARS input file."
    ))
  }

  ext <- tolower(tools::file_ext(path))
  if (ext == "json") {
    .resolve_method_from_manifest(path, page_names, reference_document_id)
  } else if (ext %in% c("r", "txt")) {
    list(
      templateCode = .read_template_code_file(path),
      parameters   = NULL
    )
  } else {
    cli::cli_abort(c(
      "Reference document {.val {reference_document_id}} has unsupported type {.val {ext}}.",
      "i" = "Use a {.file .json} method manifest or a bare {.file .R}/{.file .txt} code file."
    ))
  }
}

#' Select and validate a method from a siera method manifest
#'
#' @param path Path to the manifest JSON.
#' @param page_names Named-destination page names (method ids); may be NULL/empty.
#' @param reference_document_id Id used only for error messages.
#' @return A list with `templateCode` and `parameters` (tibble or NULL).
#' @keywords internal
.resolve_method_from_manifest <- function(path, page_names, reference_document_id) {
  manifest <- jsonlite::fromJSON(path, simplifyDataFrame = FALSE, simplifyVector = FALSE)
  methods <- manifest$methods
  if (is.null(methods) || length(methods) == 0) {
    cli::cli_abort("Method manifest {.path {path}} has no {.field methods}.")
  }

  ids <- vapply(methods, function(m) if (is.null(m$id)) NA_character_ else as.character(m$id), character(1))
  page_names <- page_names[!is.na(page_names) & nzchar(page_names)]

  selected <- if (length(page_names) == 0) {
    if (length(methods) != 1) {
      cli::cli_abort(c(
        "Reference document {.val {reference_document_id}} manifest holds {length(methods)} methods but the {.field documentRef} has no {.field pageRefs} naming one.",
        "i" = "Add a {.field pageRefs} entry with {.code refType = \"NamedDestination\"} and {.code pageNames = [\"<method id>\"]}.",
        "i" = "Available method ids: {.val {ids}}"
      ))
    }
    methods[[1]]
  } else {
    idx <- match(page_names[1], ids)
    if (is.na(idx)) {
      cli::cli_abort(c(
        "Reference document {.val {reference_document_id}} manifest has no method with id {.val {page_names[1]}}.",
        "i" = "Available method ids: {.val {ids}}"
      ))
    }
    methods[[idx]]
  }

  if (is.null(selected$templateCode) || !nzchar(selected$templateCode)) {
    cli::cli_abort("Manifest method {.val {selected$id}} in {.path {path}} has no {.field templateCode}.")
  }
  template_code <- gsub("\r|_x000D_", "", selected$templateCode)

  parameters <- .manifest_parameters_tibble(selected$parameters, selected$id, path)

  list(templateCode = template_code, parameters = parameters)
}

#' Convert manifest `parameters` to a tibble and validate their valueSources
#'
#' @param params The manifest method's `parameters` (a list of named lists), or NULL.
#' @param method_id Id used only for error messages.
#' @param path Manifest path used only for error messages.
#' @return A tibble with `name`, `description`, `valueSource`, or `NULL` when the
#'   method declares no parameters.
#' @keywords internal
.manifest_parameters_tibble <- function(params, method_id, path) {
  if (is.null(params) || length(params) == 0) {
    return(NULL)
  }

  name        <- vapply(params, function(p) if (is.null(p$name)) NA_character_ else as.character(p$name), character(1))
  valueSource <- vapply(params, function(p) if (is.null(p$valueSource)) NA_character_ else as.character(p$valueSource), character(1))
  description <- vapply(params, function(p) if (is.null(p$description)) NA_character_ else as.character(p$description), character(1))

  supported <- c(.supported_value_sources())
  op_pattern <- .operation_value_source_pattern()
  ok <- valueSource %in% supported | grepl(op_pattern, valueSource)
  if (any(!ok)) {
    bad <- unique(valueSource[!ok])
    cli::cli_abort(c(
      "Manifest method {.val {method_id}} in {.path {path}} uses unsupported valueSource{?s}: {.val {bad}}.",
      "i" = "Supported valueSources: {.val {supported}} (plus the {.code operation_N} family)."
    ))
  }

  tibble::tibble(
    name        = name,
    description = description,
    valueSource = valueSource
  )
}

#' Read a bare code-template file (Option A) and strip carriage returns
#'
#' @param path Path to a `.R` / `.txt` template file.
#' @return Scalar character of the file contents.
#' @keywords internal
.read_template_code_file <- function(path) {
  code <- paste(readLines(path, warn = FALSE), collapse = "\n")
  gsub("\r|_x000D_", "", code)
}

#' Normalise the reporting event's `referenceDocuments` into a tibble
#'
#' Accepts whatever `jsonlite::fromJSON()` produced for `referenceDocuments`
#' (a data frame under `simplifyDataFrame`, a list of records, or `NULL`) and
#' returns a tibble with `id`, `name`, `location`.
#'
#' @param reference_documents The parsed `referenceDocuments` element, or `NULL`.
#' @return A tibble with columns `id`, `name`, `location`.
#' @keywords internal
.extract_reference_documents <- function(reference_documents) {
  empty <- tibble::tibble(id = character(0), name = character(0), location = character(0))
  if (is.null(reference_documents)) {
    return(empty)
  }
  df <- if (is.data.frame(reference_documents)) {
    if (nrow(reference_documents) == 0) return(empty)
    reference_documents
  } else {
    if (length(reference_documents) == 0) return(empty)
    dplyr::bind_rows(lapply(reference_documents, function(r) {
      tibble::tibble(
        id       = if (!is.null(r$id)) as.character(r$id) else NA_character_,
        name     = if (!is.null(r$name)) as.character(r$name) else NA_character_,
        location = if (!is.null(r$location)) as.character(r$location) else NA_character_
      )
    }))
  }
  tibble::tibble(
    id       = if (!is.null(df$id)) as.character(df$id) else NA_character_,
    name     = if (!is.null(df$name)) as.character(df$name) else NA_character_,
    location = if (!is.null(df$location)) as.character(df$location) else NA_character_
  )
}

#' Extract a method's `documentRef.referenceDocumentId` (JSON shape)
#'
#' @param document_ref The `codeTemplate$documentRef` data frame from jsonlite.
#' @param i Method row index.
#' @return Scalar character id, or `NA_character_`.
#' @keywords internal
.documentref_reference_id <- function(document_ref, i) {
  if (is.null(document_ref$referenceDocumentId)) {
    return(NA_character_)
  }
  as.character(document_ref$referenceDocumentId[i])
}

#' Extract a method's `documentRef.pageRefs` named-destination page names
#'
#' @param document_ref The `codeTemplate$documentRef` data frame from jsonlite.
#' @param i Method row index.
#' @return Character vector of page names (method ids); `character(0)` if none.
#' @keywords internal
.documentref_page_names <- function(document_ref, i) {
  if (is.null(document_ref$pageRefs)) {
    return(character(0))
  }
  pr <- document_ref$pageRefs[[i]]
  if (is.null(pr)) {
    return(character(0))
  }
  if (is.data.frame(pr)) {
    if (!"pageNames" %in% names(pr)) return(character(0))
    return(unlist(pr$pageNames, use.names = FALSE))
  }
  unlist(lapply(pr, function(x) x$pageNames), use.names = FALSE)
}

#' Split an XLSX `pageRef_pages` cell into page names
#'
#' The CDISC ARS xlsx representation stores `pageRefs.pageNames` in a single
#' `pageRef_pages` cell; multiple names are separated by a comma or semicolon.
#'
#' @param x A scalar `pageRef_pages` cell value.
#' @return Character vector of page names; `character(0)` when empty.
#' @keywords internal
.split_page_names <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(as.character(x))) {
    return(character(0))
  }
  trimws(unlist(strsplit(as.character(x), "[,;]")))
}

#' Is a location a remote URI?
#'
#' Any explicit URI scheme followed by `://` (e.g. `http://`, `https://`,
#' `ftp://`, `file://`) counts as remote and is rejected in v1. Windows drive
#' paths (`C:\\...`, `C:/...`) and POSIX paths do not match.
#'
#' @param location Scalar character.
#' @return `TRUE` if remote.
#' @keywords internal
.is_remote_uri <- function(location) {
  grepl("^[A-Za-z][A-Za-z0-9+.-]*://", location)
}

#' Is a path absolute (so it must not be joined to the ARS directory)?
#'
#' Matches POSIX absolute paths (`/...`) and Windows paths, both drive-letter
#' (`C:\\...`, `C:/...`) and UNC/leading-separator forms.
#'
#' @param path Scalar character.
#' @return `TRUE` if absolute.
#' @keywords internal
.is_absolute_path <- function(path) {
  grepl("^([A-Za-z]:[\\\\/]|[\\\\/])", path)
}
