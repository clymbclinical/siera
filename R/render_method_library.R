#' Render the method-template library as a human-readable Markdown catalog
#'
#' Internal generator that turns the plain-text method library
#' (`inst/method-library/`: per-method `method.json` + `template.R`, plus
#' `constructs.json`) into a single Markdown catalog. The committed
#' `inst/method-library/METHODS.md` is produced by this function; a contract test
#' asserts the committed file matches the rendered output so the human-readable
#' view can never drift from the source of truth.
#'
#' Regenerate the committed catalog with:
#' `writeLines(siera:::.render_method_library_md(), "inst/method-library/METHODS.md")`
#'
#' @param lib_dir Path to the method-library directory. Defaults to the installed
#'   (or `load_all()`ed) copy via `system.file()`.
#' @return Character vector of Markdown lines.
#' @keywords internal
.render_method_library_md <- function(lib_dir = NULL) {
  if (is.null(lib_dir)) {
    lib_dir <- system.file("method-library", package = "siera")
  }

  method_dirs <- list.dirs(lib_dir, recursive = FALSE)
  method_dirs <- sort(method_dirs[file.exists(file.path(method_dirs, "method.json"))])
  metas <- lapply(method_dirs, function(d) {
    m <- jsonlite::fromJSON(file.path(d, "method.json"), simplifyVector = TRUE)
    m$.template <- paste(readLines(file.path(d, "template.R"), warn = FALSE), collapse = "\n")
    m
  })

  na_dash <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) "-" else as.character(x)

  out <- c(
    "# siera method-template catalog",
    "",
    "_Generated from `inst/method-library/` by `.render_method_library_md()` - do not edit by hand._",
    "_Regenerate: `writeLines(siera:::.render_method_library_md(), \"inst/method-library/METHODS.md\")`._",
    "",
    "siera generates ARD code by substituting placeholder tokens in a method's",
    "`templateCode` with values resolved per analysis from ARS metadata. Each method",
    "below is one reusable recipe; its `id` is the stable key an ARS file can",
    "reference.",
    "",
    "## Methods",
    "",
    "| id | label | status | operations | verified against |",
    "|----|-------|--------|------------|------------------|"
  )
  for (m in metas) {
    ops <- paste(m$operations$name, collapse = ", ")
    out <- c(out, sprintf("| `%s` | %s | %s | %s | %s |",
                          m$id, m$label, m$status, ops, na_dash(m$verified_against)))
  }

  cons <- jsonlite::fromJSON(file.path(lib_dir, "constructs.json"), simplifyVector = TRUE)$constructs
  out <- c(out, "",
           "## Supported valueSources",
           "",
           "| valueSource | type | assumed token | details |",
           "|-------------|------|---------------|---------|")
  for (i in seq_len(nrow(cons))) {
    out <- c(out, sprintf("| `%s` | %s | `%s` | %s |",
                          cons$valueSource[i], cons$type[i],
                          cons$assumed_parameter_name[i], cons$details[i]))
  }

  for (m in metas) {
    out <- c(out, "", "---", "",
             sprintf("## `%s` - %s", m$id, m$name),
             "",
             m$description,
             "",
             sprintf("**Status:** %s &nbsp; **Verified against:** %s",
                     m$status, na_dash(m$verified_against)),
             "",
             "**Operations**",
             "",
             "| order | stat_name | label | resultPattern |",
             "|-------|-----------|-------|---------------|")
    ops <- m$operations[order(m$operations$order), , drop = FALSE]
    for (i in seq_len(nrow(ops))) {
      out <- c(out, sprintf("| %s | `%s` | %s | `%s` |",
                            ops$order[i], ops$name[i], ops$label[i], ops$resultPattern[i]))
    }
    out <- c(out, "",
             "**Parameters**",
             "",
             "| token | valueSource | label | description |",
             "|-------|-------------|-------|-------------|")
    pp <- m$parameters
    for (i in seq_len(nrow(pp))) {
      out <- c(out, sprintf("| `%s` | `%s` | %s | %s |",
                            pp$name[i], pp$valueSource[i], pp$label[i], pp$description[i]))
    }
    out <- c(out, "",
             "**Template**",
             "",
             "```r",
             strsplit(m$.template, "\n", fixed = TRUE)[[1]],
             "```")
  }

  out
}

#' Render the method-template library as a referenceable JSON manifest catalog
#'
#' Internal generator that turns the plain-text method library into a single
#' machine-readable **method manifest** (`inst/method-library/method-library.json`):
#' a superset of each `method.json` that also carries the `templateCode` body, so
#' the whole library ships as one ARS-referenceable document. An ARS file can then
#' point a method's `codeTemplate.documentRef` at this catalog and select a method
#' by its `id` via a `pageRefs` named destination (resolved by
#' [.resolve_method_documentref()]). The committed catalog is produced by this
#' function and a contract test asserts it matches, so the catalog can never drift
#' from the per-method source files.
#'
#' Regenerate the committed catalog with:
#' `writeLines(siera:::.render_method_library_json(), "inst/method-library/method-library.json")`
#'
#' @param lib_dir Path to the method-library directory. Defaults to the installed
#'   (or `load_all()`ed) copy via `system.file()`.
#' @return Character scalar of pretty-printed JSON (one trailing newline-free
#'   string; write with [writeLines()]).
#' @keywords internal
.render_method_library_json <- function(lib_dir = NULL) {
  if (is.null(lib_dir)) {
    lib_dir <- system.file("method-library", package = "siera")
  }

  method_dirs <- list.dirs(lib_dir, recursive = FALSE)
  method_dirs <- sort(method_dirs[file.exists(file.path(method_dirs, "method.json"))])

  methods <- lapply(method_dirs, function(d) {
    m <- jsonlite::fromJSON(file.path(d, "method.json"), simplifyDataFrame = FALSE, simplifyVector = TRUE)
    template <- paste(readLines(file.path(d, "template.R"), warn = FALSE), collapse = "\n")
    # Order keys deterministically; carry the catalog identity plus everything an
    # ARS-referencing engine needs (operations + parameters + templateCode).
    list(
      id           = m$id,
      name         = m$name,
      label        = m$label,
      description  = m$description,
      status       = m$status,
      operations   = m$operations,
      parameters   = m$parameters,
      templateCode = template
    )
  })

  catalog <- list(
    `$schema`   = "siera-method-manifest/v1",
    name        = "siera method library",
    description = paste(
      "Referenceable catalog of siera analysis-method code templates.",
      "Point an ARS method codeTemplate.documentRef at this file and select a",
      "method by its id via a pageRefs NamedDestination. Generated from",
      "inst/method-library/ - do not edit by hand."
    ),
    context = "R (siera)",
    methods = methods
  )

  jsonlite::toJSON(catalog, pretty = TRUE, auto_unbox = TRUE, null = "null")
}
