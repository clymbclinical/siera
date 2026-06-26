#' Browse the bundled analysis-method template library
#'
#' siera ships a plain-text library of analysis-method code templates (the
#' `templateCode` + parameters + valueSource recipes that drive ARD generation)
#' under `inst/method-library`. This accessor lists the available methods and
#' resolves the on-disk location of a method's files, so you do not have to call
#' [system.file()] directly. Each method's stable `id` is the key intended for an
#' ARS file to reference.
#'
#' The human-readable catalog (`METHODS.md`) and the valueSource registry
#' (`constructs.json`) live at the library root, i.e.
#' `system.file("method-library", package = "siera")`.
#'
#' @param id Method id (e.g. `"risk_difference"`). If `NULL` (default), the
#'   available method ids are returned.
#' @returns When `id` is `NULL`, a character vector of available method ids.
#'   Otherwise, the path to that method's directory (containing `method.json` and
#'   `template.R`).
#' @export
#' @examples
#' method_library()
#' method_library("risk_difference")
method_library <- function(id = NULL) {
  lib <- system.file("method-library", package = "siera")
  dirs <- list.dirs(lib, recursive = FALSE)
  dirs <- dirs[file.exists(file.path(dirs, "method.json"))]

  ids <- vapply(
    dirs,
    function(d) jsonlite::fromJSON(file.path(d, "method.json"))$id,
    character(1)
  )
  names(dirs) <- ids

  if (is.null(id)) {
    return(sort(unname(ids)))
  }

  if (!id %in% ids) {
    cli::cli_abort(c(
      "Unknown method library id {.val {id}}.",
      "i" = "Available ids: {.val {sort(unname(ids))}}"
    ))
  }

  unname(dirs[[id]])
}
