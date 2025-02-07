#' Get path to ARS JSON example
#'
#' siera comes bundled with some example files in its `inst/extdata`
#' directory. This function make them easy to access.
#'
#' @param path Name of file. If `NULL`, the example files will be listed.
#' @returns A list of example files (if path is NULL), or a file itself if path is used.
#' @export
#' @examples
#' ARS_example()
#' ARS_example("ARS_V1_Common_Safety_Displays.json")
ARS_example <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "siera"))
  } else {
    system.file("extdata", path, package = "siera", mustWork = TRUE)
  }
}
