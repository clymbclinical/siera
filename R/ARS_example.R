#' Get path to ARS example files
#'
#' siera comes bundled with some example files in its `inst/extdata`
#' directory. This function makes them easy to access.
#'
#' The example ARS metadata files (\code{.json}) are paired with example ADaM
#' datasets in CSV format (\code{ADSL.csv}, \code{ADAE.csv}, \code{ADVS.csv},
#' \code{ADEXSUM.csv} and \code{ADZSDER.csv}). ADSL, ADAE, ADVS and ADEXSUM
#' are derived from the public CDISC Pilot 01 study (as distributed by the
#' \href{https://CRAN.R-project.org/package=clinTrialData}{clinTrialData}
#' package), with a few sponsor-style variables added where the example ARS
#' metadata needs them; ADZSDER is a small synthetic dataset. The script that
#' rebuilds them is kept in the package source under
#' \code{data-raw/adam_example_data.R}.
#'
#' @param path Name of file. If `NULL`, the example files will be listed.
#' @returns A list of example files (if path is NULL), or a file itself if path is used.
#' @export
#' @examples
#' ARS_example()
#' ARS_example("Common_Safety_Displays_cards.json")
ARS_example <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "siera"))
  } else {
    system.file("extdata", path, package = "siera", mustWork = TRUE)
  }
}
