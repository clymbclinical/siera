#' Get path to ARS example files
#'
#' siera comes bundled with some example files in its `inst/extdata`
#' directory. This function makes them easy to access.
#'
#' siera ships no ADaM data of its own. The example ARS metadata files
#' (\code{.json}) are written against the public CDISC Pilot 01 ADaM datasets
#' distributed by the
#' \href{https://CRAN.R-project.org/package=clinTrialData}{clinTrialData}
#' package (reading them also needs the \code{arrow} package), for example
#' \code{clinTrialData::connect_clinical_data("cdisc_pilot")$adam$read_cnt("adsl.parquet")}.
#' Save the datasets you need to a folder (e.g. as \code{ADSL.csv}) and pass
#' that folder to \code{\link[=readARS]{readARS()}} as \code{adam_path}.
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
