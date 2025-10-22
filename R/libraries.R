#' Generate library loading code
#'
#' Internal helper that returns the boilerplate code used to load the packages
#' required by generated ARD scripts.
#'
#' @return Character string containing the library load block.
#' @keywords internal
#'
.generate_library_code <- function() {
  "# load libraries ----
    library(dplyr)
    library(readxl)
    library(readr)
    library(cards)
    library(cardx)
    library(broom)
    library(parameters)
    library(tidyr)
  "
}
