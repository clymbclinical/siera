.generate_library_code <- function() {
  calls <- rlang::exprs(
    library(dplyr),
    library(readxl),
    library(readr),
    library(cards),
    library(cardx),
    library(broom),
    library(parameters),
    library(tidyr),
    library(magrittr)
  )

  library_code <- vapply(
    calls,
    function(expr) .expr_to_code(expr),
    character(1)
  )

  paste0("# load libraries ----\n", paste(library_code, collapse = "\n"), "\n")
}
