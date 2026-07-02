#' Supported code-template valueSource keys
#'
#' The authoritative set of `valueSource` keys that siera can resolve when
#' substituting tokens into an ARS method `codeTemplate`. A method-library
#' template may only reference these keys (plus the operation-ID pattern,
#' see below); any other key is left unsubstituted at generation time and the
#' emitted script will not run.
#'
#' This is the single declared contract for the valueSource vocabulary. It must
#' stay in sync with the `value_sources` list assembled per analysis in
#' [readARS()] (see `R/readARS.R`); the method-library contract test asserts the
#' bundled templates reference only these keys.
#'
#' In addition to the keys returned here, templates may reference the
#' operation-ID family `operation_1`, `operation_2`, ... `operation_N`. These are
#' derived automatically from the method's own `operations` list (in `order`) and
#' never declared as parameters, so they are matched by pattern rather than being
#' listed here.
#'
#' @return Character vector of supported valueSource keys.
#' @keywords internal
.supported_value_sources <- function() {
  c(
    # Complex constructs (assembled argument strings / lists)
    "distinct_list",
    "by_stmt",
    "by_vars",
    "strata_vars",
    "by_listc",
    "by_list",
    # Simple constructs (direct metadata lookups)
    "ana_var",
    "AG_max_dataDriven",
    "DEN_analysisid",
    "AG_denom_var1",
    "AG_var1",
    "AG_var2",
    "AG_var3"
  )
}

#' Regular expression matching the operation-ID valueSource family
#'
#' `operation_1` ... `operation_N` are derived from a method's own operations and
#' are valid valueSources even though they are not listed by
#' [.supported_value_sources()].
#'
#' @return A single regular expression string.
#' @keywords internal
.operation_value_source_pattern <- function() {
  "^operation_[0-9]+$"
}
