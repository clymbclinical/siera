#' Generate the programme header banner
#'
#' Internal helper that produces the standardised programme banner inserted at
#' the top of each generated ARD script.
#' @param OutputId Identifier of the output being generated.
#' @param Output_Name Human-readable name of the output.
#' @param date Timestamp used for the header creation date.
#'
#' @return Character string containing the formatted programme header.
#' @keywords internal
.generate_program_header <- function(OutputId, Output_Name, date) {
  template <- "\n# Programme:    Generate code to produce ARD for outputidhere\n# Output:       outputnamehere\n# Date created: datehere\n\n  "
  code <- gsub("outputidhere", OutputId, template, fixed = TRUE)
  code <- gsub("outputnamehere", Output_Name, code, fixed = TRUE)
  gsub("datehere", date, code, fixed = TRUE)
}
