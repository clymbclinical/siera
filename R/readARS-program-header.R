.generate_program_header <- function(OutputId, Output_Name, date) {
  template <- "\n# Programme:    Generate code to produce ARD for outputidhere\n# Output:       outputnamehere\n# Date created: datehere\n\n  "
  code <- gsub("outputidhere", OutputId, template, fixed = TRUE)
  code <- gsub("outputnamehere", Output_Name, code, fixed = TRUE)
  gsub("datehere", date, code, fixed = TRUE)
}

