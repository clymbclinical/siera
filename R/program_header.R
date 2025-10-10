.generate_program_header <- function(OutputId, Output_Name, date) {
  glue::glue(
    "\n# Programme:    Generate code to produce ARD for {OutputId}\n",
    "# Output:       {Output_Name}\n",
    "# Date created: {date}\n\n",
    .trim = FALSE
  )
}
