% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ARS_example.R
\name{ARS_example}
\alias{ARS_example}
\title{Get path to ARS example files}
\usage{
ARS_example(path = NULL)
}
\arguments{
\item{path}{Name of file. If `NULL`, the example files will be listed.}
}
\value{
A list of example files (if path is NULL), or a file itself if path is used.
}
\description{
siera comes bundled with some example files in its `inst/extdata`
directory. This function make them easy to access.
}
\examples{
ARS_example()
ARS_example("ARS_V1_Common_Safety_Displays.json")
}
