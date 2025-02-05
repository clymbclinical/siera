
<!-- README.md is generated from README.Rmd. Please edit that file -->

# siera

<!-- badges: start -->
<!-- [![CRAN -->
<!-- status](https://www.r-pkg.org/badges/version/siera)](https://CRAN.R-project.org/package=siera) -->

[![R-CMD-check](https://github.com/clymbclinical/siera/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/clymbclinical/siera/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Are you looking for a way to automate TFLs?

With siera, users ingest Analysis Results Standard - ARS (a CDISC
Foundational standard) metadata and auto-generate R scripts that, when
run in with provided ADaM datasets, provide Analysis Results Datasets
(ARDs).

In order to use the readARS() function, users will need to provide the
following:

1.  A Functional JSON file, representing ARS Metadata for a Reporting
    Event (to get started, see TFL Designer)
2.  An output directory where the R scripts will be placed
3.  A folder containing the related ADaM datasets for the ARDs to be
    generated

## Installation

The current version (0.1.0) of siera can be installed from
[GitHub](https://github.com/clymbclinical/siera) with:

``` r
# install.packages("devtools")
Sys.unsetenv("GITHUB_PAT")
devtools::install_github("https://github.com/clymbclinical/siera")
```

## Example

Using an example JSON ARS file, run readARS() function to produce the
ARD programs.

In this example, the following is used: 1. JSON ARS file from CDISC
Pilot Study

``` r
library(siera)

# the ARS JSON File. 
json_path <- system.file("extdata", "ARS_V1_Common_Safety_Displays.json", package = "siera") 

# store ARD scripts in this folder:
output_folder <- file.path(paste0(getwd(),"/inst")) 

# this folder contains ADaM datasets to produce ARD:
ADaM_folder <- file.path(paste0(getwd(),"/inst/extdata")) 

# run the readARS function with these 3 parameters.  This creates R scripts (1 for each output in output_folder)
readARS(json_path, output_folder, adam_path = ADaM_folder)
```

Once the R programs are created, they can be individually run, provided
that the ADaM datasets are in the location as provided to the readARS
function.
