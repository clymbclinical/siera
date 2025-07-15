
<!-- README.md is generated from README.Rmd. Please edit that file -->

# siera <a href="https://clymbclinical.github.io/siera/"><img src="man/figures/logo.png" align="right" height="138" alt="siera website" /></a>

<!-- badges: start -->

[![CRAN](https://www.r-pkg.org/badges/version/siera)](https://CRAN.R-project.org/package=siera)
[![R-CMD-check](https://github.com/clymbclinical/siera/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/clymbclinical/siera/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

## Overview

With siera, users ingest Analysis Results Standard (ARS) metadata and
auto-generate R scripts that, when run with provided ADaM datasets,
provide Analysis Results Datasets (ARDs).

## Installation

The current version of siera can be installed from
[CRAN](https://CRAN.R-project.org/package=siera) with:

``` r
install.packages("siera")
#> package 'siera' successfully unpacked and MD5 sums checked
#> 
#> The downloaded binary packages are in
#>  C:\Users\mbosm\AppData\Local\Temp\Rtmpob8Yij\downloaded_packages
```

## Requirements

*siera* has two main functions used to ingest ARS metadata - one for
JSON input - *readARS*, and another for Excel input - *readARS_xl*.
Depending on the metadata input type, the appropriate function should be
used.

When run, these functions ingest the provided metadata, and produce R
scripts that, when run as-is (with the applicable ADaM dataset), will
generate an ARD for each output specified in the metadata.

In order to make use of these functions, the following are required as
arguments:

1.  A functional ARS file, representing ARS Metadata for a Reporting
    Event
2.  An output directory where the R scripts will be placed
3.  A folder containing the related ADaM datasets for the ARDs to be
    generated

## Example to get started

``` r
library(siera)
```

The following example focuses on a JSON ARS metadata input file, thus
making use of the *readARS* function. For an example of ingesting the
ARS metadata as an Excel file, see the article [“Read ARS from
Excel”](https://clymbclinical.github.io/siera/articles/Read_Excel.html).

Note the following regarding these examples: For *readARS* (JSON
metadata):

- operations are performed within the *readARS* function, thus
  example-driven

For *readARS_xl* (Excel metadata):

- operations are defined in the metadata, using
  AnalysisMethodCodeTemplate and AnalysisMethodCodeParameters classes,
  basing the operations on functions in the *cards* and *cardx*
  packages.

In order to facilitate the examples, *siera* includes several example
files, which we use throughout the documentation. These include a JSON
ARS file, as well as some csv ADaMs (ADSL and ADAE) which can be run
with the R scripts produced by readARS function. Use the helper
ARS_example() with no arguments to list them or call it with an example
filename to get the path.

``` r
# To see a list of example files:
ARS_example()
#> [1] "ADAE.csv"                           "ADSL.csv"                          
#> [3] "ARS_V1_Common_Safety_Displays.json"

# A temporary path to a specific file:
ARS_example("ARS_V1_Common_Safety_Displays.json")
#> [1] "C:/Users/mbosm/AppData/Local/R/win-library/4.4/siera/extdata/ARS_V1_Common_Safety_Displays.json"
```

To get started with an example of ingesting the ARS JSON metadata,we
will ingest the example JSON ARS file to meta-programme ready-to-run R
scripts, which will produce the ARDs.

``` r
# Path to the the ARS JSON File. 
json_path <- ARS_example("ARS_V1_Common_Safety_Displays.json")

# Path to a folder which will contain the meta-programmed R scripts (feel free to update 
# to a more suitable path)
output_folder <- tempdir()

# this folder contains ADaM datasets to produce ARD (we will use temporary 
# directory tempdir(), but feel free to download the ADaMs required and use the location they are stored in.
# This can be done with e.g. dirname(ARS_example("ADSL.csv"))
ADaM_folder <- tempdir()

# run the readARS function with these 3 parameters.  This creates R scripts (1 for each output in output_folder)
readARS(json_path, output_folder, ADaM_folder)
```

Once the R programs are created, they can be individually run, provided
that the ADaM datasets are in the location as provided to the readARS
function.
