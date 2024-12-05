
<!-- README.md is generated from README.Rmd. Please edit that file -->

# siera

<!-- badges: start -->
<!-- badges: end -->

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
#> Using GitHub PAT from the git credential store.
#> Skipping install of 'siera' from a github remote, the SHA1 (6abe6f7c) has not changed since last install.
#>   Use `force = TRUE` to force installation
```

## Example

Using an example JSON ARS file, run readARS() function to produce the
ARD programs.

In this example, the following is used: 1. JSON ARS file: CDISC Pilot
Study Common Safety Displays JSON ARS. 2. get

``` r
library(siera)

json_path <- system.file("extdata", "ARS_V1_Common_Safety_Displays.json", package = "siera")
output_folder <- file.path(paste0(getwd()))
ADaM_folder <- file.path(paste0(getwd(),"/ADAM"))

readARS(json_path, output_folder)
#> Warning in stri_trim_both(string): argument is not an atomic vector; coercing
#> Warning in stri_trim_both(string): argument is not an atomic vector; coercing
#> Warning in stri_trim_both(string): argument is not an atomic vector; coercing
#> Warning in stri_trim_both(string): argument is not an atomic vector; coercing
#> Warning in stri_trim_both(string): argument is not an atomic vector; coercing
#> Warning in stri_trim_both(string): argument is not an atomic vector; coercing
#> Warning in stri_trim_both(string): argument is not an atomic vector; coercing
#> Warning in stri_trim_both(string): argument is not an atomic vector; coercing
#> Warning in stri_trim_both(string): argument is not an atomic vector; coercing
```

Once the R programs are created, they can be individually run, provided
that the ADaM datasets are in the location as provided to the readARS
function. In this example, one of the 5 outputs are Out14-1-1. Its
corresponding R script is ARD_Out14-1-1.R.

We also assume that there are CSV ADaMs in a folder called “adam_csv” in
the current working directory.

In this example, the ARD programs can be called as follows:

``` r

# ARD_program <- file.path(paste0(getwd(),"/ARD_Out14-1-1.R"))
# source(ARD_program)
```
