
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

In this example, the following is used: 1. JSON ARS file: CDISC Pilot
Study Common Safety Displays JSON ARS. 2. ADaM datasets: CDISC Pilot
Study Common Safety Displays - ADSL, ADAE, ADVS

``` r
library(siera)

# the ARS JSON File:
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
function. In this example, one of the 5 outputs are Out14-1-1. Its
corresponding R script is ARD_Out14-1-1.R.

We also assume that there are CSV ADaMs in a folder called “adam_csv” in
the current working directory.

In this example, an ARD programs can be called as follows:

``` r
# location to one of the created R scripts:
ARD_program <- file.path(paste0(output_folder,"/ARD_Out14-1-1.R"))

# run the program as-is.  This ingests the ADaM dataset(s) in the ADAM_folder location listed earlier.
source(ARD_program)
#> Warning in chisq.test(tab): Chi-squared approximation may be incorrect
#> Warning in chisq.test(tab): Chi-squared approximation may be incorrect
```

Once the ARD program(s) is run, the ARD is created. Among all the helper
objects created, the ARD can be identified as the “ARD_outputname”
object. Let’s view this object created by the previous command:

``` r
print(ARD_Out14_1_1)
#> # A tibble: 113 × 9
#>    Group1     res AnalsysisId MethodId OperationId OutputId pattern Group2 disp 
#>    <chr>    <dbl> <chr>       <chr>    <chr>       <chr>    <chr>   <chr>  <chr>
#>  1 Placebo  86    An01_05_SA… Mth01_C… Mth01_CatV… Out14-1… (N=XX)  <NA>   (N=8…
#>  2 Xanomel… 84    An01_05_SA… Mth01_C… Mth01_CatV… Out14-1… (N=XX)  <NA>   (N=8…
#>  3 Xanomel… 84    An01_05_SA… Mth01_C… Mth01_CatV… Out14-1… (N=XX)  <NA>   (N=8…
#>  4 Placebo  86    An03_01_Ag… Mth02_C… Mth02_Cont… Out14-1… XX      <NA>   86   
#>  5 Xanomel… 84    An03_01_Ag… Mth02_C… Mth02_Cont… Out14-1… XX      <NA>   84   
#>  6 Xanomel… 84    An03_01_Ag… Mth02_C… Mth02_Cont… Out14-1… XX      <NA>   84   
#>  7 Placebo  75.2  An03_01_Ag… Mth02_C… Mth02_Cont… Out14-1… XX.X    <NA>   75.2 
#>  8 Xanomel… 74.4  An03_01_Ag… Mth02_C… Mth02_Cont… Out14-1… XX.X    <NA>   74.4 
#>  9 Xanomel… 75.7  An03_01_Ag… Mth02_C… Mth02_Cont… Out14-1… XX.X    <NA>   75.7 
#> 10 Placebo   8.59 An03_01_Ag… Mth02_C… Mth02_Cont… Out14-1… (XX.XX) <NA>   (8.5…
#> # ℹ 103 more rows
```
