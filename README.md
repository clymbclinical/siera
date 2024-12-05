
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
devtools::install_github("https://github.com/clymbclinical/siera", 
                         auth_token = "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")
#> Downloading GitHub repo clymbclinical/siera@HEAD
#> glue     (1.7.0 -> 1.8.0) [CRAN]
#> jsonlite (1.8.8 -> 1.8.9) [CRAN]
#> Installing 2 packages: glue, jsonlite
#> Installing packages into 'C:/Users/mbosm/AppData/Local/Temp/RtmpYlUsZA/file54b472fe3d2/siera.Rcheck'
#> (as 'lib' is unspecified)
#> package 'glue' successfully unpacked and MD5 sums checked
#> package 'jsonlite' successfully unpacked and MD5 sums checked
#> 
#> The downloaded binary packages are in
#>  C:\Users\mbosm\AppData\Local\Temp\Rtmp04XK6K\downloaded_packages
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#>          checking for file 'C:\Users\mbosm\AppData\Local\Temp\Rtmp04XK6K\remotes728844f97aad\clymbclinical-siera-6abe6f7/DESCRIPTION' ...  ✔  checking for file 'C:\Users\mbosm\AppData\Local\Temp\Rtmp04XK6K\remotes728844f97aad\clymbclinical-siera-6abe6f7/DESCRIPTION'
#>       ─  preparing 'siera': (360ms)
#>    checking DESCRIPTION meta-information ...  ✔  checking DESCRIPTION meta-information
#>       ─  checking for LF line-endings in source and make files and shell scripts
#>   ─  checking for empty or unneeded directories
#>      Omitted 'LazyData' from DESCRIPTION
#>       ─  building 'siera_0.1.0.tar.gz'
#>      
#> 
#> Installing package into 'C:/Users/mbosm/AppData/Local/Temp/RtmpYlUsZA/file54b472fe3d2/siera.Rcheck'
#> (as 'lib' is unspecified)
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

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
