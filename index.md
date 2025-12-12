# siera

The Codecov badge updates when the `CODECOV_TOKEN` repository secret is
configured. You can copy the token from the repository settings at
<https://app.codecov.io/gh/clymbclinical/siera/settings> and add it as
the `CODECOV_TOKEN` secret in GitHub. Protected branches (such as
`main`) require this token for uploads to succeed.

## Overview

With siera, users ingest Analysis Results Standard (ARS) metadata and
auto-generate R scripts that, when run with corresponding ADaM datasets,
provide Analysis Results Datasets (ARDs).

The [CDISC Analysis Results
Standard](https://www.cdisc.org/standards/foundational/analysis-results-standard)
is a foundational standard that facilitates automation, reproducibility,
reusability and traceability of analysis results data.

ARS metadata is officially represented using JSON format (though there
is also an Excel representation for easier readability, but the JSON
format is recommended for official ARS usage). Such a JSON file contains
all relevant metadata to be able to calculate the Analysis Results for a
specific Reporting Event. This metadata includes (but is not limited
to):

- Analysis Sets (e.g. SAFFL = “Y”)
- AnalysisGroupings (e.g. group by Treatment)
- DataSubsets (e.g. filter by Treatment-Emergent Adverse Events)
- AnalysisMethods (e.g. calculate ‘n’, ‘Mean’, ‘Min’, ‘Max’, ‘Q1’, ‘Q3’)

Applying all these concepts to ADaM input data, yields Analysis Results
in Dataset format (ARDs).

## Installation

`siera` can be installed from
[CRAN](https://CRAN.R-project.org/package=siera) with:

``` r
install.packages("siera")
```

The development version can be installed from
[Github](https://github.com/clymbclinical/siera) using

``` r
devtools::install_github("clymbclinical/siera")
```

## Usage

The `siera` package has one main function, called `readARS`. This
function takes ARS metadata as input (either JSON or xlsx format), and
makes use of the various metadata pieces to populate R scripts, which an
be run as-is to produce ARDs. One R script is created for each output
(table) as defined in the ARS metadata for the reportingg event.

In order to make use of this function, the following are required as
arguments:

1.  A functional ARS file, representing ARS Metadata for a Reporting
    Event (JSON or xlsx)
2.  An output directory where the R scripts will be placed
3.  A folder containing the related ADaM datasets for the ARDs to be
    generated

See the [Getting
Started](https://clymbclinical.github.io/siera/articles/Getting_started.html)
vignette for examples and more detail on the process.

### More info:

- [US Connect 2025
  paper](https://phuse.s3.eu-central-1.amazonaws.com/Archive/2025/Connect/US/Orlando/PAP_OS20.pdf)
