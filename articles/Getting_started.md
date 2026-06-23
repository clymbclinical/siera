# Getting Started

``` r

library(siera)
```

We will use the following example to get started:

A modified *CDISC Common Safety Displays* ARS metadata (xlsx format,
with example dynamic AnalysisMethodCodeTemplate R code handling
operations, based on functions from the `cards` package)

In order to facilitate the example, *siera* includes several example
files, which we use throughout the documentation. These include several
ARS files (json and xlsx), as well as csv-format ADaMs which can be run
with the R scripts produced by readARS function. Use the helper
ARS_example() with no arguments to list them or call it with an example
filename to get the path.

### What you will produce

Before we start, it helps to see where we are headed. The end product of
*siera* is an **Analysis Results Dataset (ARD)** - one result per row,
in machine-readable form. Below is the ARD for a Demographics output,
produced by running one of the example scripts that *siera* generates
(more on running scripts further down). This is what we are building
toward:

``` r

# Run a ready-made example script shipped with siera; the result is the object `ARD`
source(ARD_script_example("ARD_Out14-1-1.R"))
head(ARD)
#>   group1 group1_level group2 group2_level variable variable_level stat_name
#> 1   <NA>           NA   <NA>           NA   TRT01A        Placebo         n
#> 2   <NA>           NA   <NA>           NA   TRT01A      Xanomeli…         n
#> 3   <NA>           NA   <NA>           NA   TRT01A      Xanomeli…         n
#> 4 TRT01A      Placebo   <NA>           NA      AGE             NA         N
#> 5 TRT01A      Placebo   <NA>           NA      AGE             NA      mean
#> 6 TRT01A      Placebo   <NA>           NA      AGE             NA        sd
#>   stat_label   stat
#> 1          n     86
#> 2          n     84
#> 3          n     84
#> 4          N     86
#> 5       Mean 75.209
#> 6         SD   8.59
```

Each row carries the statistic together with metadata (such as
`AnalysisId` and the grouping columns) that traces it back to the ARS
definition. The [Concepts and
conventions](https://clymbclinical.github.io/siera/articles/concepts.md)
vignette explains these traceability columns in more detail.

``` r

# To see a list of example files:
ARS_example()
#>  [1] "ADAE.csv"                          "ADEXSUM.csv"                      
#>  [3] "ADSL.csv"                          "ADVS.csv"                         
#>  [5] "ADZSDER.csv"                       "cards_constructs.xlsx"            
#>  [7] "Common_Safety_Displays_cards.xlsx" "exampleARS_1.json"                
#>  [9] "exampleARS_1a.json"                "exampleARS_2.json"                
#> [11] "exampleARS_2.xlsx"                 "exampleARS_2a.xlsx"               
#> [13] "exampleARS_3.json"                 "exampleARS_3.xlsx"                
#> [15] "exampleARS_4.json"                 "exampleARS_5.json"                
#> [17] "exampleARS_5.xlsx"                 "exampleARS_6.json"                
#> [19] "exampleARS_6.xlsx"                 "test_cards.json"

# A temporary path to a specific file:
ARS_example("exampleARS_1.json")
#> [1] "/home/runner/work/_temp/Library/siera/extdata/exampleARS_1.json"
```

### Reading in ARS metadata - what to expect

The ARS metadata contains all ARS information for the Reporting Event.
This information includes:

- *mainListOfContents* (links outputs to their respective analyses)
- *otherListsOfContents* (contains Output metadata)
- *dataSubsets* (filters data for individual analyses)
- *analysisSets* (filters data to get the Population Set for the Output)
- *analysisGroupings* (groups data according to analysis specifications)
- *analyses* (contains linking information to all components required
  for calculation of results)
- *methods* (describes operations to be performed on data to get
  results)

For a fuller treatment of what each of these sections is for, and how
they flow through *siera*, see the [Concepts and
conventions](https://clymbclinical.github.io/siera/articles/concepts.md)
vignette.

### Example

The *readARS* function reads in a completed ARS metadata file, and
generates R scripts for each Output defined in the file. Below is an
example of how this can be done using a ready-to-use ARS metadata Excel
file (note that this is meant as an example. Official ARS metadata is in
json format, which can also be passed to the *readARS* function):

``` r

# Path to the Excel ARS metadata file:
ARS_path <- ARS_example("Common_Safety_Displays_cards.xlsx")

# Path to a folder which will contain the Output meta-programmed R scripts (recommended to update
# to a more suitable local path)
output_folder <- tempdir()

# Path to the folder containing ADaM datasets in csv format (we will use temporary
# directory tempdir() to make the code run in this vignette, but it's recommended to
# 1. download the ADaMs required (csv ADSL and ADAE available using e.g. ARS_example("ADSL.csv"))
# 2. store it in a folder (the directly downloaded location can be found using dirname(ARS_example("ADSL.csv")), for example.  Use this location, or manually store the ADaM somewhere else)
# 3. use the folder path instead of tempdir() below.
ADaM_folder <- tempdir()

# run the readARS function with these 3 parameters.  This creates R scripts
# (1 for each Output in output_folder)
readARS(ARS_path, output_folder, ADaM_folder)
```

You should now have 5 R scripts (named ARD_Out14-1-1.R,
ARD_Out14-3-1-1.R, ARD_Out14-3-2-1.R, ARD_Out14-3-3-1a.R, and
ARD_Out14-3-3-1b.R) in the folder specified as *output_folder*. You can
execute any of these 5 R scripts as-is (assuming the ADaM required for
this script is available in the *ADaM_folder*), and the result will be
an ARD (one result per row format) for each of the scripts.

The generated scripts are portable across operating systems: any
Windows-style backslashes in the ADaM path are normalised to forward
slashes, so a script generated on Windows runs unchanged on macOS or
Linux (and vice versa).

#### Running a auto-generated ARD script

The structure of the auto-generated ARD script is discussed in the [ARD
structure](https://clymbclinical.github.io/siera/articles/ARD_script_structure.md)
vignette. Examples of such auto-generated ARD scripts are included in
the package, and can be accessed with the `ARD_script_example` function.
Running one (as we did to produce the preview at the top of this
vignette) looks like this:

``` r

# Step 1: open ARD_xxx.R file
# Step 2: Confirm the location of ADaM dataset(s) is correct in the code section "Load ADaM".
# For the sake of simplicity, the only update made to the ARD_Out14-1-1.R script was to point to the ADaM folder to ARS_example("ADSL.csv")
# Step 3: Run the code and enjoy automated analysis results generation.
# Note: the ARD is contained in the object "ARD" (which contains the appended) mini-ARDs from all individual Analyses ARDs.

example_ARD_script <- ARD_script_example("ARD_Out14-1-1.R")
source(example_ARD_script)
head(ARD)
```

The `ARD` object is the result of appending all analysis-level ARD
(created from the `cards` and `cardx` functions) for the output. It can
be used (and re-used) downstream for various applications. See [this
vignette](https://clymbclinical.github.io/siera/articles/apply-ARD.md)
for next step in utilising the ARD - like creating Tables, or using it
for QC.

### A note on example code used in AnalysisMethodCodeTemplate: using `cards` package

A powerful aspect of the ARS model, is that dynamic code can be supplied
as part of the metadata and be evaluated on either 1. the Output level,
2. the Analysis level, or 3. the AnalysisMethod level (for more
information on this, see
[this](https://www.lexjansen.com/phuse-us/2025/ds/PAP_DS09.pdf) PHUSE
paper). The latter case is implemented by making use of the
*AnalysisMethodCodeTemplate* and *AnalysisMethodCodeParameters* classes
in the metadata. The user supplies dynamic R code (with the option of
making use of parameters referring to metadata pieces), which will be
executed to perform the relevant AnalysisMethod operations. As was the
case in this example, the `cards` and `cardx` R packages were used. See
more information on this in the vignette on [using `cards` and
`cardx`](https://clymbclinical.github.io/siera/articles/using-cards.md).
