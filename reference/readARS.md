# Ingest ARS (Analysis Results Standard) metadata, produce ARD (Analysis Results Dataset) code for each output

Ingest ARS (Analysis Results Standard) metadata, and meta-programme R
scripts that could be run as-is to produce Analysis Results Datasets
when ingesting ADaM datasets

## Usage

``` r
readARS(
  ARS_path,
  output_path = tempdir(),
  adam_path = tempdir(),
  spec_output = "",
  output_format = "none"
)
```

## Arguments

- ARS_path:

  A file containing ARS metadata for a reporting event

- output_path:

  Path to store .R ARD scripts

- adam_path:

  Path to folder containing ADaM datasets, to be run in ARD program.
  Datasets may be supplied as CSV (\`.csv\`) or SAS transport (\`.xpt\`)
  files. siera chooses the reader for each dataset from the file
  extension found in this folder — \`.csv\` files are read with
  [`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html)
  and \`.xpt\` files with
  [`haven::read_xpt()`](https://haven.tidyverse.org/reference/read_xpt.html)
  — so no extra argument is required (mirroring how the ARS input format
  is inferred from \`.json\` vs \`.xlsx\`). Reading \`.xpt\` datasets in
  the generated script requires the haven package. When both a \`.csv\`
  and a \`.xpt\` exist for the same dataset, the \`.csv\` is used.

- spec_output:

  The output ID for a specific output to be run from the metadata

- output_format:

  Format for emitting the generated ARD. Must be exactly one of
  \`"none"\` or \`"datasetjson"\` (no partial matching). \`"none"\`
  (default) generates the ARD scripts only. \`"datasetjson"\`
  additionally appends code that writes each ARD as a CDISC Dataset-JSON
  file (\`ARD\_\<OutputId\>.json\`) when the generated script is run.
  The Dataset-JSON export requires the optional datasetjson package to
  be installed in the environment that runs the generated script.

## Value

R programmes generating ARDs - one for each output (or analysis from an
output) specified in the ARS metadata

## Examples

``` r
# path to file containing ARS metadata

ARS_path <- ARS_example("Common_Safety_Displays_cards.xlsx")

# output path for R programs
output_dir <- tempdir()

# folder containing ADaM datasets
adam_folder <- tempdir()

# run function, write to temp directory
readARS(ARS_path, output_dir, adam_folder)
#> New names:
#> • `` -> `...11`
#> • `` -> `...12`
#> • `` -> `...13`
```
