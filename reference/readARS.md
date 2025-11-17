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
  spec_output = ""
)
```

## Arguments

- ARS_path:

  A file containing ARS metadata for a reporting event

- output_path:

  Path to store .R ARD scripts

- adam_path:

  Path to folder containing ADaM datasets, to be run in ARD program

- spec_output:

  The output ID for a specific output to be run from the metadata

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
