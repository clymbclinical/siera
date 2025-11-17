# Generate data subset code

Internal helper that assembles the code required to apply a data subset
for a given analysis. The function prepares default code, augments it
with conditional filters when required, and returns metadata describing
the subset that was applied.

## Usage

``` r
.generate_data_subset_code(
  data_subsets,
  subset_id,
  analysis_id,
  analysis_set_dataset,
  file_ext
)
```

## Arguments

- data_subsets:

  DataSubsets metadata for the reporting event.

- subset_id:

  Identifier of the subset tied to the current analysis.

- analysis_id:

  Identifier of the analysis for which code is generated.

- analysis_set_dataset:

  Dataset name produced by the analysis set step.

- file_ext:

  Extension of the source ARS metadata file (json or xlsx).

## Value

A list containing the generated code, subset name, and filter
expression.
