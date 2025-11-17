# Build a data subset condition

Internal helper that translates ARS data-subset metadata into a filter
expression suitable for inclusion in generated R code. Handles
comparator translation, type coercion, and workbook-specific formatting
differences.

## Usage

``` r
.generate_data_subset_condition(variable, comparator, value, file_ext)
```

## Arguments

- variable:

  Variable name used in the subset definition.

- comparator:

  Comparison operator from the metadata.

- value:

  Value(s) associated with the comparator.

- file_ext:

  Extension of the source ARS file, used to normalise parsing.

## Value

Character string representing the filter expression to apply.
