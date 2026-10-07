# Build a data subset condition

Internal helper that translates ARS data-subset metadata into a filter
expression suitable for inclusion in generated R code. Handles
comparator translation and type coercion.

## Usage

``` r
.generate_data_subset_condition(variable, comparator, value)
```

## Arguments

- variable:

  Variable name used in the subset definition.

- comparator:

  Comparison operator from the metadata.

- value:

  Value(s) associated with the comparator; multi-value \`IN\` /
  \`NOTIN\` conditions arrive as one element per value.

## Value

Character string representing the filter expression to apply.
