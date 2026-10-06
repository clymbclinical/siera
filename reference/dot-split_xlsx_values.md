# Split an xlsx multi-value condition cell into its values

The ARS xlsx representation keeps a multi-value \`IN\` / \`NOTIN\`
condition in one cell, separated by \`" \| "\` (the TFL Designer /
\`excel2ars.py\` convention; some exports use commas). Each value is
trimmed so the blanks around the separator never become part of a value:
\`"A \| B"\` must give \`c("A", "B")\`, not \`c("A ", "B")\`, which
would silently match nothing (#213).

## Usage

``` r
.split_xlsx_values(x)
```

## Arguments

- x:

  A scalar condition-value cell.

## Value

Character vector of the individual values.
