# Recursively extract analysis IDs from a list-of-contents node data frame

Recursively extract analysis IDs from a list-of-contents node data frame

## Usage

``` r
.extract_lopa_ids(node_df, output_id)
```

## Arguments

- node_df:

  A data.frame produced by jsonlite for one level of
  \`sublist\$listItems\`. May contain columns \`analysisId\` (character)
  and/or \`sublist\` (list column). Either column may be absent or
  entirely NA/NULL.

- output_id:

  Scalar character; the \`outputId\` to attach to every row.

## Value

A tibble with columns \`listItem_analysisId\` and \`listItem_outputId\`.
