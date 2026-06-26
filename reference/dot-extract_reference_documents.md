# Normalise the reporting event's \`referenceDocuments\` into a tibble

Accepts whatever \`jsonlite::fromJSON()\` produced for
\`referenceDocuments\` (a data frame under \`simplifyDataFrame\`, a list
of records, or \`NULL\`) and returns a tibble with \`id\`, \`name\`,
\`location\`.

## Usage

``` r
.extract_reference_documents(reference_documents)
```

## Arguments

- reference_documents:

  The parsed \`referenceDocuments\` element, or \`NULL\`.

## Value

A tibble with columns \`id\`, \`name\`, \`location\`.
