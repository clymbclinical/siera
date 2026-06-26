# Extract a method's \`documentRef.pageRefs\` named-destination page names

Extract a method's \`documentRef.pageRefs\` named-destination page names

## Usage

``` r
.documentref_page_names(document_ref, i)
```

## Arguments

- document_ref:

  The \`codeTemplate\$documentRef\` data frame from jsonlite.

- i:

  Method row index.

## Value

Character vector of page names (method ids); \`character(0)\` if none.
