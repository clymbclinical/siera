# Select and validate a method from a siera method manifest

Select and validate a method from a siera method manifest

## Usage

``` r
.resolve_method_from_manifest(path, page_names, reference_document_id)
```

## Arguments

- path:

  Path to the manifest JSON.

- page_names:

  Named-destination page names (method ids); may be NULL/empty.

- reference_document_id:

  Id used only for error messages.

## Value

A list with \`templateCode\` and \`parameters\` (tibble or NULL).
