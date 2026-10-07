# Read ARS metadata from JSON

Internal helper that ingests ARS metadata stored as JSON and converts it
into the harmonised list of tibbles used elsewhere in the package.

## Usage

``` r
.read_ars_json_metadata(
  ARS_path,
  ars_dir = dirname(ARS_path),
  json_from = jsonlite::fromJSON(ARS_path)
)
```

## Arguments

- ARS_path:

  Path to the JSON ARS metadata file.

- ars_dir:

  Directory that relative \`referenceDocuments\` locations resolve
  against. Defaults to the directory of \`ARS_path\`.

- json_from:

  The parsed ARS JSON. Defaults to parsing \`ARS_path\`; the deprecated
  xlsx path passes an already-converted object instead.

## Value

A list of metadata tables extracted from the JSON file, or \`NULL\` when
required sections are missing.
