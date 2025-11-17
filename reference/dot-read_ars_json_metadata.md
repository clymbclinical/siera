# Read ARS metadata from JSON

Internal helper that ingests ARS metadata stored as JSON and converts it
into the harmonised list of tibbles used elsewhere in the package.

## Usage

``` r
.read_ars_json_metadata(ARS_path)
```

## Arguments

- ARS_path:

  Path to the JSON ARS metadata file.

## Value

A list of metadata tables extracted from the JSON file, or \`NULL\` when
required sections are missing.
