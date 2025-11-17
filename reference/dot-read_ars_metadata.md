# Read ARS metadata from disk

Internal helper that validates the ARS file extension and dispatches to
the JSON or XLSX reader before returning the harmonised metadata list.

## Usage

``` r
.read_ars_metadata(ARS_path)
```

## Arguments

- ARS_path:

  Path to the ARS metadata file (JSON or XLSX).

## Value

A list containing harmonised metadata tables, or \`NULL\` if the file
cannot be parsed.
