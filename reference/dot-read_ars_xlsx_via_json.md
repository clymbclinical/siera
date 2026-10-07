# Read a deprecated ARS xlsx workbook through the JSON reader

Converts the workbook to ARS JSON in memory with the same builder that
backs \[ars_xlsx_to_json()\], then parses it with
\`.read_ars_json_metadata()\`, so an xlsx workbook and its converted
JSON yield identical metadata. The round trip through JSON text gives
the parsed object exactly the shape \`jsonlite::fromJSON()\` produces
for an ARS JSON file.

## Usage

``` r
.read_ars_xlsx_via_json(ARS_path)
```

## Arguments

- ARS_path:

  Path to the ARS Excel workbook.

## Value

The harmonised metadata list, or \`NULL\` (with a warning) when the
workbook lacks a sheet siera needs.
