# Read ARS metadata from disk

Internal helper that validates the ARS file extension and dispatches to
the JSON reader, the single ARS parser in siera. A deprecated \`.xlsx\`
workbook is converted to ARS JSON in memory first (see
\`.read_ars_xlsx_via_json()\`).

## Usage

``` r
.read_ars_metadata(ARS_path)
```

## Arguments

- ARS_path:

  Path to the ARS metadata file (\`.json\`, or a deprecated \`.xlsx\`
  workbook).

## Value

A list containing harmonised metadata tables, or \`NULL\` if the file is
missing required sections.
