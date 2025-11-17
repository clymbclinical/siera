# Read ARS metadata from XLSX

Internal helper that ingests ARS metadata stored in Excel workbooks and
converts each worksheet into the harmonised metadata list.

## Usage

``` r
.read_ars_xlsx_metadata(ARS_path)
```

## Arguments

- ARS_path:

  Path to the Excel workbook containing ARS metadata.

## Value

A list of metadata tables extracted from the workbook, or \`NULL\` when
required sheets are missing.
