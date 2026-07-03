# Convert an ARS Excel workbook to ARS JSON

Converts a CDISC Analysis Results Standard (ARS) metadata workbook
(\`.xlsx\`) into an ARS ReportingEvent JSON file. This is an R-native
reimplementation of CDISC's Python \`excel2ars.py\` utility and performs
a faithful whole-workbook conversion: every ARS worksheet is mapped, not
only the sheets that \[readARS()\] itself consumes.

## Usage

``` r
ars_xlsx_to_json(xlsx_path, json_path = NULL)
```

## Arguments

- xlsx_path:

  Path to the ARS Excel workbook (\`.xlsx\`).

- json_path:

  Optional path for the output JSON file. When \`NULL\` (the default),
  the JSON is written beside \`xlsx_path\` with the same base name and a
  \`.json\` extension.

## Value

The path to the written JSON file, invisibly.

## Details

\`readARS()\` ingests ARS JSON. Use \`ars_xlsx_to_json()\` first to
convert an Excel workbook, then pass the resulting \`.json\` file to
\`readARS()\`.

The converter maps the full ARS object model: \`about\`, \`studyInfo\`,
the main and other lists of contents, reference documents, terminology
extensions, analysis output categorizations, analysis sets, analysis
groupings, data subsets, methods (with operations and code templates),
analyses, global display sections and outputs (with displays).

Note that the ARS spec evolves; this converter targets the worksheet
layout emitted by TFL Designer and the CDISC ARS template and may need
updating for future ARS versions.

## Examples

``` r
if (FALSE) { # \dontrun{
# Convert a workbook and generate ARD scripts from the result.
json <- ars_xlsx_to_json("study.xlsx")
readARS(json, output_path = tempdir(), adam_path = "adam")
} # }
```
