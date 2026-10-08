# Convert an ARS Excel workbook to ARS JSON

\*\*Deprecated.\*\* siera is moving to JSON-only ARS metadata, and both
\`.xlsx\` input to \[readARS()\] and this converter will be removed in a
future release. Use it now to convert each workbook once, then keep the
\`.json\` file as the source of truth for your ARS metadata. Every call
warns with class \`siera_deprecated_xlsx\`.

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

The converter maps the full ARS object model: \`about\`, \`studyInfo\`,
the main and other lists of contents, reference documents, terminology
extensions, analysis output categorizations, analysis sets, analysis
groupings, data subsets, methods (with operations and code templates),
analyses, global display sections and outputs (with displays).

Multi-value cells (for example the values of an \`IN\` condition, or
\`categoryIds\`) are split on \`\|\`, with or without surrounding
spaces, and each value is trimmed. Commas are never treated as
separators. Multi-value ARS fields are always written as JSON arrays,
even when they hold one value.

Note that the ARS spec evolves; this converter targets the worksheet
layout emitted by TFL Designer and the CDISC ARS template and may need
updating for future ARS versions.

## Examples

``` r
json <- suppressWarnings(
  ars_xlsx_to_json(ARS_example("exampleARS_6.xlsx"),
                   json_path = tempfile(fileext = ".json"))
)
#> Wrote ARS JSON: /tmp/RtmpJMQy8a/file18907b6a2582.json
readARS(json, output_path = tempdir(), adam_path = tempdir())
```
