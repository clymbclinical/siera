# siera method-template library

This directory is the **plain-text source of truth** for siera's analysis-method
code templates — the `templateCode` + parameters + valueSource recipes that an ARS
author drops into a method's `codeTemplate` so siera can generate ARD code for a
given statistic.

siera does **not** read this directory at runtime (it reads ARS metadata). The
library exists so the recipes are reviewable, diff-able, and **testable**, and so the
owner-facing Excel workbook can be *generated* from text rather than hand-edited
(which is how the legacy `R_siera_codes.xlsx` silently drifted out of sync).

## Layout

```
inst/method-library/
  <NN_key>/
    method.json          # metadata: id, name, label, description, status, operations[], parameters[]
    template.R           # the codeTemplate.code, with placeholder tokens
  constructs.json        # the reconciled valueSource vocabulary (supported keys only)
  METHODS.md             # GENERATED human-readable catalog (do not hand-edit)
  method-library.json    # GENERATED referenceable JSON manifest catalog (do not hand-edit)
  README.md              # this file
```

The `id` in each `method.json` is the **stable key**: an ARS file can
*reference* a library method by `id` rather than embedding the `templateCode`
inline (see "Referencing a method from ARS" below).

## Referencing a method from ARS (issue #175)

siera consumes the ARS `codeTemplate.documentRef` mechanism, so an ARS file can
point a method's `codeTemplate` at an external document instead of carrying the
template (and parameters) inline. The generated script is identical either way.

The whole library ships as one referenceable **method manifest**,
`method-library.json` (a superset of each `method.json` that also carries the
`templateCode`). It is generated from the per-method text files, so it can never
drift. Regenerate after any change with:

```r
writeLines(siera:::.render_method_library_json(), "inst/method-library/method-library.json")
```

An ARS author wires a method to a library entry like this (JSON):

```json
// ReportingEvent.referenceDocuments[]
{ "id": "RefDoc_siera_methods", "name": "siera method library",
  "location": "method-library.json" }

// methods[].codeTemplate (no inline "code")
{ "context": "R (siera)",
  "documentRef": {
    "referenceDocumentId": "RefDoc_siera_methods",
    "pageRefs": [ { "refType": "NamedDestination", "pageNames": ["total_n"] } ]
  } }
```

`location` resolves **locally only** (relative to the ARS file, or absolute);
remote URLs are rejected. A single-method manifest or a bare `.R`/`.txt` code
file (parameters then stay inline in the ARS) are also accepted. The XLSX ARS
representation carries the same wiring in its `ReferenceDocuments` and
`AnalysisMethodDocumentRefs` sheets. See the bundled
`exampleARS_5_documentref.json` / `.xlsx` (with `exampleARS_methods.json`) for a
runnable end-to-end example.

Access from R with the exported accessor: `method_library()` lists the available
ids, and `method_library("<id>")` returns the path to that method's directory.

### `method.json`

| Field | Meaning |
|---|---|
| `id` / `name` / `label` / `description` | Catalog identity. (The actual ARS file assigns its own method id; `id` here is a catalog key.) |
| `status` | `verified` (matched against a reference/working method) or `corrected-unvalidated` (structurally fixed, no reference to assert against). |
| `verified_against` | The working method / reference the recipe was checked against. |
| `operations[]` | `order`, `name` (the cards `stat_name` it maps from), `label`, `resultPattern`. `operation_1` = `order` 1, etc. |
| `parameters[]` | `name` (placeholder token), `valueSource` (must be in `constructs.json` / `.supported_value_sources()`), `label`, `description`. |

### `template.R`

The cards/cardx recipe using placeholder tokens. Tokens come in two kinds:

- **Parameters** (declared in `method.json`): `anavarhere`, `groupvar1here`,
  `byvarshere`, `distinctlisthere`, … — resolved per analysis from ARS metadata.
- **Internal** (substituted automatically by siera, never declared): any token
  containing `analysisidhere` (e.g. `df2_analysisidhere`, `df3_analysisidhere`),
  `methodidhere`, `outputidhere`, and `opidNhere` (the operation IDs).

Each template must assign `df3_analysisidhere <- …`. Some valueSources (`by_vars`,
`strata_vars`, `by_stmt`) inject a **leading comma**, so a raw template is not valid
R until substituted — the contract test parses a substituted copy, not the raw text.

## Contract test

`tests/testthat/test-method-library.R` validates every method on each run:

1. each `template.R` parses once placeholders are substituted with dummy values;
2. every `parameters[].valueSource` is supported (`.supported_value_sources()` or the
   `operation_N` pattern);
3. no orphan parameters (every declared token appears in the template) and no unknown
   `…here` placeholder tokens;
4. no duplicated `stat_name == '…'` left-hand side in a `case_when` (the class of bug
   that mapped `conf.low` twice and dropped `conf.high` in the legacy sheets);
5. operation IDs referenced (`opidN`) do not exceed the declared `operations`;
6. the committed `METHODS.md` equals the freshly rendered catalog (no drift);
7. the committed `method-library.json` manifest equals the freshly rendered
   catalog, and every method resolves out of it via the documentRef resolver.

## Human-readable catalog

`METHODS.md` is a generated Markdown view of this library (summary table,
valueSource reference, and a per-method section with operations, parameters, and
the template). It is produced from the text files by the internal renderer and
committed so it is diff-able in PRs. Regenerate after any change with:

```r
writeLines(siera:::.render_method_library_md(), "inst/method-library/METHODS.md")
```

The freshness contract test fails if you forget, so the human view can never
silently drift from the source of truth. The machine-readable
`method-library.json` manifest (the ARS-referenceable catalog) is generated the
same way and held to the same freshness test. There is intentionally **no**
generated xlsx - the legacy `inst/extdata/R_siera_codes.xlsx` is left untouched
but is not part of this library.

## Adding a method

See the `siera-author-method` skill. In short: create a new `<NN_key>/` folder with
`template.R` + `method.json`, reference only supported valueSources, run the contract
test, and (where a reference exists) assert the numbers in `test-etfl-regression.R`.
