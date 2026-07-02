# siera 0.5.6

* Added support for resolving a method's code template from an **external reference** instead of inline ARS metadata. When a method's `codeTemplate` carries a `documentRef` (rather than inline `code`), `readARS()` now follows the ARS `referenceDocuments` chain to load the template - and, where the method declares none inline, its parameters - from either a siera *method manifest* (a `.json` superset of `method.json` carrying `templateCode` + `parameters`, with a method selected by a `pageRefs` named destination = the method `id`) or a bare `.R`/`.txt` code file. Resolution is transparent: a `documentRef` ARS generates the same script as its inline equivalent. The whole method library now also ships as one referenceable catalog, `inst/method-library/method-library.json`, and new bundled examples (`exampleARS_5_documentref.json`/`.xlsx` + `exampleARS_methods.json`) demonstrate the mechanism. v1 resolves local paths only (relative to the ARS file, or absolute); remote URLs are rejected. JSON and XLSX inputs are supported with parity (via the XLSX `ReferenceDocuments` and `AnalysisMethodDocumentRefs` sheets) (#175).
* Added `method_library()`, an exported accessor that lists the bundled analysis-method templates and resolves a method's files on disk (parallel to `ARS_example()`).
* Added a plain-text, testable analysis-method template library under `inst/method-library/` (one `method.json` + `template.R` per method, plus a reconciled `constructs.json` valueSource registry and a generated `METHODS.md` catalog). This becomes the reviewable, diff-able source of truth for the code-template recipes that drive ARD generation; its method `id`s are intended as stable keys an ARS file can reference. A contract test validates every recipe on each run (templates parse, only supported valueSources are referenced, no orphan tokens, no duplicated `case_when` mappings, and the human-readable catalog stays in sync). Introduced an internal `.supported_value_sources()` helper as the declared valueSource contract. The legacy `inst/extdata/R_siera_codes.xlsx` and `cards_constructs.xlsx` are left untouched (#173).
* Added support for per-category risk differences. A new method template computes one risk difference and 95% confidence interval for each data-driven inner category (e.g. one per preferred term or system organ class) by looping over the second grouping and calling `cardx::ard_stats_prop_test()` within each, instead of collapsing the analysis to a single overall risk difference (#157).
* Fixed zero-event risk-difference analyses returning `NA` instead of `0`. Methods that compute over the full population (templates referencing `df_poptot`, e.g. risk differences) now bypass the empty-data subset guard, so an analysis with no events emits the conventional risk difference of `0` with a `[0, 0]` confidence interval (#156).
* Accepted SAS transport (`.xpt`) ADaM datasets in addition to CSV. `readARS()` chooses the reader for each ADaM dataset from its file extension (`.csv` via `readr::read_csv()`, `.xpt` via `haven::read_xpt()`), with no new argument; the file lookup is case-insensitive so lower-case submission file names (e.g. `adsl.xpt`) match upper-case ARS dataset names. Reading `.xpt` requires the `haven` package.
* Fixed `readARS()` crash when ARS metadata contains no `referencedAnalysisOperations` (continuous-only tables such as shift tables) by ensuring accumulator frames are initialised with their merge key columns.
* Normalised Windows backslashes in ADaM file paths to forward slashes so generated scripts run correctly on both Windows and Unix systems.
* Added CDISC ARD-compliant column stamping: generated scripts now write `group[n]_groupingId`, `group[n]_groupId` (for pre-defined groups) and `group[n]_groupValue` (for data-driven groupings) onto each ARD row.
* Supported arbitrarily deep nesting in `mainListOfContents` via a recursive LOPA extraction helper, removing the previous three-level hard limit.
* Expanded support for multi-value ARS group conditions (`IN`/`NOTIN`) by unnesting `condition.value` list columns in the JSON parser.
* Coerced `*_level` columns to character on each individual ARD frame before `bind_rows`, preventing type conflicts between continuous and categorical analyses.
* Allowed more than three grouping factors per analysis.
* Updated bundled `cards_constructs.xlsx` example file and extended test coverage across metadata parsing, ADaM loading, and ARD script generation.

# siera 0.5.5

* Strengthened analysis method validation with clear errors for missing or undefined `MethodId` values and warnings when method templates or parameter value sources cannot be resolved.
* Added guardrails for analysis set generation that warn on missing or incomplete metadata and fall back to the unfiltered analysis dataset when needed.
* Tightened ARS metadata ingestion by requiring `analysisSets` definitions in JSON and the analysis code template sheets in Excel workbooks.
* Expanded test coverage for analysis methods, analysis sets, and data subset condition handling to capture more metadata edge cases.

# siera 0.5.4

* Comprehensive testing added: checking generated ARD result values
* added warning messages with cli()
* cover scenarios for bigN vs table-level subsetting

# siera 0.5.3

* Cover scenario of multi-value DataSubsets
* Cover scenario of overlapping variables in ADSL merge

# siera 0.5.2

* Added conditional operators to cover EQ, NE, GE, GT, LE, LT

# siera 0.5.0

* readARS function to ingest xlsx, json and CDISC example

# siera 0.4.0

* addition of readARS_xl function
* example added of {cards} in AnalysisMethodCodeTemplate

# siera 0.3.0

* dynamic recognition of ADaMs

# siera 0.1.0

* Initial CRAN submission.
