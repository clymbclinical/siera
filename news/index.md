# Changelog

## siera 0.5.6

CRAN release: 2026-06-17

- Added support for per-category risk differences. A new method template
  computes one risk difference and 95% confidence interval for each
  data-driven inner category (e.g. one per preferred term or system
  organ class) by looping over the second grouping and calling
  [`cardx::ard_stats_prop_test()`](https://insightsengineering.github.io/cardx/latest-tag/reference/ard_stats_prop_test.html)
  within each, instead of collapsing the analysis to a single overall
  risk difference
  ([\#157](https://github.com/clymbclinical/siera/issues/157)).
- Fixed zero-event risk-difference analyses returning `NA` instead of
  `0`. Methods that compute over the full population (templates
  referencing `df_poptot`, e.g. risk differences) now bypass the
  empty-data subset guard, so an analysis with no events emits the
  conventional risk difference of `0` with a `[0, 0]` confidence
  interval ([\#156](https://github.com/clymbclinical/siera/issues/156)).
- Accepted SAS transport (`.xpt`) ADaM datasets in addition to CSV.
  [`readARS()`](https://clymbclinical.github.io/siera/reference/readARS.md)
  chooses the reader for each ADaM dataset from its file extension
  (`.csv` via
  [`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html),
  `.xpt` via
  [`haven::read_xpt()`](https://haven.tidyverse.org/reference/read_xpt.html)),
  with no new argument; the file lookup is case-insensitive so
  lower-case submission file names (e.g. `adsl.xpt`) match upper-case
  ARS dataset names. Reading `.xpt` requires the `haven` package.
- Fixed
  [`readARS()`](https://clymbclinical.github.io/siera/reference/readARS.md)
  crash when ARS metadata contains no `referencedAnalysisOperations`
  (continuous-only tables such as shift tables) by ensuring accumulator
  frames are initialised with their merge key columns.
- Normalised Windows backslashes in ADaM file paths to forward slashes
  so generated scripts run correctly on both Windows and Unix systems.
- Added CDISC ARD-compliant column stamping: generated scripts now write
  `group[n]_groupingId`, `group[n]_groupId` (for pre-defined groups) and
  `group[n]_groupValue` (for data-driven groupings) onto each ARD row.
- Supported arbitrarily deep nesting in `mainListOfContents` via a
  recursive LOPA extraction helper, removing the previous three-level
  hard limit.
- Expanded support for multi-value ARS group conditions (`IN`/`NOTIN`)
  by unnesting `condition.value` list columns in the JSON parser.
- Coerced `*_level` columns to character on each individual ARD frame
  before `bind_rows`, preventing type conflicts between continuous and
  categorical analyses.
- Allowed more than three grouping factors per analysis.
- Updated bundled `cards_constructs.xlsx` example file and extended test
  coverage across metadata parsing, ADaM loading, and ARD script
  generation.

## siera 0.5.5

CRAN release: 2025-12-01

- Strengthened analysis method validation with clear errors for missing
  or undefined `MethodId` values and warnings when method templates or
  parameter value sources cannot be resolved.
- Added guardrails for analysis set generation that warn on missing or
  incomplete metadata and fall back to the unfiltered analysis dataset
  when needed.
- Tightened ARS metadata ingestion by requiring `analysisSets`
  definitions in JSON and the analysis code template sheets in Excel
  workbooks.
- Expanded test coverage for analysis methods, analysis sets, and data
  subset condition handling to capture more metadata edge cases.

## siera 0.5.4

CRAN release: 2025-09-25

- Comprehensive testing added: checking generated ARD result values
- added warning messages with cli()
- cover scenarios for bigN vs table-level subsetting

## siera 0.5.3

CRAN release: 2025-08-28

- Cover scenario of multi-value DataSubsets
- Cover scenario of overlapping variables in ADSL merge

## siera 0.5.2

CRAN release: 2025-08-25

- Added conditional operators to cover EQ, NE, GE, GT, LE, LT

## siera 0.5.0

CRAN release: 2025-07-29

- readARS function to ingest xlsx, json and CDISC example

## siera 0.4.0

CRAN release: 2025-07-18

- addition of readARS_xl function
- example added of {cards} in AnalysisMethodCodeTemplate

## siera 0.3.0

CRAN release: 2025-03-03

- dynamic recognition of ADaMs

## siera 0.1.0

CRAN release: 2025-02-10

- Initial CRAN submission.
