# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Package overview

**siera** is an R package that ingests CDISC Analysis Results Standard
(ARS) metadata (JSON or Excel) and auto-generates R scripts that produce
Analysis Results Datasets (ARDs) from ADaM datasets. The only exported
user-facing function is
[`readARS()`](https://clymbclinical.github.io/siera/reference/readARS.md).

## CRAN readiness

The package must always be kept in a CRAN-submittable state. Every
change merged to `main` should pass `R CMD check` with 0 errors and 0
warnings. Avoid adding dependencies, exported symbols, or behaviours
that would violate CRAN policies. Long-running or environment-specific
tests must be guarded with `skip_on_cran()`.

## Development commands

All commands use R 4.4.1 at
`C:\Program Files\R\R-4.4.1\bin\Rscript.exe`.

``` r

devtools::document()          # Regenerate man/ from Roxygen
devtools::load_all()          # Load package for interactive dev
devtools::check(vignettes = FALSE)  # Full R CMD check (Pandoc unavailable locally)
devtools::test()              # Run all tests
```

Run a single test file or filter by name:

``` r

devtools::test_file("tests/testthat/test-readARS.R")
devtools::test_file("tests/testthat/test-readARS.R", filter = "4 grouping factors")
```

**Before every PR**: run `document()`, `load_all()`, and
`check(vignettes = FALSE)` — all must pass with 0 errors, 0 warnings.
The “unable to verify current time” NOTE is a known environment artefact
and can be ignored.

## Architecture and data flow

`readARS(ARS_path, output_path, adam_path)` is the single entry point:

1.  **Parse metadata** (`metadata.R`) — dispatches to
    [`.read_ars_json_metadata()`](https://clymbclinical.github.io/siera/reference/dot-read_ars_json_metadata.md)
    or
    [`.read_ars_xlsx_metadata()`](https://clymbclinical.github.io/siera/reference/dot-read_ars_xlsx_metadata.md)
    based on file extension. Both return the same normalised list of
    tibbles:
    - `Lopo` — list of planned outputs
    - `Lopa` — list of planned analyses (maps analyses → outputs)
    - `DataSubsets`, `AnalysisSets`, `AnalysisGroupings`
    - `Analyses`, `AnalysisMethods`, `AnalysisMethodCodeTemplate`,
      `AnalysisMethodCodeParameters`
2.  **Generate one R script per output** (`readARS.R`) — loops over
    `Lopo`, collects the analyses for that output from `Lopa`, then
    assembles code fragments by calling:
    - [`.generate_library_code()`](https://clymbclinical.github.io/siera/reference/dot-generate_library_code.md)
      — static [`library()`](https://rdrr.io/r/base/library.html) calls
    - [`.generate_program_header()`](https://clymbclinical.github.io/siera/reference/dot-generate_program_header.md)
      — script banner
    - [`.generate_adam_loading_code()`](https://clymbclinical.github.io/siera/reference/dot-generate_adam_loading_code.md)
      —
      [`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html)
      calls for required ADaM datasets
    - [`.generate_analysis_set_code()`](https://clymbclinical.github.io/siera/reference/dot-generate_analysis_set_code.md)
      — population filter (`SAFFL` etc.), with USUBJID merge when
      analysis dataset ≠ analysis-set dataset
    - [`.generate_data_subset_code()`](https://clymbclinical.github.io/siera/reference/dot-generate_data_subset_code.md)
      — WHERE clause filters; supports ARS comparators (EQ, NE, IN,
      NOTIN, CONTAINS, …) and multi-level AND/OR nesting
    - [`.generate_analysis_method_section()`](https://clymbclinical.github.io/siera/reference/dot-generate_analysis_method_section.md)
      — substitutes parameters into method code templates; uses
      [`assign()`](https://rdrr.io/r/base/assign.html)/[`get()`](https://rdrr.io/r/base/get.html)
      in a caller `envir` to pass operation IDs back
3.  **Write script** — fragments are concatenated and written to
    `ARD_<OutputId>.R` in `output_path`.

## Key implementation details

- **JSON/XLSX parity is required**: every code-generation change must
  work identically regardless of whether the ARS input file is `.json`
  or `.xlsx`. Both parsers produce the same normalised tibble names and
  column names; test both paths when adding new features that read from
  those tibbles.
- **Code generation is string-based**: method templates contain literal
  placeholder strings (e.g. `"analysisidhere"`, `"methodidhere"`)
  replaced with [`gsub()`](https://rdrr.io/r/base/grep.html). Don’t
  confuse these with R variables.
- **jsonlite produces nested data frames** (not list columns) when
  `simplifyDataFrame = TRUE`. The recursive helper `.extract_lopa_ids()`
  in `metadata.R` relies on this structure to traverse arbitrary-depth
  `sublist` nesting in `mainListOfContents`.
- **`analysisId` must be coerced to `character`** when binding rows from
  JSON because JSON `null` parses as logical `NA`, causing type
  conflicts in
  [`dplyr::bind_rows`](https://dplyr.tidyverse.org/reference/bind_rows.html).
- **Dynamic grouping** (`AG_vars`, `AG_var1/2/3…`) supports 1–N grouping
  factors; the code generation branches on how many `resultsBy`
  groupings are present in `AnalysisGroupings`.
- **`n_group_cols` determination** — the number of `group[n]` columns a
  method produces differs from `num_grp` when the last grouping goes to
  `variables=` instead of `by=`. Always inspect the *resolved*
  `code_method` string for the presence of `by_vars`/`strata_vars` or
  `by_listc` substrings. Do NOT check the parameter table — it can
  contain spurious unused parameters that give false positives.
- **`group_condition_value` in JSON is a list-column** — jsonlite always
  returns `condition.value` as a list, even for single-element EQ
  arrays. The JSON parser must call
  `tidyr::unnest(tmp_AG, cols = "group_condition_value")` after building
  `tmp_AG` to expand IN/NOTIN multi-value conditions (one row per value,
  `group_id` repeated) and collapse single-value list elements to plain
  character. Use a quoted string in `cols=` to avoid R CMD check “no
  visible binding” NOTE.
- **`_level` columns and `bind_rows` type conflicts** — cards returns
  `variable_level` as a list-of-NULLs for continuous analyses and as
  character for categorical ones. When combined with `bind_rows`, this
  creates list-type columns containing “NULL” strings. Fix: generate a
  per-df
  `mutate(across(matches("_level$"), vapply(..., function(v) if(is.null(v)) NA_character_ else as.character(v), character(1L))))`
  block before the final `bind_rows`. Do NOT use
  `across(where(is.list), as.character)` — that converts the numeric
  `stat` list column to character, breaking downstream
  [`round()`](https://rdrr.io/r/base/Round.html) calls.
- **CDISC ARD compliance** — each ARD row must carry: `AnalysisId`,
  `operationid`, and per-resultGroup: `group[n]_groupingId` + either
  `group[n]_groupId` (pre-defined groups, via `case_when` on
  `group[n]_level`) or `group[n]_groupValue` (data-driven groupings).
  See `.generate_groupid_code()` in `readARS.R`.
- **Internal functions** are prefixed with `.`
  (e.g. `.read_ars_metadata`). Only four symbols are exported:
  `readARS`, `ARS_example`, `ARD_script_example`, `%>%`.

## cards / cardx API alignment

The generated R scripts and example scripts in `inst/script/` must
always use the current `cards` API. Key function renames from cards ≥
0.7.0:

- `ard_categorical()` → `ard_tabulate()`
- `ard_continuous()` → `ard_summary()`
- `ard_complex()` → `ard_mvsummary()`

When updating example scripts or code templates, check the cards
changelog for any further deprecations. The `strata` argument exists in
`ard_tabulate()` (unlike the old `ard_categorical()`) for computing
percentages over observed combinations only — see cards docs for the
`by` vs `strata` distinction.

**`ard_tabulate()` denominator pattern:** For categorical n(%) analyses
with a pre-computed denominator, the correct structure is
`by = 'group_var', variables = 'analysis_var'` with a pre-counted
denominator data frame that has the count column renamed to
`...ard_N...`. The old pattern of
`by = c('group_var', 'analysis_var'), variables = 'dummy_var'` with a
raw data frame as denominator is no longer correct. Example:

``` r

denom <- df_pop |> dplyr::count(TRT01A) |> dplyr::rename(`...ard_N...` = n)
cards::ard_tabulate(data = in_data, by = 'TRT01A', variables = 'AGEGR1', denominator = denom)
```

Do not modify the test fixture xlsx files in `inst/extdata/` — the
package owner updates those separately. The example R scripts in
`inst/script/` are pre-generated and committed; regenerate them manually
using
[`readARS()`](https://clymbclinical.github.io/siera/reference/readARS.md)
after the xlsx templates are updated, then adjust the ADaM-loading lines
to use
[`siera::ARS_example()`](https://clymbclinical.github.io/siera/reference/ARS_example.md).

## Test conventions

- **Every code change must be accompanied by a test.** If you add a
  feature, add a test that would fail without it. If you fix a bug, add
  a test that reproduces the bug. Do not submit a PR without new or
  updated tests covering the change.
- Integration tests source the generated `.R` script into an isolated
  `new.env(parent = baseenv())` and inspect the resulting `ARD` object.
- Long-running tests (those that source generated scripts against real
  ADaM data) are guarded with `skip_on_cran()`.
- Temp directories use
  [`withr::local_tempdir()`](https://withr.r-lib.org/reference/with_tempfile.html)
  for automatic cleanup.
- Example ARS files live in `inst/extdata/` (`exampleARS_1` –
  `exampleARS_6`, both `.json` and `.xlsx`). Retrieve them in tests via
  `ARS_example("exampleARS_6.json")`.

## Notes:

- always delete codes or datasets used for temporary testing once the
  temp testing is completed (keep the repo clean)
- after a reasonable development effort and milestone, capture learnings
  and add to claude.md any information that might be useful in the
  future regarding the understanding and workings of the project.  
- notify the user when there are strong candidate processes for creating
  a skill. Whenever there are repeated prompts or processes that would
  fit a use case. Suggest the skill and its benefits.
- keep in mind that the target audience are other industry members.
  Whenever logic or usage or something important for a user to know is
  affected, highlight this and suggest education material or a vignette
  idea.
