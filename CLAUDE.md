# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package overview

**siera** is an R package that ingests CDISC Analysis Results Standard (ARS) metadata (JSON or Excel) and auto-generates R scripts that produce Analysis Results Datasets (ARDs) from ADaM datasets. The only exported user-facing function is `readARS()`.

## CRAN readiness

The package must always be kept in a CRAN-submittable state. Every change merged to `main` should pass `R CMD check` with 0 errors and 0 warnings. Avoid adding dependencies, exported symbols, or behaviours that would violate CRAN policies. Long-running or environment-specific tests must be guarded with `skip_on_cran()`.

## Development commands

All commands use R 4.4.1 at `C:\Program Files\R\R-4.4.1\bin\Rscript.exe`.

```r
devtools::document()          # Regenerate man/ from Roxygen
devtools::load_all()          # Load package for interactive dev
devtools::check(vignettes = FALSE)  # Full R CMD check (Pandoc unavailable locally)
devtools::test()              # Run all tests
```

Run a single test file or filter by name:
```r
devtools::test_file("tests/testthat/test-readARS.R")
devtools::test_file("tests/testthat/test-readARS.R", filter = "4 grouping factors")
```

**Before every PR**: run `document()`, `load_all()`, and `check(vignettes = FALSE)` — all must pass with 0 errors, 0 warnings. The "unable to verify current time" NOTE is a known environment artefact and can be ignored.

## Key files

| File | Purpose |
|---|---|
| `R/readARS.R` | Main entry point — `readARS()` orchestrates the full pipeline |
| `R/metadata.R` | Parses ARS JSON/XLSX into flat R data frames |
| `R/AnalysisSet.R` | Generates analysis set (population) filter code |
| `R/DataSubsets.R` | Generates data subset filter code |
| `R/AnalysisMethods.R` | Resolves method code templates and valueSource parameters |
| `R/loadADaM.R` | Generates ADaM dataset loading code |
| `R/libraries.R` | Generates `library()` calls for generated scripts |
| `R/program_header.R` | Generates programme header comments |
| `inst/extdata/` | Example ARS files (JSON and XLSX) used in tests and examples |
| `inst/script/` | Example generated ARD scripts |

## Architecture and data flow

`readARS(ARS_path, output_path, adam_path)` is the single entry point:

1. **Parse metadata** (`metadata.R`) — dispatches to `.read_ars_json_metadata()` or `.read_ars_xlsx_metadata()` based on file extension. Both return the same normalised list of tibbles:
   - `Lopo` — list of planned outputs
   - `Lopa` — list of planned analyses (maps analyses → outputs)
   - `DataSubsets`, `AnalysisSets`, `AnalysisGroupings`
   - `Analyses`, `AnalysisMethods`, `AnalysisMethodCodeTemplate`, `AnalysisMethodCodeParameters`

2. **Generate one R script per output** (`readARS.R`) — loops over `Lopo`, collects the analyses for that output from `Lopa`, then assembles code fragments by calling:
   - `.generate_library_code()` — static `library()` calls
   - `.generate_program_header()` — script banner
   - `.generate_adam_loading_code()` — `readr::read_csv()` calls for required ADaM datasets
   - `.generate_analysis_set_code()` — population filter (`SAFFL` etc.), with USUBJID merge when analysis dataset ≠ analysis-set dataset
   - `.generate_data_subset_code()` — WHERE clause filters; supports ARS comparators (EQ, NE, IN, NOTIN, CONTAINS, …) and multi-level AND/OR nesting
   - `.generate_analysis_method_section()` — substitutes parameters into method code templates; uses `assign()`/`get()` in a caller `envir` to pass operation IDs back

3. **Write script** — fragments are concatenated and written to `ARD_<OutputId>.R` in `output_path`.

## Key implementation details

- **JSON/XLSX parity is required**: every code-generation change must work identically regardless of whether the ARS input file is `.json` or `.xlsx`. Both parsers produce the same normalised tibble names and column names; test both paths when adding new features that read from those tibbles.
- **Code generation is string-based**: method templates contain literal placeholder strings (e.g. `"analysisidhere"`, `"methodidhere"`) replaced with `gsub()`. Don't confuse these with R variables.
- **jsonlite produces nested data frames** (not list columns) when `simplifyDataFrame = TRUE`. The recursive helper `.extract_lopa_ids()` in `metadata.R` relies on this structure to traverse arbitrary-depth `sublist` nesting in `mainListOfContents`.
- **`analysisId` must be coerced to `character`** when binding rows from JSON because JSON `null` parses as logical `NA`, causing type conflicts in `dplyr::bind_rows`.
- **Dynamic grouping** (`AG_vars`, `AG_var1/2/3…`) supports 1–N grouping factors; the code generation branches on how many `resultsBy` groupings are present in `AnalysisGroupings`.
- **`n_group_cols` determination** — the number of `group[n]` columns a method produces differs from `num_grp` when the last grouping goes to `variables=` instead of `by=`. This is determined by `.n_group_cols_from_template()` in `readARS.R`, which checks which valueSource types (`by_vars`, `strata_vars`, `by_listc`) appear in parameters whose placeholder tokens are actually present in the method's code template. Never check the parameter table alone — it can contain spurious rows whose placeholder tokens are absent from the template (false positives). Never check the generated code string — the runtime-resolved values won't match the placeholder names. **Practical consequence**: `by_stmt` resolves to `n_group_cols = 0` (no group stamping, no `group1_groupingId` etc. in the ARD). Any method that must emit CDISC-compliant group metadata columns must use `by_listc` (or `by_vars`/`strata_vars`) — not `by_stmt`.
- **`group_condition_value` in JSON is a list-column** — jsonlite always returns `condition.value` as a list, even for single-element EQ arrays. The JSON parser must call `tidyr::unnest(tmp_AG, cols = "group_condition_value")` after building `tmp_AG` to expand IN/NOTIN multi-value conditions (one row per value, `group_id` repeated) and collapse single-value list elements to plain character. Use a quoted string in `cols=` to avoid R CMD check "no visible binding" NOTE.
- **`_level` columns and `bind_rows` type conflicts** — cards returns `variable_level` as a list-of-NULLs for continuous analyses and as character for categorical ones. When combined with `bind_rows`, this creates list-type columns containing "NULL" strings. Fix: generate a per-df `mutate(across(matches("_level$"), vapply(..., function(v) if(is.null(v)) NA_character_ else as.character(v), character(1L))))` block before the final `bind_rows`. Do NOT use `across(where(is.list), as.character)` — that converts the numeric `stat` list column to character, breaking downstream `round()` calls.
- **CDISC ARD compliance** — each ARD row must carry: `AnalysisId`, `operationid`, and per-resultGroup: `group[n]_groupingId` + either `group[n]_groupId` (pre-defined groups, via `case_when` on `group[n]_level`) or `group[n]_groupValue` (data-driven groupings). See `.generate_groupid_code()` in `readARS.R`.
- **Internal functions** are prefixed with `.` (e.g. `.read_ars_metadata`). Only four symbols are exported: `readARS`, `ARS_example`, `ARD_script_example`, `%>%`.

## Coding standards (pharmaverse / admiral style)

- **Tidyverse over base R**: use `dplyr`, `tidyr`, `stringr`; pipe with `|>` (prefer native pipe over `%>%`).
- **cli for output**: `cli::cli_abort()` not `stop()`, `cli::cli_warn()` not `warning()`, `cli::cli_inform()` not `message()`.
- **Fail fast**: check arguments early with `is.na()`, `nchar()`, `is.null()` guards; emit errors pointing at the bad input.
- **No global state**: all inputs must be explicit arguments; no reads from or writes to global objects.
- **Function naming**: internal helpers prefixed with `.` (e.g. `.generate_analysis_set_code()`); exported functions in camelCase (e.g. `readARS()`, `ARS_example()`).
- **Comments**: explain the *why*, not the *what*; section markers end with `----` for RStudio outline (e.g. `# Load ADaMs ----`).

## ADaM dataset conventions

siera reads ADaM datasets to generate loading code and resolve variable references.

### Common datasets
| Dataset | Contents |
|---|---|
| `ADSL` | One row per subject; demographics, treatment arms, flags |
| `ADAE` | Adverse events (one row per event) |
| `ADVS` | Vital signs parameters |
| `ADLB` | Laboratory parameters |
| `ADEX` | Exposure |
| `ADEXSUM` | Exposure summary |

### Key variable conventions
| Variable | Meaning |
|---|---|
| `STUDYID` / `USUBJID` | Study and unique subject identifiers |
| `PARAMCD` / `PARAM` | Parameter code / label (long datasets) |
| `AVAL` / `AVALC` | Analysis value (numeric / character) |
| `TRT01P` / `TRT01A` | Planned / actual treatment (period 1) |
| `AVISIT` / `AVISITN` | Analysis visit (character / numeric) |
| `SAFFL` / `ITTFL` | Safety / ITT population flags (`"Y"` / `""`) |
| `--FL` suffix | Flag variables (Y/blank pattern) |
| `--DT` / `--DTM` / `--DTC` | Date / datetime / character date variables |

ARS `analysisSets` map to ADaM flag variables (e.g. `ADSL.SAFFL EQ "Y"`); siera translates these into `dplyr::filter()` calls. When `dataDriven: true`, groups are discovered from ADaM data at runtime.

## Pharmaverse ecosystem context

| Package | Role |
|---|---|
| `{admiral}` | ADaM dataset creation (upstream of siera) |
| `{cards}` | ARD computation (`ard_tabulate()`, `ard_summary()`, etc.) |
| `{cardx}` | Extended ARD statistics (models, tests) |
| `{gtsummary}` | TFL rendering from ARDs |
| `{admiraldev}` | Development utilities for pharmaverse packages |

siera sits between ARS metadata and `{cards}` — it generates the `cards`-based R code a statistician would otherwise write by hand.

## GitHub API access

`gh` CLI is **not installed** on this machine. Use the GitHub REST API directly via PowerShell instead.

**Step 1 — retrieve the stored token** (Git Credential Manager holds a GitKraken OAuth token):
```powershell
$token = (printf "protocol=https\nhost=github.com\n" | git credential fill | Select-String "^password=").ToString().Replace("password=","").Trim()
```

Or retrieve it once and reuse in the session:
```bash
# in Bash tool
printf "protocol=https\nhost=github.com\n" | git credential fill
# copy the password= line value
```

**Step 2 — set up headers**:
```powershell
$token   = "gho_..."   # from step 1
$headers = @{ Authorization = "token $token"; Accept = "application/vnd.github.v3+json" }
$repo    = "clymbclinical/siera"
```

**Common operations:**

```powershell
# Create an issue
$body = @{ title = "..."; body = "..." } | ConvertTo-Json
Invoke-RestMethod -Uri "https://api.github.com/repos/$repo/issues" -Method Post -Headers $headers -Body $body -ContentType "application/json"

# Create a PR
$body = @{ title = "..."; head = "branch-name"; base = "main"; body = "..." } | ConvertTo-Json
Invoke-RestMethod -Uri "https://api.github.com/repos/$repo/pulls" -Method Post -Headers $headers -Body $body -ContentType "application/json"

# List open issues
Invoke-RestMethod -Uri "https://api.github.com/repos/$repo/issues?state=open" -Headers $headers

# Get a specific issue
Invoke-RestMethod -Uri "https://api.github.com/repos/$repo/issues/139" -Headers $headers
```

The token is an OAuth token (`gho_` prefix) stored in Windows Credential Manager by GitKraken. It has repo scope and works for all standard issue/PR/label operations.

## Git / branch workflow

- Branch naming: `<issue-number>-short-description` (e.g. `137-fix-windows-adam-paths`).
- All work goes to feature branches; PRs target `main`. Never push directly to `main`.
- Commit messages: imperative mood, short (e.g. `Fix #139: update cards_constructs.xlsx`).
- Every PR must be linked to a GitHub issue. Inform the user when editing siera source files if no issue exists yet.

## cards / cardx API alignment

The generated R scripts and example scripts in `inst/script/` must always use the current `cards` API. Key function renames from cards ≥ 0.7.0:

- `ard_categorical()` → `ard_tabulate()`
- `ard_continuous()` → `ard_summary()`
- `ard_complex()` → `ard_mvsummary()`

When updating example scripts or code templates, check the cards changelog for any further deprecations. The `strata` argument exists in `ard_tabulate()` (unlike the old `ard_categorical()`) for computing percentages over observed combinations only — see cards docs for the `by` vs `strata` distinction.

**`ard_tabulate()` denominator pattern:** For categorical n(%) analyses with a pre-computed denominator, the correct structure is `by = 'group_var', variables = 'analysis_var'` with a pre-counted denominator data frame that has the count column renamed to `...ard_N...`. The old pattern of `by = c('group_var', 'analysis_var'), variables = 'dummy_var'` with a raw data frame as denominator is no longer correct. Example:
```r
denom <- df_pop |> dplyr::count(TRT01A) |> dplyr::rename(`...ard_N...` = n)
cards::ard_tabulate(data = in_data, by = 'TRT01A', variables = 'AGEGR1', denominator = denom)
```

Do not modify the test fixture xlsx files in `inst/extdata/` — the package owner updates those separately. The example R scripts in `inst/script/` are pre-generated and committed; regenerate them manually using `readARS()` after the xlsx templates are updated, then adjust the ADaM-loading lines to use `siera::ARS_example()`.

## Test conventions

- **Every code change must be accompanied by a test.** If you add a feature, add a test that would fail without it. If you fix a bug, add a test that reproduces the bug. Do not submit a PR without new or updated tests covering the change.
- Integration tests source the generated `.R` script into an isolated `new.env(parent = baseenv())` and inspect the resulting `ARD` object.
- Long-running tests (those that source generated scripts against real ADaM data) are guarded with `skip_on_cran()`.
- Temp directories use `withr::local_tempdir()` for automatic cleanup.
- Example ARS files live in `inst/extdata/` (`exampleARS_1` – `exampleARS_6`, both `.json` and `.xlsx`). Retrieve them in tests via `ARS_example("exampleARS_6.json")`.
- **eTFL Portal integration testing** — `tests/testthat/testdata/etfl/` holds the CDISC eTFL Portal fixtures unzipped into three subfolders (no `.zip` files are committed): `metadata/<table>-siera.json` (ARS metadata enriched with `codeTemplate` + `parameters` so siera emits runnable scripts), `adam/<table>/*.xpt` (the ADaM data for that table), and `reference/<table>-ard.json` (the published reference ARD). `test-etfl-regression.R` runs the full pipeline for each of the 12 tables and compares the computed statistics against the reference ARD; the machinery lives in `helper-etfl.R` (`.run_etfl_pipeline()` reads XPT via `haven::read_xpt()` → CSV → `readARS()` → sources the script; `.load_etfl_reference()`; the `.cmp_bigN/.cmp_n_pct/.cmp_n_pct_2level/.cmp_continuous/.cmp_rd` comparators; and `.expect_all_match()`). Reference ARDs are Dataset-JSON; load with `jsonlite::fromJSON(..., simplifyDataFrame=FALSE, simplifyVector=FALSE)` to avoid row-parsing failures. `fda-ds-t04` derives `DISCONFL` from `DCTREAS` via the `adsl_transform` hook. Known-limitation analyses are exercised but not asserted: 0-event risk difference returns `NA` (#156), per-term/per-category RD is not emitted (#157), and top-N AE per-term subject counts can differ (#158). The raw XPT/JSON is ~28 MB in the working tree but git-packs and tarball-gzips to ~2 MB, so it ships fine for CRAN.

## Notes: 

- Always delete code or datasets used for temporary testing once done (keep the repo clean).
- After a reasonable development milestone, capture learnings and update CLAUDE.md with anything useful for future sessions.
- Notify the user when there are strong candidates for creating a Claude skill — repeated prompts or processes that would benefit from automation.
- Target audience are clinical trial industry members; whenever logic or usage changes, suggest education material or a vignette idea.
- Inform the user before editing siera source files if there is no linked GitHub issue — all changes should be traceable to an issue and land on a feature branch.
- Suggest documentation updates when features are added or functionality changes (this is a public-facing CRAN package).
- Add tests for all new code; ensure every new branch has test coverage for its changes.
- Ask clarifying questions and check for understanding when there is uncertainty about a request or approach.

