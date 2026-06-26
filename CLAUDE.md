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

**Test coverage**: cover every new/changed line and branch with tests *as part of the change*, so the `codecov/patch` check (effectively 100% patch target) is already green when you push — treat codecov as a backstop, not a TODO generator. Deliberately exercise the easy-to-miss paths: default-argument branches (a `function(x = NULL)` whose default body is bypassed when every test passes the arg — add a no-arg test), error/guard branches (`cli::cli_abort()` → `expect_error`), and each arm of `if`/`switch`/`case_when`. Remove dead/untestable code rather than shipping it uncovered. Verify locally when unsure with `covr::file_coverage()` (a few files) or `covr::package_coverage()` + `covr::zero_coverage()`. (gh CLI is unavailable; if you do need to read codecov status, use the GitHub REST check-runs endpoint and the Codecov PR comment.)

## Key files

| File | Purpose |
|---|---|
| `R/readARS.R` | Main entry point — `readARS()` orchestrates the full pipeline |
| `R/metadata.R` | Parses ARS JSON/XLSX into flat R data frames |
| `R/AnalysisSet.R` | Generates analysis set (population) filter code |
| `R/DataSubsets.R` | Generates data subset filter code |
| `R/AnalysisMethods.R` | Resolves method code templates and valueSource parameters |
| `R/loadADaM.R` | Generates ADaM dataset loading code (CSV via `readr::read_csv()` or XPT via `haven::read_xpt()`, chosen per dataset by file extension) |
| `R/DatasetJSON.R` | Generates the optional CDISC Dataset-JSON export block appended to ARD scripts (`output_format = "datasetjson"`) |
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
- **Empty-data guard** — generated method code is wrapped in `if(nrow(df2_<analysisid>) != 0){ ... } else { df3_<analysisid> = data.frame(AnalysisId, MethodId, OutputId) }` (see `.generate_analysis_method_section()` in `AnalysisMethods.R` ~L94/L135 and `.generate_groupid_code()` in `readARS.R` ~L495). When a data subset is empty the cards call is skipped and only a stub row (no statistics) is emitted. This is correct for n/continuous ("no data → no stats"). **#156 fix (population-based bypass):** methods whose template references `df_poptot` compute over the full population (e.g. risk differences via `prop.test` on a flag joined onto `df_poptot`) and stay valid on an empty `df2` — a 0-event RD must yield `RD = 0`, `CI = [0, 0]`, not a stub/NA. `.generate_analysis_method_section()` sets `population_based <- grepl("df_poptot", template_code, fixed = TRUE)` and, when true, drops all three `nrow(df2)!=0` guards (the body wrap, the df3 identifier-stamp `if/else`, and — via the `population_based` arg threaded into `.generate_groupid_code()` — the group-stamp wrap). `population_based` is returned in the method-section result list. The non-empty path is unchanged: the guard only ever affected the empty case. `prop.test(c(0,0), c(n1,n2), correct = FALSE)` returns estimate/CI all `0`, matching the CDISC `fda-ae-t06 An_34` reference.
- **Dataset-JSON export** (`output_format = "datasetjson"`, issue #160) — appends a self-contained block (built by `.generate_datasetjson_code()` in `R/DatasetJSON.R`) to each generated `ARD_<Output>.R`. The block runs *at script runtime* (not generation time) because the ARD's column set is only known after sourcing; it introspects `names(ARD)`, derives Dataset-JSON column metadata, and writes `ARD_<Output>.json` beside the script via the optional `{datasetjson}` package (Suggests, guarded with `requireNamespace()` in the emitted code — absent package → message, not error). Two runtime gotchas it handles: (a) the ARD carries **four `{cards}` list-columns** (`stat`, `fmt_fun`, `warning`, `error`) that must be flattened to atomic — `stat`→numeric, the rest→character; (b) variable **labels** use a built-in dictionary for siera + `{cards}` vocabulary plus a `group[n]_(groupingId|groupId|groupValue|level)` regex, falling back to the raw name for data-driven ADaM columns. NB: the `code_pattern` block in `readARS.R` is dead code — `pattern`/`res`/`disp` are never produced; the numeric result lives in the `stat` list-column.
- **ADaM input format (CSV or XPT, issue #161)** — `adam_path` may hold CSV (`.csv`) or SAS transport (`.xpt`) ADaM files; siera picks the reader **per dataset at generation time** from the extension found on disk (no `readARS()` argument, mirroring `.json`/`.xlsx` ARS dispatch). `.generate_one_adam_read()` in `R/loadADaM.R` resolves the file: it prefers an exact-case match, else does a **case-insensitive** `list.files()` lookup (real submission XPTs are lower-case like `adsl.xpt` while ARS names datasets upper-case `ADSL`), and emits the real on-disk path. `.csv`→`readr::read_csv()`, `.xpt`→`haven::read_xpt()`; **CSV wins when both exist**; when neither exists it falls back to the conventional `<dataset>.csv` path (so example/placeholder `tempdir()` folders still generate csv code, keeping old tests/examples green). `haven` is in Suggests (was added for the eTFL suite) and the emitted call is namespace-qualified — no `library(haven)` is generated, so xpt-free scripts don't require haven. The object on the LHS is always the metadata dataset name regardless of the file's case.
- **Internal functions** are prefixed with `.` (e.g. `.read_ars_metadata`). Only five symbols are exported: `readARS`, `ARS_example`, `ARD_script_example`, `method_library` (accessor for the bundled `inst/method-library` templates), `%>%`.

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

**Non-ASCII bodies (issue/PR markdown):** Windows PowerShell 5.1 sends `Invoke-RestMethod -Body` using the console ANSI codepage, which mangles any non-ASCII (em-dashes, arrows `→`/`←`, box-drawing `└─├─►`) — GitHub then 400s with `Problems parsing JSON` or `Invalid request … properties/body`. Fixes that work: (1) read the body file with `[System.IO.File]::ReadAllText($p,[Text.Encoding]::UTF8)` (NOT `Get-Content -Raw`, which reads ANSI), (2) `ConvertTo-Json -Depth 5`, then send `[Text.Encoding]::UTF8.GetBytes($payload)` as `-Body` with `-ContentType "application/json; charset=utf-8"`. Simplest robust option: pre-sanitise the body to ASCII first (`-replace [char]0x2014,'-'` etc.; note `String.Replace(char,char)` rejects multi-char targets like `->`, so use the `-replace` operator for those). Authoring issue bodies in ASCII from the start avoids the whole problem.

## Git / branch workflow

- Branch naming: `<issue-number>-short-description` (e.g. `137-fix-windows-adam-paths`).
- All work goes to feature branches; PRs target `main`. Never push directly to `main`.
- Commit messages: imperative mood, short (e.g. `Fix #139: update cards_constructs.xlsx`).
- Every PR must be linked to a GitHub issue. Inform the user when editing siera source files if no issue exists yet.

### Parallel work / multiple sessions (use git worktrees)

More than one Claude session may be active on this repo at once (typically one per issue). Sessions that share a single working tree also share **one index and one HEAD**, so any `git checkout`, `git stash`, `git reset`, or `git commit` in one session silently corrupts the other (e.g. a `git stash -u` to start a branch will drag a parallel session's uncommitted WIP onto your branch, and your commit can land on their branch).

- **Before stashing or switching branches, check `git branch --show-current` and whether a parallel session is mid-edit.** If the working tree has untracked/modified files you did not create, a parallel session is likely active — stop and use a worktree instead.
- **For genuinely parallel work, give each line of work its own checkout with `git worktree`** (branches, commits, and pushes are shared via the common `.git`; only the working files are isolated):
  - New branch: `git worktree add -b <issue>-<slug> ../siera-<issue> origin/main`
  - Existing branch: `git worktree add ../siera-<issue> <existing-branch>`
  - Run that session with its working directory pointed at the new folder; edit/commit/push there.
  - Clean up after merge: `git worktree remove ../siera-<issue>`.
  - A branch can be checked out in only one worktree at a time (git enforces this). Stagger `devtools::check()`/`install()` across worktrees — they share the same installed-package library.
- **Never `git stash` in a shared tree, and never `git add -A`** — stage only your own explicit paths.
- **Never `git reset --hard` to undo a misplaced commit in a shared tree** — it destroys a parallel session's modified tracked files. To extract a commit onto its own branch without touching the working tree: `git branch -f <target-branch> <commit>`; `git reset --soft origin/main`; `git restore --source=origin/main --staged --worktree -- <only your files>`.

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

**Where the cards API in generated output actually comes from:** the `cards`/`cardx` function names and argument patterns in generated (and `inst/script/`) ARDs are copied verbatim from the ARS metadata's `AnalysisMethodCodeTemplate.templateCode` — siera only substitutes the placeholder parameters, it does not rewrite the cards calls. **Consequence:** you cannot bring generated scripts onto a newer cards API by editing siera's R code or the vignettes alone; the example metadata's `templateCode` must be updated. The bundled `Common_Safety_Displays_cards.xlsx` carries the templates in the `AnalysisMethodCodeTemplate` sheet (`templateCode` column, 5 rows). A name-only swap (`ard_categorical`→`ard_tabulate`, `ard_continuous`→`ard_summary`) runs fine on current cards — `ard_tabulate()` still accepts the legacy `by = c('grp','var'), variables = 'dummy', denominator = raw_df` pattern, so the "old" denominator pattern above still executes and yields identical results even though the `...ard_N...` form is preferred for new templates.

Do not modify the test fixture xlsx files in `inst/extdata/` without explicit owner authorisation — the package owner normally updates those separately. (In PR #162 the owner did authorise renaming the cards functions in `templateCode`; that edit was made with `openxlsx::loadWorkbook()` → `writeData()` on the single column → `saveWorkbook()`, which preserves all 24 sheets — verify the sheet count and re-run `readARS()` afterwards.) The example R scripts in `inst/script/` are pre-generated and committed; regenerate them manually using `readARS()` after the xlsx templates are updated, then `gsub()` the ADaM-loading `readr::read_csv('…/<DS>.csv'` lines to `readr::read_csv(siera::ARS_example("<DS>.csv")`.

## Test conventions

- **Every code change must be accompanied by a test.** If you add a feature, add a test that would fail without it. If you fix a bug, add a test that reproduces the bug. Do not submit a PR without new or updated tests covering the change.
- Integration tests source the generated `.R` script into an isolated `new.env(parent = baseenv())` and inspect the resulting `ARD` object.
- Long-running tests (those that source generated scripts against real ADaM data) are guarded with `skip_on_cran()`.
- Temp directories use `withr::local_tempdir()` for automatic cleanup.
- Example ARS files live in `inst/extdata/` (`exampleARS_1` – `exampleARS_6`, both `.json` and `.xlsx`). Retrieve them in tests via `ARS_example("exampleARS_6.json")`.
- **eTFL Portal integration testing** — `tests/testthat/testdata/etfl/` holds the CDISC eTFL Portal fixtures unzipped into three subfolders (no `.zip` files are committed): `metadata/<table>-siera.json` (ARS metadata enriched with `codeTemplate` + `parameters` so siera emits runnable scripts), `adam/<table>/*.xpt` (the ADaM data for that table), and `reference/<table>-ard.json` (the published reference ARD). `test-etfl-regression.R` runs the full pipeline for each of the 12 tables and compares the computed statistics against the reference ARD; the machinery lives in `helper-etfl.R` (`.run_etfl_pipeline()` reads XPT via `haven::read_xpt()` → CSV → `readARS()` → sources the script; `.load_etfl_reference()`; the `.cmp_bigN/.cmp_n_pct/.cmp_n_pct_2level/.cmp_continuous/.cmp_rd` comparators; and `.expect_all_match()`). Reference ARDs are Dataset-JSON; load with `jsonlite::fromJSON(..., simplifyDataFrame=FALSE, simplifyVector=FALSE)` to avoid row-parsing failures. `fda-ds-t04` derives `DISCONFL` from `DCTREAS` via the `adsl_transform` hook. The `.cmp_pergroup_rd()`/`.etfl_pergroup_rd_truth()` helpers assert per-category RD (#157, see below). Remaining known-limitation analyses exercised but not asserted: per-(SOC × PT) RD (fda-ae-t36 An_57, a 3-grouping case `Mth_03_1a` doesn't cover) and fda-ae-t06's per-action/per-severity RD (still on single-RD `Mth_03_1`). The raw XPT/JSON is ~28 MB in the working tree but git-packs and tarball-gzips to ~2 MB, so it ships fine for CRAN.
  - **#158 — fda-ae-t13 An_80 arm × PT counts (resolved: reference defect, not a siera bug).** The published reference ARD for An_80 was generated **without** the treatment-emergent filter (`TRTEMFL == "Y"`) that the analysis's own ARS metadata mandates via `Dss_04`, so its per-term distinct-subject counts are inflated for the cells whose extra subjects have only non-treatment-emergent occurrences of a PT (concretely: subject `01-701-1192`'s sole COUGH event is `TRTEMFL` blank, yet the reference counts it). siera applies `Dss_04` correctly. Proof: an independent ground truth computed from the raw ADaM **with** the TE filter matches siera exactly on all 354 non-zero arm × PT cells / 230 terms; **without** the filter it matches the reference exactly. The sister TEAE table fda-ae-t12 (whose subset includes `TRTEMFL == "Y"`) matches its reference, confirming the reference tool normally applies the filter — t13 An_80 is the anomaly. So An_80 is now **asserted** against this independent TE ground truth (helpers `.etfl_ae_pt_te_truth()` / `.cmp_ae_pt_te()`), not against the defective reference ARD. The earlier "siera under-counts" framing was the reference over-counting.
  - **#157 — per-category risk difference (resolved via new method `Mth_03_1a`).** The single-RD method `Mth_03_1` collapses an analysis to one overall risk difference; tables needing one RD per data-driven inner category (fda-ae-t13 An_81/An_81_1 per PT; fda-ae-t36 An_55/An_55_1 per SOC) need a variant that loops the inner grouping. `Mth_03_1a` (added to the t13 + t36 `-siera.json` metadata, and as a sheet in the owner master workbook `R(siera)_codes_*.xlsx`) loops `sort(unique(as.character(df2$AG_var2)))`, builds a 0/1 `FL` on `df_poptot`, and calls **`cardx::ard_stats_prop_test(by = AG_var1, variables = FL, correct = FALSE)`** per category (`estimate`/`conf.low`/`conf.high` × 100, with `group2_level` stamped). It uses only existing valueSources (`AG_var1`, `AG_var2`, `ana_var`, `operation_1..3`) — **no `R/readARS.R` change needed**. The two comparison arms are self-derived from `df2` (`utils::head(sort(unique(...)), 2)`), so no arm-filter construct (`trt_filter_stm`) is required; referencing `df_poptot` makes it `population_based`. **Both references are unsafe oracles for RD**: t13's omits the TE filter (the #158 defect — e.g. DIARRHOEA Low-vs-Placebo reference -4.5 vs spec-correct -5.7), and t36's rounds component percentages before differencing. So per-category RD is asserted against an **independent** `stats::prop.test(correct = FALSE)` recomputation on distinct-subject counts (`.etfl_pergroup_rd_truth()` / `.cmp_pergroup_rd()`), full-join matching every category × statistic — siera (cardx) matched it exactly on all 180/187 PTs and 22 SOCs. Remaining gaps tracked as follow-ups: fda-ae-t06 per-action/severity RD (just a repoint to `Mth_03_1a`, #171) and fda-ae-t36 An_57 per-(SOC × PT) RD (needs a 3-grouping variant `Mth_03_1b`, #172). The owner master code-template library is git-tracked at `inst/extdata/R_siera_codes.xlsx` (one sheet per method, e.g. `Risk Difference (per grp)`, plus a `Constructs` registry) — add new methods there too. It is **excluded from the CRAN tarball** via `.Rbuildignore` (`^inst/extdata/R_siera_codes`), so it never ships; edit it with `openpyxl` (load → `create_sheet` → save) to preserve the other sheets. To author further methods see the `siera-author-method` skill.
  - **#173 — method-template library formalised as plain text (supersedes the xlsx as source of truth).** The owner xlsx workbooks (`inst/extdata/R_siera_codes.xlsx`, `cards_constructs.xlsx`) are reference-only (siera never reads them at runtime) and had silently drifted: the single `Risk Difference` and `Fisher's Exact test` sheets referenced the **unsupported** valueSource `trt_filter_stm` and carried a `case_when` copy-paste bug (`conf.low` mapped twice, `conf.high` dropped), and `Total N`/`Categorical`/`Continuous` used the deprecated `ard_categorical`/`ard_continuous` names. The new **source of truth** is `inst/method-library/` — one folder per method (`<NN_key>/method.json` + `template.R`), a reconciled `constructs.json` (only valueSources siera actually resolves; legacy `num_grp_any`/`anset_cond_stm`/`fisher_cond_stm`/`AG_ds1`/`trt_filter_stm` are intentionally absent, `by_list` added), and a **generated** `METHODS.md` catalog. siera's authoritative valueSource list is declared in `R/value_sources.R` (`.supported_value_sources()`), kept in sync with the `value_sources` assembly in `R/readARS.R`. The catalog is produced by `.render_method_library_md()` in `R/render_method_library.R`; regenerate with `writeLines(siera:::.render_method_library_md(), "inst/method-library/METHODS.md")`. `tests/testthat/test-method-library.R` is the contract test (templates parse after dummy substitution, only supported valueSources, no orphan/unknown `…here` tokens, no duplicated `case_when` LHS, `opidN` ≤ declared operations, `constructs.json` == `.supported_value_sources()`, and `METHODS.md` not stale). The legacy xlsx files are left untouched. **Intended future direction:** ARS metadata references a library method by its `id` instead of embedding `templateCode` inline. Add new methods here (not the xlsx); there is intentionally no generated xlsx.

## Documentation and vignettes

- **Always review the full documentation set after any change — every time, before opening a PR.** Do not only update the docs you assume are affected. Walk the whole set and check each for needed updates: all five vignettes, `README.md`/`README.Rmd`, `NEWS.md`, `_pkgdown.yml`, `CLAUDE.md`, `man/` (via `document()`), and any relevant `.claude/skills/`. Stale docs that merely *mention* a file or workflow you changed (e.g. a vignette pointing at a now-superseded reference) count as needed updates. Treat this documentation review as a required, non-skippable pre-PR step.
- **Keep documentation and functionality in sync.** Any user-visible change — a new feature, a behaviour change, a change to generated-script or ARD output, or a new capability the user should know about — must be reflected in the documentation (vignettes, README, and `NEWS.md`) as part of the same change, not deferred. Treat user-facing docs as part of the definition of done. When in doubt about whether a change is user-visible, assume it is and update the docs. The `siera-docs-refresh` skill automates a full documentation pass.
- Five vignettes, ordered via the number prefix in `%\VignetteIndexEntry{}`: 1. `Getting_started`, 2. `concepts` (Concepts and conventions), 3. `using-cards`, 4. `ARD_script_structure`, 5. `apply-ARD`. Keep the prefixes in sync with `_pkgdown.yml` if reordering.
- `_pkgdown.yml` groups the Articles navbar (Get started / Concepts / Guides / Next steps). The reference index is intentionally left ungrouped.
- The pipeline diagram is a hand-authored static **SVG** (CRAN-safe, no new dependency), committed twice: `vignettes/figures/siera-pipeline.svg` (used by `concepts.Rmd` via `knitr::include_graphics()`) and `man/figures/siera-pipeline.svg` (used by the README / pkgdown home). Update both copies together.
- **Verifying vignettes without Pandoc:** full `rmarkdown::render()` / `build_vignettes()` needs Pandoc (unavailable locally), but `knitr::knit(vig, output = tempfile())` executes all chunks without Pandoc — use it to confirm chunks run. `Getting_started.Rmd` has a **live** chunk that `source()`s an `inst/script/` example and prints `head(ARD)`, so it requires `cards`/`cardx` installed and the example scripts to be current.
- Tone is conversational, second-person, tutorial-style with inline-commented chunks; match it when editing docs. Audience is clinical-trial practitioners, not R developers.
- `README.Rmd`'s install chunks lack `eval=FALSE`, so re-knitting would actually run `install.packages()`/`install_github()`. For small static edits, change `README.Rmd` and `README.md` by hand in sync rather than re-knitting.

## Notes: 

- Always delete code or datasets used for temporary testing once done (keep the repo clean).
- After a reasonable development milestone, capture learnings and update CLAUDE.md with anything useful for future sessions.
- Notify the user when there are strong candidates for creating a Claude skill — repeated prompts or processes that would benefit from automation.
- Target audience are clinical trial industry members; whenever logic or usage changes, suggest education material or a vignette idea.
- Inform the user before editing siera source files if there is no linked GitHub issue — all changes should be traceable to an issue and land on a feature branch.
- Suggest documentation updates when features are added or functionality changes (this is a public-facing CRAN package).
- Add tests for all new code; ensure every new branch has test coverage for its changes.
- Ask clarifying questions and check for understanding when there is uncertainty about a request or approach. Rather ask than guess.
- when making a PR, ensure the updates to claude.md are pushed with it. 
- ensure the package structure is in line with best practices for CRAN R Packages, and comparable to pharmaverse R packages. 

