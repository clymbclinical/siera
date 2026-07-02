# siera method-template catalog

_Generated from `inst/method-library/` by `.render_method_library_md()` - do not edit by hand._
_Regenerate: `writeLines(siera:::.render_method_library_md(), "inst/method-library/METHODS.md")`._

siera generates ARD code by substituting placeholder tokens in a method's
`templateCode` with values resolved per analysis from ARS metadata. Each method
below is one reusable recipe; its `id` is the stable key an ARS file can
reference.

## Methods

| id | label | status | operations | verified against |
|----|-------|--------|------------|------------------|
| `total_n` | Total N (per group) | verified | n | Common_Safety_Displays_cards.xlsx Mth01_CatVar_Count_ByGrp |
| `categorical_summary` | Categorical n (%) | verified | n, p | Common_Safety_Displays_cards.xlsx Mth01_CatVar_Summ_ByGrp |
| `continuous_summary` | Continuous summary (N, mean, SD, median, Q1, Q3, min, max) | verified | N, mean, sd, median, p25, p75, min, max | Common_Safety_Displays_cards.xlsx Mth02_ContVar_Summ_ByGrp |
| `risk_difference` | Risk difference + 95% CI | verified | Risk_Difference_%, 95%_CI_Low, 95%_CI_High | tests/testthat/testdata/etfl/metadata/fda-ae-t06-siera.json Mth_03_1 |
| `risk_difference_per_group` | Risk difference + 95% CI (per group) | verified | Risk_Difference_%, 95%_CI_Low, 95%_CI_High | tests/testthat/testdata/etfl/metadata/fda-ae-t13-siera.json Mth_03_1a |
| `fishers_exact` | Fisher's exact (odds ratio + 95% CI) | corrected-unvalidated | estimate, conf.low, conf.high | - |
| `chisq` | Chi-square p-value | verified | p.value | Common_Safety_Displays_cards.xlsx Mth03_CatVar_Comp_PChiSq |
| `anova` | ANOVA p-value | verified | p.value | Common_Safety_Displays_cards.xlsx Mth04_ContVar_Comp_Anova |

## Supported valueSources

| valueSource | type | assumed token | details |
|-------------|------|---------------|---------|
| `ana_var` | simple | `anavarhere` | Analysis variable name |
| `AG_var1` | simple | `groupvar1here` | First analysis grouping variable |
| `AG_var2` | simple | `groupvar2here` | Second analysis grouping variable |
| `AG_var3` | simple | `groupvar3here` | Third analysis grouping variable |
| `DEN_analysisid` | simple | `denomanaidhere` | Analysis ID of the referenced denominator analysis |
| `AG_denom_var1` | simple | `denom_anagroupvarshere` | First grouping variable of the referenced denominator analysis |
| `AG_max_dataDriven` | simple | `isdatadrivenhere` | TRUE/FALSE: whether the highest resultsByGroup grouping is data-driven |
| `distinct_list` | complex | `distinctlisthere` | All active grouping variables plus the analysis variable, comma separated, unquoted (for dplyr::distinct) |
| `by_listc` | complex | `byvarshere` | All grouping variables as a quoted, comma-separated list (to be wrapped in c(...)). Stamps CDISC group metadata. Preferred for grouped tabulations. |
| `by_list` | complex | `bylisthere` | All grouping variables as an UNQUOTED, comma-separated list. Same vars as by_listc but without quotes. |
| `by_vars` | complex | `bystmthere` | Leading-comma 'by/variables' argument string: the last grouping goes to variables=, the rest to by=. Stamps CDISC group metadata. The raw token is invalid R until substituted (it injects a leading comma). |
| `strata_vars` | complex | `stratastmthere` | Leading-comma 'strata/variables' argument string (data-driven analogue of by_vars). Stamps CDISC group metadata. |
| `by_stmt` | complex | `byhere` | Leading-comma 'by = ...' argument string (all groupings to by=). CAUTION: resolves to n_group_cols = 0, so siera stamps NO group[n] metadata columns - do not use where CDISC-compliant group metadata is required; prefer by_listc / by_vars / strata_vars. |
| `operation_N` | pattern | `opidNhere` | operation_1 ... operation_N: the method's own operation IDs in 'order'. Matched by pattern (^operation_[0-9]+$), never declared as a parameter. |

---

## `total_n` - Big N denominator count by group

Distinct count of the analysis variable (typically USUBJID) per treatment group. Used as the N= header / denominator source. Verified against Common_Safety_Displays Mth01_CatVar_Count_ByGrp; current cards API.

**Status:** verified &nbsp; **Verified against:** Common_Safety_Displays_cards.xlsx Mth01_CatVar_Count_ByGrp

**Operations**

| order | stat_name | label | resultPattern |
|-------|-----------|-------|---------------|
| 1 | `n` | Count | `xx` |

**Parameters**

| token | valueSource | label | description |
|-------|-------------|-------|-------------|
| `anavarhere` | `ana_var` | ana var | Analysis variable (e.g. USUBJID) |
| `groupvar1here` | `AG_var1` | grp var 1 | First grouping variable (treatment arm) |
| `bystmthere` | `by_vars` | by stmt | by/variables argument string for the grouping(s) |

**Template**

```r
in_data = df2_analysisidhere |>
    dplyr::select(anavarhere, groupvar1here) |>
    unique()
df3_analysisidhere <-
  cards::ard_tabulate(
    data = in_data
    bystmthere
  ) |>
  dplyr::filter(stat_name == 'n') |>
  dplyr::mutate(operationid = 'opid1here')
```

---

## `categorical_summary` - Summary of a categorical variable (n and %)

n and percentage of a categorical analysis variable per group, with a referenced denominator analysis and a data-driven/pre-defined grouping branch. Verified against Common_Safety_Displays Mth01_CatVar_Summ_ByGrp; current cards API. Spurious by_vars/strata_vars params from the legacy sheet have been dropped (the template uses by_listc).

**Status:** verified &nbsp; **Verified against:** Common_Safety_Displays_cards.xlsx Mth01_CatVar_Summ_ByGrp

**Operations**

| order | stat_name | label | resultPattern |
|-------|-----------|-------|---------------|
| 1 | `n` | Count | `xx` |
| 2 | `p` | Percentage | `(xx.x)` |

**Parameters**

| token | valueSource | label | description |
|-------|-------------|-------|-------------|
| `distinctlisthere` | `distinct_list` | distinct list | Grouping variables + analysis variable, comma separated |
| `denomanaidhere` | `DEN_analysisid` | denom ana id | Analysis ID supplying the denominator |
| `denom_anagroupvarshere` | `AG_denom_var1` | denom grp var | Grouping variable(s) of the denominator analysis |
| `isdatadrivenhere` | `AG_max_dataDriven` | is dataDriven | TRUE/FALSE: highest grouping is data-driven |
| `byvarshere` | `by_listc` | by vars (list) | Grouping variables as a quoted, comma-separated list |

**Template**

```r
denom_dataset = df2_denomanaidhere |>
  dplyr::select(denom_anagroupvarshere)

in_data = df2_analysisidhere |>
    dplyr::distinct(distinctlisthere) |>
    dplyr::mutate(dummy = 'dummyvar')

dataDriven = isdatadrivenhere
if(dataDriven == TRUE){
df3_analysisidhere <-
  cards::ard_tabulate(
    data = in_data,
    strata = c(byvarshere),
    variables = 'dummy',
    denominator = denom_dataset
  ) } else {
df3_analysisidhere <-
 cards::ard_tabulate(
    data = in_data,
    by = c(byvarshere),
    variables = 'dummy',
    denominator = denom_dataset
  ) }
df3_analysisidhere <- df3_analysisidhere|>
  dplyr::filter(stat_name %in% c('n', 'p')) |>
  dplyr::mutate(operationid = dplyr::case_when(stat_name == 'n' ~ 'opid1here',
                                               stat_name == 'p' ~ 'opid2here'))
```

---

## `continuous_summary` - Summary of a continuous variable

Descriptive statistics of a continuous analysis variable per group. Verified against Common_Safety_Displays Mth02_ContVar_Summ_ByGrp; current cards API (ard_summary, by_listc). Fixes the legacy sheet which used the deprecated ard_continuous + by_stmt (the latter stamps no group metadata).

**Status:** verified &nbsp; **Verified against:** Common_Safety_Displays_cards.xlsx Mth02_ContVar_Summ_ByGrp

**Operations**

| order | stat_name | label | resultPattern |
|-------|-----------|-------|---------------|
| 1 | `N` | n | `xx` |
| 2 | `mean` | Mean | `xx.x` |
| 3 | `sd` | SD | `(xx.xx)` |
| 4 | `median` | Median | `xx.x` |
| 5 | `p25` | Q1 | `xx.x` |
| 6 | `p75` | Q3 | `xx.x` |
| 7 | `min` | Min | `xx` |
| 8 | `max` | Max | `xx` |

**Parameters**

| token | valueSource | label | description |
|-------|-------------|-------|-------------|
| `byvarshere` | `by_listc` | by vars (list) | Grouping variables as a quoted, comma-separated list |
| `anavarhere` | `ana_var` | ana var | Continuous analysis variable to summarise |

**Template**

```r
df3_analysisidhere <-
  cards::ard_summary(
    data = df2_analysisidhere,
    by = c(byvarshere),
    variables = anavarhere
  ) |>
  dplyr::mutate(operationid = dplyr::case_when(stat_name == 'N'      ~ 'opid1here',
                                               stat_name == 'mean'   ~ 'opid2here',
                                               stat_name == 'sd'     ~ 'opid3here',
                                               stat_name == 'median' ~ 'opid4here',
                                               stat_name == 'p25'    ~ 'opid5here',
                                               stat_name == 'p75'    ~ 'opid6here',
                                               stat_name == 'min'    ~ 'opid7here',
                                               stat_name == 'max'    ~ 'opid8here'))
```

---

## `risk_difference` - Risk difference of two arms (overall)

One overall risk difference (%) and 95% CI between the two treatment arms present in the data subset. The two arms are self-derived from the data (no arm-filter construct). References df_poptot so a 0-event subset still yields RD = 0 / CI = [0,0] (population_based; matches the fda-ae-t06 An_34 reference). Uses base stats::prop.test rather than cardx because cardx::ard_stats_prop_test is unsafe on a fully degenerate (0-event in both arms) 2x2 table. Verified against fda-ae-t06 Mth_03_1.

**Status:** verified &nbsp; **Verified against:** tests/testthat/testdata/etfl/metadata/fda-ae-t06-siera.json Mth_03_1

**Operations**

| order | stat_name | label | resultPattern |
|-------|-----------|-------|---------------|
| 1 | `Risk_Difference_%` | Risk Difference % | `xx.x` |
| 2 | `95%_CI_Low` | 95% CI Low | `xx.x` |
| 3 | `95%_CI_High` | 95% CI High | `xx.x` |

**Parameters**

| token | valueSource | label | description |
|-------|-------------|-------|-------------|
| `groupvar1here` | `AG_var1` | grp var 1 | Grouping variable (treatment arm) |
| `anavarhere` | `ana_var` | ana var | Analysis variable (subject ID) |

**Template**

```r
.arms_analysisidhere <- unique(df2_analysisidhere$groupvar1here)
full_analysisidhere <- if (length(.arms_analysisidhere) >= 2) {
  df_poptot |> dplyr::filter(groupvar1here %in% .arms_analysisidhere)
} else {
  df_poptot
}

success_analysisidhere = df2_analysisidhere |>
    dplyr::distinct(anavarhere) |>
    dplyr::mutate(FL = 1L)

ana_analysisidhere = full_analysisidhere |>
    dplyr::left_join(
        success_analysisidhere |> dplyr::select(anavarhere, FL),
        by = "anavarhere"
    ) |>
    dplyr::mutate(FL = dplyr::if_else(is.na(FL), 0L, FL))

grp_analysisidhere <- sort(unique(ana_analysisidhere$groupvar1here))

if (length(grp_analysisidhere) >= 2) {
n1_analysisidhere <- sum(ana_analysisidhere$groupvar1here == grp_analysisidhere[[1]])
n2_analysisidhere <- sum(ana_analysisidhere$groupvar1here == grp_analysisidhere[[2]])
x1_analysisidhere <- sum(ana_analysisidhere$FL[ana_analysisidhere$groupvar1here == grp_analysisidhere[[1]]])
x2_analysisidhere <- sum(ana_analysisidhere$FL[ana_analysisidhere$groupvar1here == grp_analysisidhere[[2]]])
pt_analysisidhere <- stats::prop.test(
    x = c(x1_analysisidhere, x2_analysisidhere),
    n = c(n1_analysisidhere, n2_analysisidhere),
    correct = FALSE
)

df3_analysisidhere <- tibble::tibble(
    variable = "FL",
    variable_level = list(NULL, NULL, NULL),
    stat_name = c("estimate", "conf.low", "conf.high"),
    stat = list(
        (pt_analysisidhere$estimate[[1]] - pt_analysisidhere$estimate[[2]]) * 100,
        pt_analysisidhere$conf.int[[1]] * 100,
        pt_analysisidhere$conf.int[[2]] * 100
    ),
    operationid = c('opid1here', 'opid2here', 'opid3here')
)
} else {
df3_analysisidhere <- tibble::tibble(
    variable = "FL",
    variable_level = list(NULL, NULL, NULL),
    stat_name = c("estimate", "conf.low", "conf.high"),
    stat = list(NA_real_, NA_real_, NA_real_),
    operationid = c('opid1here', 'opid2here', 'opid3here')
)
}
```

---

## `risk_difference_per_group` - Risk difference per data-driven inner category

One risk difference (%) + 95% CI per data-driven inner category (e.g. PT or SOC) of Group2, comparing the two arms present in the subset. The two arms are self-derived from the data. References df_poptot (population_based). Verified against fda-ae-t13/t36 Mth_03_1a using an independent stats::prop.test(correct = FALSE) recomputation (180/187 PTs, 22 SOCs).

**Status:** verified &nbsp; **Verified against:** tests/testthat/testdata/etfl/metadata/fda-ae-t13-siera.json Mth_03_1a

**Operations**

| order | stat_name | label | resultPattern |
|-------|-----------|-------|---------------|
| 1 | `Risk_Difference_%` | Risk Difference % | `xx.x` |
| 2 | `95%_CI_Low` | 95% CI Low | `xx.x` |
| 3 | `95%_CI_High` | 95% CI High | `xx.x` |

**Parameters**

| token | valueSource | label | description |
|-------|-------------|-------|-------------|
| `groupvar1here` | `AG_var1` | grp var 1 | Grouping variable from Group1 (treatment arm) |
| `groupvar2here` | `AG_var2` | grp var 2 | Grouping variable from Group2 (data-driven inner category) |
| `anavarhere` | `ana_var` | ana var | Analysis variable (subject ID) |

**Template**

```r
.arms_analysisidhere <- utils::head(sort(unique(df2_analysisidhere$groupvar1here)), 2)
full_analysisidhere <- df_poptot |> dplyr::filter(groupvar1here %in% .arms_analysisidhere)

.cats_analysisidhere <- sort(unique(as.character(df2_analysisidhere$groupvar2here)))

.rd_one_analysisidhere <- function(.cat) {
  success <- df2_analysisidhere |>
      dplyr::filter(as.character(groupvar2here) == .cat) |>
      dplyr::distinct(anavarhere) |>
      dplyr::mutate(FL = 1L)
  ana <- full_analysisidhere |>
      dplyr::left_join(dplyr::select(success, anavarhere, FL), by = "anavarhere") |>
      dplyr::mutate(FL = dplyr::if_else(is.na(FL), 0L, FL))
  cardx::ard_stats_prop_test(data = ana, by = groupvar1here, variables = FL, correct = FALSE) |>
      dplyr::filter(stat_name %in% c("estimate", "conf.low", "conf.high")) |>
      dplyr::mutate(
          stat = lapply(stat, function(.v) .v * 100),
          group2_level = .cat,
          operationid = dplyr::case_when(
              stat_name == "estimate"  ~ "opid1here",
              stat_name == "conf.low"  ~ "opid2here",
              stat_name == "conf.high" ~ "opid3here"
          )
      )
}

df3_analysisidhere <- if (length(.arms_analysisidhere) >= 2 && length(.cats_analysisidhere) > 0) {
  dplyr::bind_rows(lapply(.cats_analysisidhere, .rd_one_analysisidhere))
} else {
  tibble::tibble(variable = character(0), group2_level = character(0),
                 stat_name = character(0), stat = list(),
                 operationid = character(0))
}
```

---

## `fishers_exact` - Fisher's exact test of two arms

Fisher's exact odds-ratio estimate and 95% CI between the two arms present in the subset. CORRECTED from the legacy sheet: arms are self-derived from the data (the legacy sheet used the unsupported trt_filter_stm valueSource), and the conf.high operation mapping is fixed (the legacy case_when mapped conf.low twice). References df_poptot (population_based). NOT YET validated against a reference ARD - no Fisher table exists in the bundled metadata; structure only is checked by the contract test.

**Status:** corrected-unvalidated &nbsp; **Verified against:** -

**Operations**

| order | stat_name | label | resultPattern |
|-------|-----------|-------|---------------|
| 1 | `estimate` | Odds ratio | `xx.xx` |
| 2 | `conf.low` | 95% CI Low | `xx.xx` |
| 3 | `conf.high` | 95% CI High | `xx.xx` |

**Parameters**

| token | valueSource | label | description |
|-------|-------------|-------|-------------|
| `groupvar1here` | `AG_var1` | grp var 1 | Grouping variable (treatment arm) |
| `anavarhere` | `ana_var` | ana var | Analysis variable (subject ID) |

**Template**

```r
.arms_analysisidhere <- utils::head(sort(unique(df2_analysisidhere$groupvar1here)), 2)
full_analysisidhere <- df_poptot |> dplyr::filter(groupvar1here %in% .arms_analysisidhere)

success_analysisidhere = df2_analysisidhere |>
    dplyr::distinct(anavarhere) |>
    dplyr::mutate(FL = 1L)

ana_analysisidhere = full_analysisidhere |>
    dplyr::left_join(dplyr::select(success_analysisidhere, anavarhere, FL), by = "anavarhere") |>
    dplyr::mutate(FL = dplyr::if_else(is.na(FL), 0L, FL))

df3_analysisidhere = if (length(.arms_analysisidhere) >= 2) {
  cardx::ard_stats_fisher_test(data = ana_analysisidhere, by = groupvar1here, variables = FL) |>
   dplyr::filter(stat_name %in% c('estimate', 'conf.low', 'conf.high')) |>
   dplyr::mutate(operationid = dplyr::case_when(stat_name == 'estimate'  ~ 'opid1here',
                                                stat_name == 'conf.low'  ~ 'opid2here',
                                                stat_name == 'conf.high' ~ 'opid3here'))
} else {
  tibble::tibble(variable = character(0), stat_name = character(0),
                 stat = list(), operationid = character(0))
}
```

---

## `chisq` - Chi-square test of a categorical variable across arms

Chi-square test p-value of a categorical variable across treatment arms. Verified against Common_Safety_Displays Mth03_CatVar_Comp_PChiSq.

**Status:** verified &nbsp; **Verified against:** Common_Safety_Displays_cards.xlsx Mth03_CatVar_Comp_PChiSq

**Operations**

| order | stat_name | label | resultPattern |
|-------|-----------|-------|---------------|
| 1 | `p.value` | p-value | `x.xxx` |

**Parameters**

| token | valueSource | label | description |
|-------|-------------|-------|-------------|
| `groupvar1here` | `AG_var1` | grp var 1 | Grouping variable (treatment arm), the by-variable |
| `groupvar2here` | `AG_var2` | grp var 2 | Categorical variable being tested |

**Template**

```r
df3_analysisidhere <-
    cardx::ard_stats_chisq_test(by = groupvar1here, data = df2_analysisidhere, variables = groupvar2here) |>
  dplyr::filter(stat_name == 'p.value') |>
  dplyr::mutate(operationid = 'opid1here')
```

---

## `anova` - One-way ANOVA of a continuous variable across arms

One-way ANOVA p-value of a continuous variable across treatment arms. Verified against Common_Safety_Displays Mth04_ContVar_Comp_Anova.

**Status:** verified &nbsp; **Verified against:** Common_Safety_Displays_cards.xlsx Mth04_ContVar_Comp_Anova

**Operations**

| order | stat_name | label | resultPattern |
|-------|-----------|-------|---------------|
| 1 | `p.value` | p-value | `x.xxx` |

**Parameters**

| token | valueSource | label | description |
|-------|-------------|-------|-------------|
| `anavarhere` | `ana_var` | ana var | Continuous analysis variable (LHS of the formula) |
| `groupvar1here` | `AG_var1` | grp var 1 | Grouping variable (treatment arm), the RHS of the formula |

**Template**

```r
df3_analysisidhere <-
    cardx::ard_stats_aov(anavarhere ~ groupvar1here, data = df2_analysisidhere) |>
  dplyr::filter(stat_name == 'p.value') |>
  dplyr::mutate(operationid = 'opid1here')
```
