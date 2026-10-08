# ARD program structure

``` r

library(siera)
```

## My ARD program has been auto-generated: What can I expect?

Each auto-generated ARD program (one generated for each output) follows
a logical structure linked to the ARS model. Each script contains code
for all the analyses related to the output, and follows the same code
pattern for each analysis (except the first analysis, which handles the
“big N” calculation by convention). An analysis-level ARD is generated
for each analysis, and at the end of the program, all these
analysis-level ARDs are appended to create one output-level ARD. Keep in
mind that each of these code sections are auto-populated with ARS
metadata. This can be visualized as follows:

``` r

# Section 1: Program header

# Section 2: Load libraries

# Section 3: Load ADaM datasets

# Section 4a (first Analysis): Code to calculate results as an ARD

# Section 4b (subsequent Analyses): Code to calculate results as an ARD

# Section 5: Append Analysis-level ARDs

# Section 6: Format results per ARS resultPattern
```

Note that the generated scripts link each analysis to the ARS metadata
with
[`siera::ars_stamp()`](https://clymbclinical.github.io/siera/reference/ars_stamp.md)
(see Step 4 below), so the *siera* package needs to be installed
wherever the scripts are run. If you would rather have self-contained
scripts that only use `dplyr` and `cards`/`cardx` for this, use
`readARS(code_style = "expanded")`.

Section 3 (“Load ADaM datasets”) reads each ADaM dataset referenced by
the output. siera supports three on-disk formats and chooses the reader
from each file’s extension: CSV (`.csv`) files are read with
[`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html),
SAS transport (`.xpt`) files with
[`haven::read_xpt()`](https://haven.tidyverse.org/reference/read_xpt.html),
and CDISC Dataset-JSON (`.json`) files with
[`datasetjson::read_dataset_json()`](https://atorus-research.github.io/datasetjson/reference/read_dataset_json.html).
No extra argument is needed - just point
[`readARS()`](https://clymbclinical.github.io/siera/reference/readARS.md)
at a folder of `.csv`, `.xpt` or `.json` ADaMs. Reading `.xpt` datasets
requires the `haven` package, and `.json` datasets the `datasetjson`
package, to be installed. The file lookup is case-insensitive, so the
lower-case file names typical of regulatory submissions
(e.g. `adsl.xpt`) are matched to the upper-case dataset names in the ARS
metadata (`ADSL`). When several formats exist for the same dataset,
precedence is `.csv`, then `.xpt`, then `.json`.

### Analysis-level code to calculate ARDs

Each analysis related to the output follows a logical structure based on
the ARS model to create an analysis-level ARD. This structure is as
follows:

#### Step 1: Apply “Analysis Set” to ADaM(s)

This step applies each analysis’s Analysis Set (e.g. Safety Population)
to the ADaM dataset(s). In the case where the “big N” count is based on
another dataset (like ADSL) than the main ADaM (e.g. ADAE), two separate
datasets are created for downstream use in subsequent analyses. Example:

``` r

overlap <- intersect(names(ADSL), names(ADAE))
overlapfin <- setdiff(overlap, "USUBJID")

df_pop <- dplyr::filter(
  ADSL,
  SAFFL == "Y"
) |>
  merge(ADAE |> dplyr::select(-dplyr::all_of(overlapfin)),
    by = "USUBJID",
    all = FALSE
  )

df_poptot <- dplyr::filter(
  ADSL,
  SAFFL == "Y"
)
```

`df_poptot` holds one row per subject in the population, and `df_pop`
holds the population’s records in the content dataset (here ADAE). The
population code is written once, above the first analysis, and every
analysis then starts from whichever of the two it needs. *siera* decides
this from the metadata, not from where the analysis sits in the output:

- An analysis whose groupings and data subset all use the analysis set’s
  dataset (ADSL) reads `df_poptot`. This is how the “big N” analysis
  counts every subject in the population, including subjects with no
  records in ADAE.
- An analysis that uses another dataset - a data subset on ADAE
  (e.g. treatment-emergent events), a grouping on an ADAE variable
  (e.g. system organ class) or an analysis variable from that dataset -
  reads `df_pop`.

When the analyses of one output use different Analysis Sets - say, a
disposition table with a row each for the randomized, ITT, safety and
per-protocol populations - a population is built for each set and every
analysis reads its own. The data frames are then named after the set,
e.g. `df_poptot__AnalysisSet_07`. Similarly, if one Analysis Set is
merged onto more than one content dataset, each merged data frame is
named after its dataset, e.g. `df_pop__ADLB`.

#### Step 2: Apply “Data Subset”

Based on the resulting dataset from step 1, further data subsetting is
applied which is relevant to the current analysis (e.g. filtering for
serious, treatment-related Adverse Events). If no data subsetting is
required for the analysis, a simple assignment of the previous dataset
is done with no ‘filter’ statement. This step has a convention of
starting the dataframe name with “df2”, followed by the AnalysisId.

``` r

df2_An07_03_SerTEAE_Summ_ByTrt <- df_pop |>
  dplyr::filter(TRTEMFL == "Y" & AESER == "Y")
```

#### Step 3: Apply “Method”

This step takes the subsetted dataset, and applies the required
AnalysisMethod (e.g. counting the subjects with a serious TEAE in each
treatment arm). As explained in the vignette for [using `cards` and
`cardx`](https://clymbclinical.github.io/siera/articles/using-cards.md),
functions from these packages are applied to handle the statistical
operations for the analysis. Typically, there would be some pre-work
done on the dataset before passing it to a `cards` or `cardx` function.
When the function is applied, the result is an analysis-level ARD. See
example below:

``` r

# the method only runs when there is data; otherwise df3 stays NULL and Step 4
# turns it into a stub row that still identifies the analysis
df3_An07_03_SerTEAE_Summ_ByTrt <- NULL
if (nrow(df2_An07_03_SerTEAE_Summ_ByTrt) != 0) {
  # intermediate step: Prepare Denominator Dataset for `cards` function
  denom_dataset <- df2_An01_05_SAF_Summ_ByTrt |>
    dplyr::select(TRT01A)

  # intermediate step: Prepare input dataset for `cards` function
  # (one row per subject and arm, however many serious TEAEs they had)
  in_data <- df2_An07_03_SerTEAE_Summ_ByTrt |>
    dplyr::distinct(TRT01A, USUBJID) |>
    dplyr::mutate(dummy = "dummyvar")

  # calculate subject counts and % (based on big N) grouped by treatment
  df3_An07_03_SerTEAE_Summ_ByTrt <- cards::ard_tabulate(
    data = in_data,
    by = c("TRT01A"),
    variables = "dummy",
    denominator = denom_dataset
  ) |>
    # select relevant statistics as defined by the Method, and assign operation Ids
    dplyr::filter(stat_name %in% c("n", "p")) |>
    dplyr::mutate(
      # the rows describe the ARS analysis variable, not the helper column
      variable = "USUBJID",
      variable_level = list(NULL),
      operationid = dplyr::case_when(
        stat_name == "n" ~ "Mth01_CatVar_Summ_ByGrp_1_n",
        stat_name == "p" ~ "Mth01_CatVar_Summ_ByGrp_2_pct"
      )
    )
}
```

The `dummy` column is only a counting device: every subject contributes
one row, so tabulating a constant counts the subjects in each arm. The
result rows are then labelled with the analysis variable from the ARS
(`USUBJID`), so the ARD says what was counted.

Not every method follows this shape. Methods that must report something
even when no subject qualifies - a zero count for every **pre-defined**
group, or a risk difference of `0` - compute over the whole population
and skip the empty-data guard. The demographics analyses in the same
example (age group, sex, ethnicity and race) use such a method from
siera’s method library, `categorical_summary_per_predefined_group`, so
all nine race groups that the ARS defines are reported, with zero counts
for those that do not occur in the data. See the [Concepts and
conventions](https://clymbclinical.github.io/siera/articles/concepts.md)
vignette for when to choose which.

This example uses the current `cards` function name `ard_tabulate()`
(formerly `ard_categorical()`); see the [using `cards` and
`cardx`](https://clymbclinical.github.io/siera/articles/using-cards.md)
vignette for the full list of renames.

#### Step 4: Link the ARD to the ARS metadata

The last step of every analysis is a single call to
[`ars_stamp()`](https://clymbclinical.github.io/siera/reference/ars_stamp.md).
It takes the analysis-level ARD from Step 3 and adds the **CDISC ARD
traceability columns**, so that every result can be traced back to its
ARS definition:

``` r

# Link ARS identifiers ---
df3_An07_03_SerTEAE_Summ_ByTrt <- siera::ars_stamp(
  df3_An07_03_SerTEAE_Summ_ByTrt,
  analysis_id = 'An07_03_SerTEAE_Summ_ByTrt', # analyses[].id
  method_id   = 'Mth01_CatVar_Summ_ByGrp',    # analyses[].methodId
  output_id   = 'Out14-3-1-1',                # mainListOfContents outputId
  groupings   = list(                         # analyses[].orderedGroupings
    siera::ars_grouping('AnlsGrouping_01_Trt', groups = c(
      'AnlsGrouping_01_Trt_1' = 'Placebo',
      'AnlsGrouping_01_Trt_2' = 'Xanomeline Low Dose',
      'AnlsGrouping_01_Trt_3' = 'Xanomeline High Dose'))
  )
)
```

An analysis with more than one grouping gets one
[`ars_grouping()`](https://clymbclinical.github.io/siera/reference/ars_grouping.md)
per grouping, in the order of `analyses[].orderedGroupings`. The race
analysis, for example, adds its nine pre-defined race groups after the
treatment arms (shortened here):

``` r

  groupings   = list(
    siera::ars_grouping('AnlsGrouping_01_Trt', groups = c(...)),
    siera::ars_grouping('AnlsGrouping_04_Race', groups = c(
      'AnlsGrouping_04_Race_1' = 'American Indian or Alaska Native',
      'AnlsGrouping_04_Race_2' = 'Asian',
      # ...
      'AnlsGrouping_04_Race_9' = 'Other'))
  )
```

Each group is matched by the value in its `group[n]_level` column. For
the treatment arms that is the data value (`Placebo`). The race rows
come from a method that counts per pre-defined group condition, and it
labels each row with the group’s ARS name
(`American Indian or Alaska Native`, not the ADaM value
`AMERICAN INDIAN OR ALASKA NATIVE`), so those names are what the call
matches. For the same reason, an age group defined as
`AGEGR1 IN ("65-80", ">80")` appears as one row, `≥ 65 years`.

A data-driven grouping, such as the system organ class of an adverse
event, is written
`siera::ars_grouping('<groupingId>', data_driven = TRUE)` instead.

Every argument is copied straight from the ARS metadata, so the call
reads as a small, checkable summary of the analysis:

| Argument | ARS JSON path | ARD column(s) |
|----|----|----|
| `analysis_id` | `analyses[].id` | `AnalysisId` |
| `method_id` | `analyses[].methodId` | `MethodId` |
| `output_id` | `mainListOfContents` list item `outputId` | `OutputId` |
| `groupings` (position `n` in the list) | `analyses[].orderedGroupings[]` | `group[n]_groupingId` plus `group[n]_groupId` or `group[n]_groupValue` |
| `ars_grouping(id, ...)` | `analysisGroupings[].id` | `group[n]_groupingId` |
| `ars_grouping(groups = )` (names = group ids) | `analysisGroupings[].groups[].id` and `.condition.value` | `group[n]_groupId` |
| `ars_grouping(data_driven = )` | `analysisGroupings[].dataDriven` | `group[n]_groupValue` |

For each grouping you will get a `group[n]_groupingId` (which grouping
the column belongs to) plus one of:

- `group[n]_groupId` - for **pre-defined groups** (groups listed in the
  metadata, like treatment arms), looked up from `group[n]_level`; or
- `group[n]_groupValue` - for **data-driven groupings**
  (`dataDriven: true`, where categories such as cause of death are
  discovered from the ADaM data at run time), capturing the discovered
  value directly.

[`ars_stamp()`](https://clymbclinical.github.io/siera/reference/ars_stamp.md)
also coerces every `*_level` column to character so the analysis-level
ARDs can be appended safely, and, when Step 3 was skipped because the
data subset was empty (`df3` is `NULL`), returns a stub row that holds
only `AnalysisId`, `MethodId` and `OutputId`.

See the [Concepts and
conventions](https://clymbclinical.github.io/siera/articles/concepts.md)
vignette for the full picture of these traceability columns.

##### Prefer to see everything spelled out?

[`ars_stamp()`](https://clymbclinical.github.io/siera/reference/ars_stamp.md)
is a convenience: it produces exactly the same ARD as the plain `dplyr`
code that siera used to write into each script (an
`AnalysisId`/`MethodId`/`OutputId` `mutate()`, a `case_when()` per
grouping and a [`vapply()`](https://rdrr.io/r/base/lapply.html) to
coerce `*_level` columns). If you would rather have your scripts not
call any siera function for this step, or you want to review that plain
code, generate the scripts with `code_style = "expanded"`:

``` r

readARS(ARS_path, output_path, adam_path, code_style = "expanded")
```

The default is `code_style = "wrapped"`. Both styles are tested to
produce identical ARDs.

### Final steps

The above process repeats for each Analysis, although the code for each
step would of course vary (as defined in the specific ARS metadata for
each Analysis). Once each Analysis ARD has been created, these ARDs are
all appened to create output-level ARD. See example below:

``` r

# combine analyses to create ARD ----
ARD <- dplyr::bind_rows(
  df3_An01_05_SAF_Summ_ByTrt,
  df3_An03_01_Age_Summ_ByTrt,
  df3_An03_01_Age_Comp_ByTrt,
  df3_An03_02_AgeGrp_Summ_ByTrt,
  df3_An03_02_AgeGrp_Comp_ByTrt,
  df3_An03_03_Sex_Summ_ByTrt,
  df3_An03_03_Sex_Comp_ByTrt,
  df3_An03_04_Ethnic_Summ_ByTrt,
  df3_An03_04_Ethnic_Comp_ByTrt,
  df3_An03_05_Race_Summ_ByTrt,
  df3_An03_05_Race_Comp_ByTrt,
  df3_An03_06_Height_Summ_ByTrt,
  df3_An03_06_Height_Comp_ByTrt
)
```

### Result formatting: `res`, `pattern` and `disp`

After the analysis-level ARDs are combined, a final block formats each
result for display. The ARS metadata attaches a `resultPattern` to every
method operation (e.g. `(N=XX)`, `XX.X`, `(XX.XX)`), describing how the
raw statistic should look in the final table. The generated script
applies these patterns and adds three columns to the ARD:

- `res` - the raw statistic as a plain numeric value (flattened from the
  `stat` list-column produced by `cards`);
- `pattern` - the ARS `resultPattern` for the row’s operation;
- `disp` - the printable, table-ready formatted string (e.g. `(N=86)`,
  `75.2`, `(8.59)`), honouring the pattern’s decimal count and any
  prefix/suffix such as parentheses.

Rounding follows the clinical-reporting norm - **round half away from
zero**, the same rule SAS’s `ROUND()` uses - rather than R’s default
round-half-to-even. So a raw `1.369` with pattern `X.X` becomes `1.4`
(not `1.3`), and an exact tie such as `2.5` with pattern `XX` becomes
`3` (base R’s [`round()`](https://rdrr.io/r/base/Round.html) would give
`2`). This keeps `disp` values aligned with SAS-generated reference
tables. The raw `res` value is left untouched, so any downstream
re-rounding is still your call.

One more convention to be aware of: `cards` reports proportions on the
0-1 scale (under `stat_name == "p"`), while result patterns express
percentages - so those rows are multiplied by 100 before formatting (a
raw `0.756` with pattern `( XX.X)` becomes `( 75.6)`). p-values
(`stat_name == "p.value"`) are left as-is.

The `cards`-internal `fmt_fun`/`fmt_fn` columns (which hold R formatting
*functions*, not printable values) are dropped from the final ARD.

### Deeper tables and more groupings

The same pattern scales without special handling on your part:

- **Arbitrarily deep table hierarchies** - the table stub (defined in
  *mainListOfContents*) can nest as deeply as your output requires;
  there is no longer a three-level limit.
- **More than three grouping factors** - an analysis can be split by
  four or more groupings at once (e.g. Treatment x Age group x Sex x
  Region). Each grouping simply adds its own `group[n]_*` set of columns
  to the ARD.

### Example

Examples of such an ARD script has been shipped with this package. Below
are such examples, for

- Summary of Demographics: ARD_Out14-1-1.R
- Overall Summary of Treatment-Emergent Adverse Events:
  ARD_Out14-3-1-1.R

Access these with the below functions:

``` r

# see location of script:
ARD_script_example("ARD_Out14-1-1.R")
ARD_script_example("ARD_Out14-3-1-1.R")
```

``` r

# open script to inspect:
file.edit(ARD_script_example("ARD_Out14-1-1.R"))
file.edit(ARD_script_example("ARD_Out14-3-1-1.R"))
```

``` r

# run script locally:
source(ARD_script_example("ARD_Out14-1-1.R"))
source(ARD_script_example("ARD_Out14-3-1-1.R"))
```

This ARD can be used in various ways downstream. Read more about this in
the vignette on [utilising
ARDs](https://clymbclinical.github.io/siera/articles/apply-ARD.md).
