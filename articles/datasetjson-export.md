# Exporting ARDs as Dataset-JSON

``` r

library(siera)
```

The ARD that a *siera* script produces is an R data frame. That is
perfect for working in R, but sooner or later you will want to hand your
results to someone (or something) else: a QC programmer working in SAS,
a results repository, a reviewer. This vignette shows how to have
*siera* write each ARD to a **CDISC Dataset-JSON** file as well, in a
form that is ready to exchange.

### What is Dataset-JSON, and why should you care?

[Dataset-JSON](https://www.cdisc.org/standards/data-exchange/dataset-json)
is CDISC’s JSON-based format for exchanging tabular clinical data. A
Dataset-JSON file carries the data rows **together with** the metadata
that describes them: each column’s name, label and data type, plus a
name and label for the dataset itself.

It matters because it is the industry’s leading candidate to replace SAS
version 5 transport (XPT) files. The FDA, together with CDISC and PHUSE,
has piloted Dataset-JSON for regulatory submissions. Compared with XPT
it has no 8-character variable name or 200-character value limits, it is
an open, text-based format, and it is easy to read from R, SAS, Python
or any other language.

*siera* writes **Dataset-JSON v1.1** files.

### Turning it on

Dataset-JSON export is controlled by one argument to
[`readARS()`](https://clymbclinical.github.io/siera/reference/readARS.md):
`output_format`. It defaults to `"none"` (generate the ARD scripts
only). Set it to `"datasetjson"` and every generated `ARD_<OutputId>.R`
script gets an extra section at the end that writes the ARD to a
Dataset-JSON file when you run it.

``` r

# ARS metadata and the folder of ADaM datasets shipped with siera
ARS_path  <- ARS_example("exampleARS_4.json")
adam_path <- dirname(ARS_example("ADSL.csv"))

# a fresh folder for the generated script (and, later, the Dataset-JSON file)
output_path <- file.path(tempdir(), "siera-datasetjson")
dir.create(output_path, showWarnings = FALSE)

# generate the ARD script for output Out_01, with Dataset-JSON export switched on
readARS(
  ARS_path,
  output_path,
  adam_path,
  spec_output   = "Out_01",
  output_format = "datasetjson"
)

list.files(output_path)
#> [1] "ARD_Out_01.R"
```

The value must be spelled out in full: `output_format = "dataset"` is an
error rather than a guess.

#### What the export section looks like

The export runs when **you run the generated script**, not when
[`readARS()`](https://clymbclinical.github.io/siera/reference/readARS.md)
runs. That is because *siera* only writes the code: the ARD does not
exist until the script has been run against your ADaM data, and its
columns depend on what the data contains. So the export section looks at
the finished `ARD` object and works out the column metadata from it.
Here is how it starts:

``` r

script <- readLines(file.path(output_path, "ARD_Out_01.R"))
start  <- grep("Export ARD as CDISC Dataset-JSON", script)
cat(script[start:(start + 6)], sep = "\n")
#> # Export ARD as CDISC Dataset-JSON ----
#> if (requireNamespace("datasetjson", quietly = TRUE)) {
#>   .ds_df <- ARD
#> 
#>   # Dataset-JSON columns must be atomic; flatten {cards} list-columns.
#>   for (.nm in names(.ds_df)) {
#>     if (is.list(.ds_df[[.nm]])) {
```

### You need the `datasetjson` package where the script runs

The file is written with the
[`datasetjson`](https://cran.r-project.org/package=datasetjson) package.
*siera* does not require it, so it must be installed in the R session
that **runs** the generated script (which may be a different machine
from the one that ran
[`readARS()`](https://clymbclinical.github.io/siera/reference/readARS.md)):

``` r

install.packages("datasetjson")
```

If `datasetjson` is not installed, the script still produces the `ARD`
object as usual. It simply prints a message that the Dataset-JSON export
was skipped, instead of stopping with an error.

### Running the script and finding the file

Run the generated script as you normally would. The Dataset-JSON file is
written to the folder you gave
[`readARS()`](https://clymbclinical.github.io/siera/reference/readARS.md)
as `output_path`, beside the script, and is named after the output:
`ARD_<OutputId>.json`.

``` r

source(file.path(output_path, "ARD_Out_01.R"))

list.files(output_path)
#> [1] "ARD_Out_01.json" "ARD_Out_01.R"
```

### Reading it back

Use
[`datasetjson::read_dataset_json()`](https://atorus-research.github.io/datasetjson/reference/read_dataset_json.html)
to read the file back into R. You get the same rows and columns as the
`ARD` object:

``` r

ard_json <- datasetjson::read_dataset_json(
  file.path(output_path, "ARD_Out_01.json")
)

dim(ard_json)
#> [1] 252  19
dim(ARD)
#> [1] 252  19

head(ard_json[, c("AnalysisId", "operationid", "group1_level", "res", "disp")])
#>   AnalysisId operationid group1_level         res    disp
#> 1      An_01 Mth_01_01_n            1  84.0000000  (N=84)
#> 2      An_01 Mth_01_01_n            2  84.0000000  (N=84)
#> 3      An_01 Mth_01_01_n            3  86.0000000  (N=86)
#> 4      An_02 Mth_01_01_n         <NA> 254.0000000 (N=254)
#> 5      An_03 Mth_02_01_n            1  50.0000000      50
#> 6      An_03 Mth_02_02_%            1   0.5952381  (59.5)
```

The column metadata travels with the data. Read the file with `jsonlite`
and look at its `columns` section to see each column’s name, label and
data type:

``` r

meta <- jsonlite::fromJSON(file.path(output_path, "ARD_Out_01.json"))$columns
head(meta[, c("name", "label", "dataType")], 10)
#>              name                   label dataType
#> 1          group1        Group 1 Variable   string
#> 2    group1_level           Group 1 Value   string
#> 3        variable       Analysis Variable   string
#> 4  variable_level Analysis Variable Level   string
#> 5         context       Statistic Context   string
#> 6       stat_name          Statistic Name   string
#> 7      stat_label         Statistic Label   string
#> 8            stat         Statistic Value    float
#> 9         warning                 Warning   string
#> 10          error                   Error   string
```

### How columns are prepared

Two things happen to the ARD on its way into Dataset-JSON. Neither
changes the `ARD` object in your R session; only the exported copy is
affected.

**List-columns are flattened.** Every Dataset-JSON column must hold a
single value per row. A few columns produced by `cards` are
*list-columns*, where each cell can hold an R object. These are
converted:

- `stat` (the statistic) becomes a numeric column, written with data
  type `float`.
- `warning` and `error` become text columns; when a cell holds more than
  one message, they are joined with `"; "`.

**Variable labels are filled in.** Each column needs a label. *siera*
chooses one as follows:

1.  A label you supplied yourself through `column_labels` (see below).
2.  A built-in label for *siera*’s and `cards`’ standard columns,
    e.g. `AnalysisId` is “Analysis Identifier”, `stat` is “Statistic
    Value” and `disp` is “Formatted Result”.
3.  A generated label for the grouping columns, e.g. `group1_groupingId`
    is “Group 1 Grouping Identifier” and `group2_level` is “Group 2
    Value”.
4.  Otherwise, the column name itself. This is what happens to columns
    named after ADaM variables, which *siera* cannot know in advance.

### Using your own labels

Your submission conventions may call for different wording, or you may
want proper labels for ADaM-named columns. Pass a named character vector
to `column_labels`: the names are ARD column names and the values are
the labels you want. Your labels take priority over the built-in ones,
so you can reword those too.

``` r

readARS(
  ARS_path,
  output_path,
  adam_path,
  spec_output   = "Out_01",
  output_format = "datasetjson",
  column_labels = c(
    group1_level = "Actual Treatment",
    AnalysisId   = "Analysis ID"
  )
)
```

``` r

source(file.path(output_path, "ARD_Out_01.R"))

meta <- jsonlite::fromJSON(file.path(output_path, "ARD_Out_01.json"))$columns
meta[meta$name %in% c("group1_level", "AnalysisId", "group1_groupingId"),
     c("name", "label")]
#>                 name                       label
#> 2       group1_level            Actual Treatment
#> 12        AnalysisId                 Analysis ID
#> 15 group1_groupingId Group 1 Grouping Identifier
```

A few things to know about `column_labels`:

- [`readARS()`](https://clymbclinical.github.io/siera/reference/readARS.md)
  checks the vector straight away. It must be a character vector where
  every element has a name, no name appears twice, and no label is
  missing; otherwise you get an error.
- Whether each name is a real ARD column can only be checked once the
  script runs, because that is when the ARD’s columns are known. If a
  name does not match any column (for example a typo such as `TRT01AA`),
  the script warns you and carries on writing the file. Look out for
  that warning.
- `column_labels` only affects the Dataset-JSON export. If you supply it
  without `output_format = "datasetjson"`,
  [`readARS()`](https://clymbclinical.github.io/siera/reference/readARS.md)
  warns you that it is being ignored.
- It works the same whether your ARS metadata is a `.json` or an `.xlsx`
  file.

### Where next?

- The [ARD program
  structure](https://clymbclinical.github.io/siera/articles/ARD_script_structure.md)
  vignette walks through the rest of a generated script, including the
  `res`, `pattern` and `disp` columns that come with every ARD.
- [What do I do with my
  ARD?](https://clymbclinical.github.io/siera/articles/apply-ARD.md)
  covers turning an ARD into tables or using it for QC.
