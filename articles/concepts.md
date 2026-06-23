# Concepts and conventions

``` r

library(siera)
```

This vignette steps back from the “how” (covered in the other vignettes)
to explain the “why” behind *siera*: what an Analysis Results Dataset
(ARD) is good for, how *siera* fits into the workflow, and the
conventions you will see in the generated scripts and their output.

## Why an ARD?

Traditionally, analysis results have lived inside static outputs - the
numbers in an RTF or PDF table. An **Analysis Results Dataset (ARD)**
instead stores those same results as *machine-readable data*, one row
per result, with metadata describing exactly what each number is. Once
results are data, several things become a lot easier:

- **QC** - results can be compared programmatically against an
  independent calculation, instead of by eye.
- **Re-use across outputs** - the same result can feed a table, a
  figure, or a listing without being re-calculated.
- **Re-use across reporting events** - results computed once can be
  carried into a later submission or a different deliverable.
- **Creating final TFLs** - downstream packages such as
  [`tfrmt`](https://gsk-biostatistics.github.io/tfrmt/) and
  [`gtsummary`](https://www.danieldsjoberg.com/gtsummary/) format ARDs
  straight into submission-ready tables.

*siera’s* job is to get you to an ARD without writing the analysis code
by hand: you supply ARS metadata, and *siera* writes the R that produces
the ARD.

## The siera pipeline

![ARS metadata is read by readARS(), which writes one R script per
output; each script runs against ADaM datasets to produce an
ARD.](figures/siera-pipeline.svg)

The flow is always the same:

1.  You start with **ARS metadata** (the Analysis Results Standard
    description of your reporting event), in either JSON or Excel
    format.
2.  You pass it to
    **[`readARS()`](https://clymbclinical.github.io/siera/reference/readARS.md)**,
    which writes **one R script per Output** defined in the metadata.
3.  You run a generated script against your **ADaM datasets** - supplied
    as either CSV (`.csv`) or SAS transport (`.xpt`) files - and the
    result is an **ARD** - one row per result, ready for downstream use.
    siera reads each ADaM dataset according to its file extension
    (`.csv` with
    [`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html),
    `.xpt` with
    [`haven::read_xpt()`](https://haven.tidyverse.org/reference/read_xpt.html)),
    so no extra argument is needed.

The statistical computation itself is performed by the
[`cards`](https://insightsengineering.github.io/cards/) and
[`cardx`](https://insightsengineering.github.io/cardx/) packages, whose
functions *siera* writes into the generated scripts (see the vignette on
[using `cards` and
`cardx`](https://clymbclinical.github.io/siera/articles/using-cards.md)).

## The seven ARS sections siera consumes

ARS metadata describes a whole reporting event, but *siera* only needs
seven sections to generate code. It is worth knowing what each one
contributes:

| ARS section | What siera does with it |
|----|----|
| *mainListOfContents* | Links each Output to its analyses, and sets the row order and indentation of the table stub. |
| *otherListsOfContents* | Supplies Output-level metadata (the list of planned outputs). |
| *analysisSets* | Defines the population filter for the Output (e.g. Safety Population, `SAFFL == "Y"`). |
| *dataSubsets* | Adds row-level filters for individual analyses (e.g. serious, treatment-emergent AEs). |
| *analysisGroupings* | Defines the columns/subgroups results are split by (e.g. treatment arm), including data-driven groupings discovered at run time. |
| *analyses* | Ties everything together for one calculation: which method, population, subset and groupings apply. |
| *methods* | Describes the operations to perform, and carries the dynamic R code template *siera* fills in. |

Each generated script is assembled from these pieces, and every result
it produces carries identifiers back to them (see “Reading an ARD row”
below).

## JSON and XLSX parity

ARS metadata officially travels as JSON, but *siera* also accepts an
Excel (XLSX) representation of the same information. **The two are
semantically equivalent** -
[`readARS()`](https://clymbclinical.github.io/siera/reference/readARS.md)
produces the same generated scripts either way, so you can choose
whichever format fits your tooling. The examples shipped with the
package include both (see
[`ARS_example()`](https://clymbclinical.github.io/siera/reference/ARS_example.md)).

## Reading an ARD row: CDISC traceability columns

A core promise of the ARD is *traceability*: every result can be traced
back to the metadata that defines it. To make that possible, each row of
a *siera*-generated ARD carries identifier columns alongside the
statistic itself:

- **`AnalysisId`** - which analysis produced the row.
- **`operationid`** - which operation within the method (e.g. the `n`
  count vs. the `%`).
- For each grouping applied to the analysis, a set of `group[n]_*`
  columns:
  - **`group[n]_groupingId`** - the grouping the column belongs to
    (e.g. the treatment-arm grouping).
  - **`group[n]_groupId`** - for **pre-defined groups** (groups listed
    explicitly in the metadata), the identifier of the specific group.
  - **`group[n]_groupValue`** - for **data-driven groupings**
    (`dataDriven: true`, where the categories are discovered from the
    ADaM data at run time, e.g. cause of death or AE term), the actual
    value found in the data.

The distinction matters: a treatment-arm grouping is usually
pre-defined, so its rows carry `group1_groupId`; a grouping such as
“cause of death” is typically data-driven, so its rows carry
`group1_groupValue` with whatever categories appeared in the data. A
single ARD can contain both. The [ARD program
structure](https://clymbclinical.github.io/siera/articles/ARD_script_structure.md)
vignette shows where these columns are stamped on in the generated code.
