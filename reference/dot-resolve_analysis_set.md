# Resolve an analysis set to its population dataset and filter

Internal helper for \[.generate_population_code()\]. Translates one ARS
analysis set into the dataset and filter expression of its population.
When the set cannot be used (no id, no metadata, unknown id or
incomplete condition) it warns and falls back to the unfiltered dataset
of the first analysis using it, signalled by a \`NULL\` filter.

## Usage

``` r
.resolve_analysis_set(
  set_id,
  analysis_sets,
  analysis_id,
  analysis_dataset,
  set_datasets
)
```

## Arguments

- set_id:

  Analysis set identifier (or \`NA\`).

- analysis_sets:

  AnalysisSets metadata for the reporting event.

- analysis_id:

  First analysis using the set (for messages).

- analysis_dataset:

  Dataset of that analysis (fallback population).

- set_datasets:

  Datasets of every analysis using the set (picks the population dataset
  of a filter-less set).

## Value

A list with \`dataset\`, \`filter\` (character, \`""\` for a filter-less
set, or \`NULL\` for the fallback) and \`name\`.

## Details

A set declaring neither a \`condition\` nor a \`compoundExpression\`
(e.g. "All Subjects") is valid ARS and resolves silently to an empty
filter (#200). Having no declared dataset, its population is the
subject-level \`ADSL\` when one of its analyses is on \`ADSL\`, else the
first analysis' dataset; unlike the fallback it is merged onto content
datasets like a filtered set.
