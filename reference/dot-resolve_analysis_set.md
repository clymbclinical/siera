# Resolve an analysis set to its population dataset and filter

Internal helper for \[.generate_population_code()\]. Translates one ARS
analysis set into the dataset and filter expression of its population.
When the set cannot be used (no id, no metadata, unknown id or
incomplete condition) it warns and falls back to the unfiltered dataset
of the first analysis using it, signalled by a \`NULL\` filter.

## Usage

``` r
.resolve_analysis_set(set_id, analysis_sets, analysis_id, analysis_dataset)
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

## Value

A list with \`dataset\`, \`filter\` (character or \`NULL\`) and
\`name\`.
