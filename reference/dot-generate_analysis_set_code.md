# Generate analysis set code and context

Internal helper that builds the code needed to apply an analysis set to
the current analysis. The function inspects the ARS metadata to
determine filter conditions and returns the templated code together with
the name of the resulting population dataset.

## Usage

``` r
.generate_analysis_set_code(
  j,
  analysis_sets,
  analyses,
  anas,
  analysis_set_id,
  analysis_id
)
```

## Arguments

- j:

  Index of the current analysis within the output loop.

- analysis_sets:

  AnalysisSets metadata for the reporting event.

- analyses:

  Analyses metadata for the reporting event.

- anas:

  The analyses tied to the current output (Lopa subset).

- analysis_set_id:

  Identifier for the analysis set used by the analysis.

- analysis_id:

  Identifier for the analysis that is being generated.

## Value

A list containing the generated code and the name of the dataset that
holds the filtered analysis population.
