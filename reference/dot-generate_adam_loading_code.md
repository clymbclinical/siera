# Generate ADaM loading code

Internal helper that inspects the analyses referenced by an output and
builds the code required to read the relevant ADaM datasets. Only
datasets referenced in the analyses, analysis sets, or data subsets are
included in the returned code block.

## Usage

``` r
.generate_adam_loading_code(
  Anas,
  Analyses,
  AnalysisSets,
  DataSubsets,
  adam_path
)
```

## Arguments

- Anas:

  Subset of analyses tied to the current output (Lopa rows).

- Analyses:

  Analyses metadata for the reporting event.

- AnalysisSets:

  AnalysisSets metadata for the reporting event.

- DataSubsets:

  DataSubsets metadata for the reporting event.

- adam_path:

  Directory containing the ADaM datasets on disk. ADaM datasets may be
  supplied as CSV (\`.csv\`) or SAS transport (\`.xpt\`) files; the
  reader emitted for each dataset is chosen from the file extension
  found on disk (see \[.generate_one_adam_read()\]).

## Value

Character string containing the code used to load ADaM datasets.
