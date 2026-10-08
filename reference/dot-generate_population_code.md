# Generate the population code for one output

Internal helper that builds, once per output, the code defining every
analysis-set population frame the output's analyses need, and reports
which frame each analysis reads from. Everything is derived from the ARS
metadata rather than from the position of an analysis within the output
(#197):

## Usage

``` r
.generate_population_code(
  anas,
  analyses,
  analysis_sets,
  analysis_groupings = NULL,
  data_subsets = NULL
)
```

## Arguments

- anas:

  The analyses tied to the current output (Lopa subset), in order.

- analyses:

  Analyses metadata for the reporting event.

- analysis_sets:

  AnalysisSets metadata for the reporting event.

- analysis_groupings:

  AnalysisGroupings metadata for the reporting event.

- data_subsets:

  DataSubsets metadata for the reporting event.

## Value

A list with \`code\` (the population block), \`frames\` (named character
vector: analysis id -\> frame its data subset starts from) and
\`poptot\` (named character vector: analysis id -\> subject-level frame
of its analysis set, substituted for \`df_poptot\` in method templates).

## Details

\* one population per distinct \`analysisSetId\` used by the output; \*
an analysis reads the \*\*subject-level\*\* frame (\`df_poptot\`, the
analysis set condition applied to its own dataset, typically ADSL) when
every dataset it touches is the analysis set's dataset - its groupings'
\`groupingDataset\`, its data subset's \`condition.dataset\` and, unless
its variable is the subject key \`USUBJID\`, its own \`dataset\`. This
keeps the bigN analysis counting every subject in the population,
including those with no records in the content dataset; \* otherwise it
reads the \*\*record-level\*\* frame (\`df_pop\`): the population
inner-joined onto the one foreign dataset the analysis touches.

Frame names stay \`df_pop\` / \`df_poptot\` for the common single-set,
single-content-dataset output. With more than one analysis set the set
id is appended (\`df_poptot\_\_AnalysisSet_07\`), and when one set is
merged onto more than one content dataset the dataset is appended to the
merged frame (\`df_pop\_\_ADLB\`).
