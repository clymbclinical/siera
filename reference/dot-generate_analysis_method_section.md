# Format numeric values consistently

Internal helper to apply consistent number formatting across analyses.

## Usage

``` r
.generate_analysis_method_section(
  analysis_methods,
  analysis_method_code_template,
  analysis_method_code_parameters,
  method_id,
  analysis_id,
  output_id,
  value_sources = list(),
  code_style = c("expanded", "wrapped"),
  population_frame = "df_poptot"
)
```

## Arguments

- analysis_methods:

  AnalysisMethod dataset for the reporting event

- analysis_method_code_template:

  AnalysisMethodCodeTemplate dataset for the reporting event

- analysis_method_code_parameters:

  AnalysisMethodCodeParameters dataset for the reporting event

- method_id:

  MethodId for the method applied to current Analysis

- analysis_id:

  AnalysisId for current Analysis

- output_id:

  OutputId to which current Analysis belongs

- value_sources:

  Named list of string values that ARS code-template parameters can
  reference via their valueSource key (e.g. by_vars, ana_var, AG_var1).
  An entry may also be a zero-argument function returning the string; it
  is called lazily, only when a parameter of the method actually
  references that valueSource (e.g. AG_var2_group_values, whose
  resolution can warn). Operation IDs (operation_1, operation_2, …) are
  derived from the method itself and do not need to be supplied here.

- code_style:

  \`"expanded"\` (default) writes the empty-data guard and the
  identifier stamp out inline; \`"wrapped"\` emits only the method call
  inside \`df3\_\<id\> \<- NULL\` followed by an \`if (nrow(df2\_\<id\>)
  != 0)\` block, and leaves the identifier linking to the caller's
  \`siera::ars_stamp()\` step (see \`.generate_stamp_code()\`).

- population_frame:

  Name of the subject-level population frame of the analysis' own
  analysis set. Method templates refer to the population as
  \`df_poptot\`; when an output uses more than one analysis set the
  frames are suffixed (e.g. \`df_poptot\_\_AnalysisSet_07\`, \#197) and
  the template's \`df_poptot\` is rewritten to this name.

## Value

Character vector with formatted numbers.
