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
  envir = parent.frame()
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

- envir:

  Environment (parent environment as default)

## Value

Character vector with formatted numbers.
