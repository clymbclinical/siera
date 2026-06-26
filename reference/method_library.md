# Browse the bundled analysis-method template library

siera ships a plain-text library of analysis-method code templates (the
\`templateCode\` + parameters + valueSource recipes that drive ARD
generation) under \`inst/method-library\`. This accessor lists the
available methods and resolves the on-disk location of a method's files,
so you do not have to call \[system.file()\] directly. Each method's
stable \`id\` is the key intended for an ARS file to reference.

## Usage

``` r
method_library(id = NULL)
```

## Arguments

- id:

  Method id (e.g. \`"risk_difference"\`). If \`NULL\` (default), the
  available method ids are returned.

## Value

When \`id\` is \`NULL\`, a character vector of available method ids.
Otherwise, the path to that method's directory (containing
\`method.json\` and \`template.R\`).

## Details

The human-readable catalog (\`METHODS.md\`) and the valueSource registry
(\`constructs.json\`) live at the library root, i.e.
\`system.file("method-library", package = "siera")\`.

## Examples

``` r
method_library()
#> [1] "anova"                     "categorical_summary"      
#> [3] "chisq"                     "continuous_summary"       
#> [5] "fishers_exact"             "risk_difference"          
#> [7] "risk_difference_per_group" "total_n"                  
method_library("risk_difference")
#> [1] "/home/runner/work/_temp/Library/siera/method-library/04_risk_difference"
```
