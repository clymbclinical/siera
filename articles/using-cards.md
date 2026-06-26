# Making use of \`cards\` and \`cardx\`

``` r

library(siera)
```

## Introduction to cards/cardx

`cards` and `cardx` are reputable R packages, part of the `pharmaverse`,
which wrap statistical procedures and deliver Analysis Results Datasets
(ARDs).

Read more about `cards`
[here](https://insightsengineering.github.io/cards/), and `cardx`
[here](https://insightsengineering.github.io/cardx/).

## Using `cards` and `cardx` in ARS metadata

A powerful aspect of the ARS model, is that dynamic code can be supplied
as part of the metadata and be evaluated on either 1. the Output level,
2. the Analysis level, or 3. the AnalysisMethod level. In an insightful
PHUSE paper by Richard Marshall
[here](https://www.lexjansen.com/phuse-us/2025/ds/PAP_DS09.pdf), he
explains how dynamic code can ‘drive’ automation in the ARS model by
categorising it into 5 “geARS”.

“Gear 5” entails making use of the *AnalysisMethodCodeTemplate* and
*AnalysisMethodCodeParameters* classes in the metadata. The user
supplies dynamic R code (with the option of making use of parameters
referring to metadata pieces), which will be executed to perform the
relevant AnalysisMethod operations.

### Example: `ard_summary`

For the `siera` package, the R code used in *AnalysisMethodCodeTemplate*
is aligned with the *cards* and `cardx` package. Below is a simple
example of how a function *ard_summary* from the *cards* package is used
in the *AnalysisMethodCodeTemplate* class:

``` r

# example of 'cards' code in AnalysisMethodCodeTemplate, using the ard_summary function:

Analysis_ARD <- ard_summary(
  data = filtered_ADSL,
  by = c(byvariables_here),
  variables = analysisvariable_here
)
```

> **A note on `cards` function names.** Recent versions of `cards`
> renamed several functions: `ard_continuous()` is now `ard_summary()`,
> `ard_categorical()` is now `ard_tabulate()`, and `ard_complex()` is
> now `ard_mvsummary()`. *siera’s* example metadata and generated
> scripts use the current names; if you are reading scripts produced by
> an older *siera* version you may still see the previous names.

In the above example of *AnalysisMethodCodeTemplate* code for a
particular AnalysisMethod (summary statistics for a continuous
variable), the *ard_summary* function from the *cards* package is used.
Notice, however, that two parameters are used in the code:

1.  ‘byvariables_here’, and
2.  ‘analysisvariable_here’.

These are example placeholder names, to be replaced by actual variables,
depending on the context for the Analysis. For example,
‘byvariables_here’ could be replaced by TRT01A, and
‘analysisvariable_here’ could be replaced by AGE, for an ARD with
summary statistics on AGE by Treatment. The same code, however, will be
reused for summary statistics on e.g. HEIGHT for another Analysis, and
the ‘analysisvariable_here’ will be replaced with HEIGHT. All this is
dynamically applied within the
[`readARS()`](https://clymbclinical.github.io/siera/reference/readARS.md)
function when generating R scripts using *siera*. Let’s see, for
example, how the above statement would be auto-populated with actual
objects (from the ARS metadata):

``` r

# example of 'cards' code in AnalysisMethodCodeTemplate, populated with AnalysisMethodCodeParameters:

Analysis_ARD <- ard_summary(
  data = filtered_ADSL,
  by = c(TRT01A),
  variables = AGE
)
```

In order to facilitate the use of placeholders and their replacement
with actual values from the metadata, several “constructs” have been
defined in the *readARS* function, and are available to the user to call
from the *AnalysisMethodCodeTemplate* and *AnalysisMethodCodeParameters*
classes in the metadata. These constructs are either simple variables or
datasets defined in the metadata, or complex, dynamic constructs from
various metadata pieces, to be inserted in specific *cards* functions,
or pre-processing steps.

The authoritative, tested reference for these constructs — together with
a catalog of ready-to-use analysis-method templates that use them —
ships with the package as a plain-text **method-template library**. Each
template is validated automatically (it parses, references only
constructs *siera* can resolve, and stays in sync with the catalog), so
what you read there always reflects what *siera* actually does. You can
browse it with:

``` r

# list the available analysis-method templates:
method_library()

# resolve a particular method's files (method.json + template.R):
method_library("risk_difference")

# the library root also holds the authoritative construct (valueSource) list
# and a human-readable catalog of all method templates:
lib <- system.file("method-library", package = "siera")
file.path(lib, "constructs.json")
file.path(lib, "METHODS.md")
```

An earlier, illustrative list of constructs is also bundled as a
spreadsheet (the method-template library above is the source of truth):

``` r

ARS_example("cards_constructs.xlsx")
```
