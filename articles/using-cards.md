# Making use of \`cards\` and \`cardx\`

``` r
library(siera)
```

## Introduction to cards/cardx

`cards` and `cardx` are reputable R packages, part of the `pharmaverse`,
which wrap statistical procedures and deliver Analysis Results Datasets
(ARDs).

Read more about `cards`
[here](https://insightsengineering.github.io/cards/latest-tag/), and
`cardx` [here](https://insightsengineering.github.io/cards/latest-tag/).

## Using `cards` and `cardx` in ARS metadata

A powerful aspect of the ARS model, is that dynamic code can be supplied
as part of the metadata and be evaluated on either 1. the Output level,
2. the Analysis level, or 3. the AnalysisMethod level. In an insightful
insightful PHUSE paper by Richard Marshall
[here](https://www.lexjansen.com/phuse-us/2025/ds/PAP_DS09.pdf), he
explains how dynamic code can ‘drive’ automation in the ARS model by
categorising it into 5 “geARS”.

“Gear 5” entails making use of the *AnalysisMethodCodeTemplate* and
*AnalysisMethodCodeParameters* classes in the metadata. The user
supplies dynamic R code (with the option of making use of parameters
referring to metadata pieces), which will be executed to perform the
relevant AnalysisMethod operations.

### Example: `ard_continuous`

For the `siera` package, the R code used in *AnalysisMethodCodeTemplate*
is aligned with the *cards* and `cardx` package. Below is a simple
example of how a function *ard_continuous* from the *cards* package is
used in the *AnalysisMethodCodeTemplate* class:

``` r
# example of 'cards' code in AnalysisMethodCodeTemplate, using the ard_continuous function:

Analysis_ARD <- ard_continuous(
  data = filtered_ADSL,
  by = c(byvariables_here),
  variables = analysisvariable_here
)
```

In the above example of *AnalysisMethodCodeTemplate* code for a
particular AnalysisMethod (summary statistics for a continuous
variable), the *ard_continuous* function from the *cards* package is
used. Notice, however, that two parameters are used in the code:

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

Analysis_ARD <- ard_continuous(
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
or pre-processing steps. A list of defined constructs and examples is
shipped with this package, and can be accessed with:

``` r
ARS_example("cards_constructs.xlsx")
```
