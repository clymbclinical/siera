# Get path to ARS example files

siera comes bundled with some example files in its \`inst/extdata\`
directory. This function make them easy to access.

## Usage

``` r
ARS_example(path = NULL)
```

## Arguments

- path:

  Name of file. If \`NULL\`, the example files will be listed.

## Value

A list of example files (if path is NULL), or a file itself if path is
used.

## Examples

``` r
ARS_example()
#>  [1] "ADAE.csv"                          "ADEXSUM.csv"                      
#>  [3] "ADSL.csv"                          "ADVS.csv"                         
#>  [5] "ADZSDER.csv"                       "Common_Safety_Displays_cards.xlsx"
#>  [7] "cards_constructs.xlsx"             "exampleARS_1.json"                
#>  [9] "exampleARS_1a.json"                "exampleARS_2.json"                
#> [11] "exampleARS_2.xlsx"                 "exampleARS_2a.xlsx"               
#> [13] "exampleARS_3.json"                 "exampleARS_3.xlsx"                
#> [15] "exampleARS_4.json"                 "exampleARS_5.json"                
#> [17] "exampleARS_5.xlsx"                 "test_cards.json"                  
ARS_example("Common_Safety_Displays_cards.xlsx")
#> [1] "/home/runner/work/_temp/Library/siera/extdata/Common_Safety_Displays_cards.xlsx"
```
