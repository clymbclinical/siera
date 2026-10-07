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
#>  [5] "ADZSDER.csv"                       "Common_Safety_Displays_cards.json"
#>  [7] "Common_Safety_Displays_cards.xlsx" "cards_constructs.xlsx"            
#>  [9] "exampleARS_1.json"                 "exampleARS_1a.json"               
#> [11] "exampleARS_2.json"                 "exampleARS_2.xlsx"                
#> [13] "exampleARS_2a.xlsx"                "exampleARS_3.json"                
#> [15] "exampleARS_3.xlsx"                 "exampleARS_4.json"                
#> [17] "exampleARS_5.json"                 "exampleARS_5.xlsx"                
#> [19] "exampleARS_5_documentref.json"     "exampleARS_6.json"                
#> [21] "exampleARS_6.xlsx"                 "exampleARS_7.json"                
#> [23] "exampleARS_methods.json"           "test_cards.json"                  
ARS_example("Common_Safety_Displays_cards.json")
#> [1] "/home/runner/work/_temp/Library/siera/extdata/Common_Safety_Displays_cards.json"
```
