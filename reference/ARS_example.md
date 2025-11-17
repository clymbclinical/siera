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
#>  [1] "ADAE.csv"                          "ADSL.csv"                         
#>  [3] "ADVS.csv"                          "ADZSDER.csv"                      
#>  [5] "Common_Safety_Displays_cards.xlsx" "cards_constructs.xlsx"            
#>  [7] "exampleARS_1.json"                 "exampleARS_1a.json"               
#>  [9] "exampleARS_2.json"                 "exampleARS_2.xlsx"                
#> [11] "exampleARS_2a.xlsx"                "exampleARS_3.json"                
#> [13] "exampleARS_3.xlsx"                 "exampleARS_4.json"                
#> [15] "test_cards.json"                  
ARS_example("Common_Safety_Displays_cards.xlsx")
#> [1] "/home/runner/work/_temp/Library/siera/extdata/Common_Safety_Displays_cards.xlsx"
```
