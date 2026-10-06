# Describe one ARS analysis grouping for \[ars_stamp()\]

Builds a small, validated description of one entry of an analysis'
\`orderedGroupings\`, so that \[ars_stamp()\] can add the CDISC ARD
columns \`group\<n\>\_groupingId\`, \`group\<n\>\_groupId\` and (for
data-driven groupings) \`group\<n\>\_groupValue\`. siera calls this
function in the scripts it generates, with the values copied from the
ARS metadata - each argument maps to one ARS element:

## Usage

``` r
ars_grouping(id, groups = NULL, data_driven = FALSE)
```

## Arguments

- id:

  Grouping id: a single, non-empty string
  (\`analyses\[\].orderedGroupings\[\].groupingId\`).

- groups:

  Optional named character vector describing the pre-defined groups of a
  non-data-driven grouping. Names are group ids
  (\`analysisGroupings\[\].groups\[\].id\`), values the matching
  condition values
  (\`analysisGroupings\[\].groups\[\].condition.value\`). \`NULL\`
  (default) means the grouping defines no groups.

- data_driven:

  Single logical, \`analysisGroupings\[\].dataDriven\`. Defaults to
  \`FALSE\`.

## Value

An object of class \`siera_ars_grouping\`, to be supplied in the
\`groupings\` list of \[ars_stamp()\].

## Details

|  |  |  |
|----|----|----|
| **Argument** | **ARS JSON path** | **Notes** |
| \`id\` | \`analyses\[\].orderedGroupings\[\].groupingId\` (= \`analysisGroupings\[\].id\`) | Written to \`group\<n\>\_groupingId\`. |
| \`groups\` | \`analysisGroupings\[\].groups\[\]\` | Names are the group ids (\`groups\[\].id\`), values the group condition values (\`groups\[\].condition.value\`). An \`IN\` condition repeats the name once per value. |
| \`data_driven\` | \`analysisGroupings\[\].dataDriven\` | When \`TRUE\` the groups are discovered from the data, so \`groups\` must be empty. |

## See also

\[ars_stamp()\]

## Examples

``` r
# A pre-defined grouping (e.g. treatment arms)
ars_grouping(
  "AnlsGrouping_01_Trt",
  groups = c(
    "AnlsGrouping_01_Trt_1" = "Placebo",
    "AnlsGrouping_01_Trt_2" = "Xanomeline Low Dose"
  )
)
#> $id
#> [1] "AnlsGrouping_01_Trt"
#> 
#> $groups
#> AnlsGrouping_01_Trt_1 AnlsGrouping_01_Trt_2 
#>             "Placebo" "Xanomeline Low Dose" 
#> 
#> $data_driven
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "siera_ars_grouping"

# A data-driven grouping (categories are discovered from the data)
ars_grouping("AnlsGrouping_04_AEBODSYS", data_driven = TRUE)
#> $id
#> [1] "AnlsGrouping_04_AEBODSYS"
#> 
#> $groups
#> character(0)
#> 
#> $data_driven
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "siera_ars_grouping"
```
