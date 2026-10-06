# Link ARS identifiers to an analysis' ARD

The last step of every analysis in a siera-generated ARD script: it
takes the ARD that the analysis method computed (typically a cards
object) and links it back to the ARS metadata by adding the CDISC ARD
identifier columns:

## Usage

``` r
ars_stamp(ard, analysis_id, method_id, output_id, groupings = list())
```

## Arguments

- ard:

  The ARD computed by the analysis method: a data frame (typically a
  cards object), or \`NULL\` when the analysis had no data.

- analysis_id:

  Analysis id: a single string (\`analyses\[\].id\`).

- method_id:

  Method id: a single string (\`analyses\[\].methodId\`).

- output_id:

  Output id: a single string (the \`outputId\` of the
  \`mainListOfContents\` list item the analysis belongs to).

- groupings:

  A list of \[ars_grouping()\] objects
  (\`analyses\[\].orderedGroupings\[\]\`). The position in the list is
  the ARD group number: the first grouping is stamped onto
  \`group1\_\*\`, the second onto \`group2\_\*\`, and so on. Each needs
  the matching \`group\<n\>\_level\` column in \`ard\`. Default: no
  groupings.

## Value

\`ard\` with the identifier columns added (a \`data.frame\` stub when
\`ard\` is \`NULL\`).

## Details

\* \`AnalysisId\`, \`MethodId\` and \`OutputId\`; \* for each grouping,
\`group\<n\>\_groupingId\` plus either \`group\<n\>\_groupId\`
(pre-defined groups, looked up from the \`group\<n\>\_level\` column) or
\`group\<n\>\_groupValue\` (data-driven groupings); \* all \`\*\_level\`
columns are coerced to character, so ARDs of different analyses can be
row-bound safely.

When \`ard\` is \`NULL\` - the analysis had no data, so the method was
skipped - a statistics-free stub row carrying only \`AnalysisId\`,
\`MethodId\` and \`OutputId\` is returned.

siera calls this function in the scripts it generates, with the values
copied from the ARS metadata - each argument maps to one ARS element:

|  |  |  |
|----|----|----|
| **Argument** | **ARS JSON path** | **ARD column(s)** |
| \`ard\` | result of the analysis method (\`methods\[\].codeTemplate\`) | (input) |
| \`analysis_id\` | \`analyses\[\].id\` | \`AnalysisId\` |
| \`method_id\` | \`analyses\[\].methodId\` | \`MethodId\` |
| \`output_id\` | \`mainListOfContents\` list item \`outputId\` | \`OutputId\` |
| \`groupings\` | \`analyses\[\].orderedGroupings\[\]\` | \`group\<n\>\_groupingId\`, \`group\<n\>\_groupId\`, \`group\<n\>\_groupValue\` |

## See also

\[ars_grouping()\], \[readARS()\]

## Examples

``` r
# A tiny ARD, as an analysis method might return it
ard <- tibble::tibble(
  group1_level = c("Placebo", "Xanomeline Low Dose"),
  stat_name = "n",
  stat = list(86L, 84L)
)

ars_stamp(
  ard,
  analysis_id = "An_01",
  method_id = "Mth_01",
  output_id = "Out_01",
  groupings = list(
    ars_grouping(
      "AnlsGrouping_01_Trt",
      groups = c(
        "AnlsGrouping_01_Trt_1" = "Placebo",
        "AnlsGrouping_01_Trt_2" = "Xanomeline Low Dose"
      )
    )
  )
)
#> # A tibble: 2 × 8
#>   group1_level    stat_name stat  AnalysisId MethodId OutputId group1_groupingId
#>   <chr>           <chr>     <lis> <chr>      <chr>    <chr>    <chr>            
#> 1 Placebo         n         <int> An_01      Mth_01   Out_01   AnlsGrouping_01_…
#> 2 Xanomeline Low… n         <int> An_01      Mth_01   Out_01   AnlsGrouping_01_…
#> # ℹ 1 more variable: group1_groupId <chr>

# No data: a stub row identifying the analysis is returned
ars_stamp(NULL, "An_01", "Mth_01", "Out_01")
#>   AnalysisId MethodId OutputId
#> 1      An_01   Mth_01   Out_01
```
