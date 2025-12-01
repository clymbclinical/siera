# siera 0.5.5

* Strengthened analysis method validation with clear errors for missing or undefined `MethodId` values and warnings when method templates or parameter value sources cannot be resolved.
* Added guardrails for analysis set generation that warn on missing or incomplete metadata and fall back to the unfiltered analysis dataset when needed.
* Tightened ARS metadata ingestion by requiring `analysisSets` definitions in JSON and the analysis code template sheets in Excel workbooks.
* Expanded test coverage for analysis methods, analysis sets, and data subset condition handling to capture more metadata edge cases.

# siera 0.5.4

* Comprehensive testing added: checking generated ARD result values
* added warning messages with cli()
* cover scenarios for bigN vs table-level subsetting

# siera 0.5.3

* Cover scenario of multi-value DataSubsets
* Cover scenario of overlapping variables in ADSL merge

# siera 0.5.2

* Added conditional operators to cover EQ, NE, GE, GT, LE, LT

# siera 0.5.0

* readARS function to ingest xlsx, json and CDISC example

# siera 0.4.0

* addition of readARS_xl function
* example added of {cards} in AnalysisMethodCodeTemplate

# siera 0.3.0

* dynamic recognition of ADaMs

# siera 0.1.0

* Initial CRAN submission.
