# siera
siera is an R package that ingests ARS metadata (JSON or xlsx), and produces R programs that generate ARDs.

## ARS
ARS (Analysis Results Standard) is a foundational CDISC standard that provides a framework for organising metadata as it relates to the Analysis Results for a Reporting Event.  Amongst other things, this metadata 
contains machine-readable descriptions of the following:
1. Analysis Sets applicable to the Reporting Event
2. Data Groupings to be applied to data in various analyses
3. Data Filters/Subsets
4. Statistical Operations
5. Formats (e.g. 'N=XX')

By linking all this metadata in a logical model (which is now an industry standard), ARS paves the way for TFL automation.  

## ARD
An ARD (Analysis Results Dataset) is a tabular representation of all results for an analysis (this could be a single output, or a full reporting event).  Auto-generating  an ARD becomes possible when using ARS, as 
each result is based on specific metadata relating to its Analysis Set, Grouping(s), Subset(s), Operation, etc.  The variables of the ARD are structured to include descriptive variables for the result, as well as
a variable for the result itself.  These results are machine-readable, and can be utilised in various ways, including:
1. As validation for Production Results
2. As an intermediate step in producing actual Outputs - the ARD only needs to be formatted/transposed to the Shell format.
3. In AI processes that aim to summarise and report on the results in context for the CSR.

<!-- badges: start -->
[![R-CMD-check](https://github.com/clymbclinical/siera/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/clymbclinical/siera/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->
