## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

* Unable to verify current time - I'm not sure how to resolve this.  Please let me know if I missed
    something.

*Please always write package names, software names and API (application
programming interface) names in single quotes in title and description.
e.g: --> 'siera'
Please note that package names are case sensitive.
For more details:
<https://contributor.r-project.org/cran-cookbook/description_issues.html#formatting-software-names>

*Please add \value to .Rd files regarding exported methods and explain
the functions results in the documentation. Please write about the
structure of the output (class) and also what the output means. (If a
function does not return a value, please document that too, e.g.
\value{No return value, called for side effects} or similar)
For more details:
<https://contributor.r-project.org/cran-cookbook/docs_issues.html#missing-value-tags-in-.rd-files>
  -> Missing Rd-tags:
      ARS_example.Rd: \value

*Some code lines in examples are commented out. Please never do that.
Ideally find toy examples that can be regularly executed and checked.
Lengthy examples (> 5 sec), can be wrapped in \donttest{}. -> Examples
in comments in:
       readARS.Rd

*Please add small files needed for the examples or vignette in the
inst/extdata subfolder of your package and use system.file() to get the
correct package path.
