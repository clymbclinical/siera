# Split an XLSX \`pageRef_pages\` cell into page names

The CDISC ARS xlsx representation stores \`pageRefs.pageNames\` in a
single \`pageRef_pages\` cell; multiple names are separated by a comma
or semicolon.

## Usage

``` r
.split_page_names(x)
```

## Arguments

- x:

  A scalar \`pageRef_pages\` cell value.

## Value

Character vector of page names; \`character(0)\` when empty.
