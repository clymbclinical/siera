# Render the method-template library as a human-readable Markdown catalog

Internal generator that turns the plain-text method library
(\`inst/method-library/\`: per-method \`method.json\` + \`template.R\`,
plus \`constructs.json\`) into a single Markdown catalog. The committed
\`inst/method-library/METHODS.md\` is produced by this function; a
contract test asserts the committed file matches the rendered output so
the human-readable view can never drift from the source of truth.

## Usage

``` r
.render_method_library_md(lib_dir = NULL)
```

## Arguments

- lib_dir:

  Path to the method-library directory. Defaults to the installed (or
  \`load_all()\`ed) copy via \`system.file()\`.

## Value

Character vector of Markdown lines.

## Details

Regenerate the committed catalog with:
\`writeLines(siera:::.render_method_library_md(),
"inst/method-library/METHODS.md")\`
