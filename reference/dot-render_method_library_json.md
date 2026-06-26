# Render the method-template library as a referenceable JSON manifest catalog

Internal generator that turns the plain-text method library into a
single machine-readable \*\*method manifest\*\*
(\`inst/method-library/method-library.json\`): a superset of each
\`method.json\` that also carries the \`templateCode\` body, so the
whole library ships as one ARS-referenceable document. An ARS file can
then point a method's \`codeTemplate.documentRef\` at this catalog and
select a method by its \`id\` via a \`pageRefs\` named destination
(resolved by \[.resolve_method_documentref()\]). The committed catalog
is produced by this function and a contract test asserts it matches, so
the catalog can never drift from the per-method source files.

## Usage

``` r
.render_method_library_json(lib_dir = NULL)
```

## Arguments

- lib_dir:

  Path to the method-library directory. Defaults to the installed (or
  \`load_all()\`ed) copy via \`system.file()\`.

## Value

Character scalar of pretty-printed JSON (one trailing newline-free
string; write with \[writeLines()\]).

## Details

Regenerate the committed catalog with:
\`writeLines(siera:::.render_method_library_json(),
"inst/method-library/method-library.json")\`
