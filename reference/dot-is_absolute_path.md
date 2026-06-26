# Is a path absolute (so it must not be joined to the ARS directory)?

Matches POSIX absolute paths (\`/...\`) and Windows paths, both
drive-letter (\`C:\\..\`, \`C:/...\`) and UNC/leading-separator forms.

## Usage

``` r
.is_absolute_path(path)
```

## Arguments

- path:

  Scalar character.

## Value

\`TRUE\` if absolute.
