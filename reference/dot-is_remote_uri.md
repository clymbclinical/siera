# Is a location a remote URI?

Any explicit URI scheme followed by \`://\` (e.g. \`http://\`,
\`https://\`, \`ftp://\`, \`file://\`) counts as remote and is rejected
in v1. Windows drive paths (\`C:\\..\`, \`C:/...\`) and POSIX paths do
not match.

## Usage

``` r
.is_remote_uri(location)
```

## Arguments

- location:

  Scalar character.

## Value

\`TRUE\` if remote.
