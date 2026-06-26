# Convert manifest \`parameters\` to a tibble and validate their valueSources

Convert manifest \`parameters\` to a tibble and validate their
valueSources

## Usage

``` r
.manifest_parameters_tibble(params, method_id, path)
```

## Arguments

- params:

  The manifest method's \`parameters\` (a list of named lists), or NULL.

- method_id:

  Id used only for error messages.

- path:

  Manifest path used only for error messages.

## Value

A tibble with \`name\`, \`description\`, \`valueSource\`, or \`NULL\`
when the method declares no parameters.
