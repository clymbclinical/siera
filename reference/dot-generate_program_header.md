# Generate the programme header banner

Internal helper that produces the standardised programme banner inserted
at the top of each generated ARD script.

## Usage

``` r
.generate_program_header(OutputId, Output_Name, date)
```

## Arguments

- OutputId:

  Identifier of the output being generated.

- Output_Name:

  Human-readable name of the output.

- date:

  Timestamp used for the header creation date.

## Value

Character string containing the formatted programme header.
