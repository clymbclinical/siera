# Generate the read call for a single ADaM dataset

Internal helper that emits the code to read one ADaM dataset, choosing
the reader from the file extension present on disk. SAS transport files
(\`.xpt\`) are read with \[haven::read_xpt()\]; everything else (and the
default when no matching file is found) is read with
\[readr::read_csv()\]. To remain backward compatible, CSV takes
precedence when both a \`.csv\` and a \`.xpt\` exist for the same
dataset.

## Usage

``` r
.generate_one_adam_read(ad, adam_dir)
```

## Arguments

- ad:

  Dataset name (without extension), e.g. \`"ADSL"\`.

- adam_dir:

  Directory containing the ADaM datasets, with forward slashes.

## Value

Character string containing the read call for the dataset.

## Details

The lookup is case-insensitive on the file name, because real-world
CDISC SAS transport files are commonly lower-case (e.g. \`adsl.xpt\`)
while the ARS metadata names the dataset in upper case (\`ADSL\`). The
dataset is still assigned to an object named after the metadata dataset
name, but the path in the generated code points at the actual file found
on disk.
