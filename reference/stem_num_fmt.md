# Derive an Excel number format from Stem percentage labels

Internal helper. Reads the preformatted `stem_label` column back to work
out how many decimal places and which suffix the ggplot labels use, and
returns the matching Excel number format code. Values handed to the
chart are percentages (0-100), so the `%` sign is escaped rather than
applied as a format multiplier.

## Usage

``` r
stem_num_fmt(labels, fallback = "0")
```

## Arguments

- labels:

  Character vector of formatted labels (the `stem_label` column).

- fallback:

  Number format used when no label can be parsed.

## Value

A length-one character vector holding an Excel number format code.
