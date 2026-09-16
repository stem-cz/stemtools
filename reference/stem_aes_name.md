# Name of the data column behind an aesthetic

Internal helper. The Stem plotting functions map aesthetics either as
bare symbols (`.response`) or through the pronoun
(`.data[["government"]]`). This returns the column name for both forms,
and `NULL` when the aesthetic is a constant (e.g. the `y = ""` of an
inline bar) or absent.

## Usage

``` r
stem_aes_name(quo, data)
```

## Arguments

- quo:

  A quosure taken from a ggplot mapping.

- data:

  The plot data.

## Value

A length-one character vector, or `NULL`.
