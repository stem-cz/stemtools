# Build a plot title from a variable label or name

Internal helper that returns the `"label"` attribute of
`data[[item_name]]`, falling back to `item_name` when the variable has
no label. Optionally wraps the title in Czech low/high double quotation
marks.

## Usage

``` r
stem_plot_title(data, item_name, quote = FALSE, wrap = 80)
```

## Arguments

- data:

  Data frame holding the item variable.

- item_name:

  Name of the item column.

- quote:

  If `TRUE`, wrap the title in `„` and `“`.

- wrap:

  Maximum number of characters per line. Titles longer than this are
  wrapped onto several lines. Use `NULL` or `Inf` to disable wrapping.

## Value

A length-one character vector.
