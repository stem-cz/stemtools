# Build a stacked horizontal bar plot

Internal renderer shared by
[`stem_barplot()`](https://stem-cz.github.io/stemtools/reference/stem_barplot.md)
and
[`stem_inline()`](https://stem-cz.github.io/stemtools/reference/stem_inline.md).
Draws a single stacked horizontal bar per `y_name` category (or one bar
in total when `y_name` is `NULL`).

## Usage

``` r
stem_stack(
  plot_data,
  fill_name,
  y_name = NULL,
  palette = "div1",
  direction = 1,
  labels = TRUE,
  label_color = "black",
  label_bicolor = TRUE
)
```

## Arguments

- plot_data:

  Data produced by
  [`stem_plot_data()`](https://stem-cz.github.io/stemtools/reference/stem_plot_data.md).

- fill_name:

  Name of the item column mapped to `fill`.

- y_name:

  Name of the group column mapped to the y axis, or `NULL` for a single
  (inline) bar.

- palette:

  Name of a Stem palette. See
  [`stem_palette()`](https://stem-cz.github.io/stemtools/reference/stem_palette.md).

- direction:

  Palette direction. Use `-1` to reverse the colours.

- labels:

  If `TRUE`, prints a percentage label on each bar.

- label_color:

  Colour of the segment labels in grouped (stacked) plots. Defaults to
  `"black"`.

- label_bicolor:

  If `TRUE` (default), in grouped (stacked) plots the labels of the two
  side (extreme) item categories are drawn in white, while all other
  labels use `label_color`. Set to `FALSE` to colour every label with
  `label_color`.

## Value

A ggplot2 object.
