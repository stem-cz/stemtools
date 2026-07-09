# Inline stacked bar plot

Plots the (possibly weighted) distribution of a single categorical
variable as one stacked horizontal bar. Useful as a compact, inline
summary of a single item.

## Usage

``` r
stem_inline(
  data,
  item,
  weight = NULL,
  collapse_item = NULL,
  palette = "div1",
  direction = 1,
  labels = TRUE,
  label_accuracy = 1,
  label_suffix = "",
  label_hide = 0.05,
  label_color = "black",
  label_bicolor = TRUE
)
```

## Arguments

- data:

  Data frame holding the item (and group) variables.

- item:

  Categorical variable to plot.

- weight:

  Optional survey weights.

- collapse_item:

  Optional named list passed to
  [`stem_summarise_cat()`](https://stem-cz.github.io/stemtools/reference/stem_summarise_cat.md)
  to collapse (or rename) item categories.

- palette:

  Name of a Stem palette. See
  [`stem_palette()`](https://stem-cz.github.io/stemtools/reference/stem_palette.md).

- direction:

  Palette direction. Use `-1` to reverse the colours.

- labels:

  If `TRUE`, prints a percentage label at the end of each bar.

- label_accuracy:

  Rounding accuracy of labels. `1` gives whole numbers, `0.1` one
  decimal place.

- label_suffix:

  Suffix appended to labels. Defaults to `""`.

- label_hide:

  Proportions below this threshold are left unlabelled.

- label_color:

  Colour of the segment labels. Defaults to `"black"`.

- label_bicolor:

  If `TRUE` (default), the labels of the two side (extreme) response
  categories are drawn in white, while all other labels use
  `label_color`. Set to `FALSE` to colour every label with
  `label_color`.

## Value

A ggplot2 object.

## Examples

``` r
if (FALSE) { # \dontrun{
stem_inline(trust, police)
stem_inline(trust, police, weight = W)
} # }
```
