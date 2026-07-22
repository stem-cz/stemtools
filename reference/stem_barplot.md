# Horizontal bar plot

Plots the (possibly weighted) distribution of a categorical variable as
horizontal bars. Without a `group`, one bar is drawn per item category.
Supplying a `group` variable instead draws one stacked horizontal bar
per group category, with the item mapped to fill; proportions are then
computed within each group, so every bar sums to 100%.

## Usage

``` r
stem_barplot(
  data,
  item,
  group = NULL,
  weight = NULL,
  collapse_item = NULL,
  collapse_group = NULL,
  palette = "div1",
  direction = 1,
  labels = TRUE,
  label_accuracy = 1,
  label_suffix = "",
  label_hide = NULL,
  label_color = "black",
  label_bicolor = TRUE,
  errorbar = FALSE,
  title_show = FALSE,
  title_quote = FALSE,
  title_wrap = 80
)
```

## Arguments

- data:

  Data frame holding the item (and group) variables.

- item:

  Categorical variable to plot.

- group:

  Optional grouping variable. When supplied, one stacked horizontal bar
  is drawn per group category (with the item mapped to fill) and
  proportions are computed within each group.

- weight:

  Optional survey weights.

- collapse_item:

  Optional named list passed to
  [`stem_summarise_cat()`](https://stem-cz.github.io/stemtools/reference/stem_summarise_cat.md)
  to collapse (or rename) item categories.

- collapse_group:

  Optional named list to collapse (or rename) group categories.

- palette:

  Name of a Stem palette. See
  [`stem_palette()`](https://stem-cz.github.io/stemtools/reference/stem_palette.md).

- direction:

  Palette direction. Use `-1` to reverse the colours.

- labels:

  If `TRUE`, prints a percentage label on each bar.

- label_accuracy:

  Rounding accuracy of labels. `1` gives whole numbers, `0.1` one
  decimal place.

- label_suffix:

  Suffix appended to labels. Defaults to `""`.

- label_hide:

  Proportions below this threshold are left unlabelled. Defaults to
  `0.05` when a `group` is supplied (to keep small stacked segments
  unlabelled) and `0` otherwise.

- label_color:

  Colour of the segment labels in grouped (stacked) plots. Defaults to
  `"black"`.

- label_bicolor:

  If `TRUE` (default), in grouped (stacked) plots the labels of the two
  side (extreme) item categories are drawn in white, while all other
  labels use `label_color`. Set to `FALSE` to colour every label with
  `label_color`.

- errorbar:

  If `TRUE`, adds 95% confidence interval error bars (ungrouped plots
  only).

- title_show:

  If `TRUE`, adds a plot title taken from the item's `"label"`
  attribute, falling back to the variable name when no label is present.
  The title is styled (e.g. drawn in bold) by
  [`theme_stem()`](https://stem-cz.github.io/stemtools/reference/theme_stem.md).
  Defaults to `FALSE`.

- title_quote:

  If `TRUE`, wraps the title in low/high double quotation marks (`„` and
  `“`). Defaults to `FALSE`.

- title_wrap:

  Maximum number of characters per title line; longer titles are wrapped
  onto several lines so they do not overflow the plot. Use `NULL` or
  `Inf` to disable wrapping. Defaults to `80`.

## Value

A ggplot2 object.

## Examples

``` r
if (FALSE) { # \dontrun{
stem_barplot(trust, government)
stem_barplot(trust, police, group = eu_index, weight = W)
} # }
```
