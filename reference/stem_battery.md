# Stacked bar plot for a battery of like items

Plots a set of categorical variables that share the same response
categories (e.g. a battery of Likert items) as a single chart, drawing
one stacked horizontal bar per item. Internally the items are reshaped
to long format and handed to the same machinery as
[`stem_barplot()`](https://stem-cz.github.io/stemtools/reference/stem_barplot.md),
with the item taking the role of the group.

## Usage

``` r
stem_battery(
  data,
  items,
  weight = NULL,
  order_by = NULL,
  item_label = TRUE,
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

- items:

  Items to plot. Supports tidyselect helpers (e.g.
  [`dplyr::starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html)).

- weight:

  Optional survey weights.

- order_by:

  Optional vector of response categories used to order the items, e.g.
  `c("Definitely Agree", "Rather Agree")` orders items by their combined
  share of those two categories.

- item_label:

  If `TRUE` (default), item labels (the `"label"` attribute) are used
  instead of variable names. Falls back to names, with a warning, if any
  item lacks a label.

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

## Value

A ggplot2 object.

## Examples

``` r
if (FALSE) { # \dontrun{
stem_battery(trust, items = c(police, eu, government, army))
stem_battery(trust,
             items = c(police, eu, government, army),
             weight = W,
             order_by = c("Definitely Agree", "Rather Agree"))
} # }
```
