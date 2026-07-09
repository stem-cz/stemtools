# Simple horizontal bar plot

Plots the (possibly weighted) distribution of a single categorical
variable as horizontal bars, one bar per category. Supplying a `group`
variable draws dodged bars so categories can be compared across groups.

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
  label_hide = 0,
  errorbar = FALSE
)
```

## Arguments

- data:

  Data frame holding the item (and group) variables.

- item:

  Categorical variable to plot.

- group:

  Optional grouping variable. When supplied, bars are dodged and
  coloured by group and proportions are computed within each group.

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

  If `TRUE`, prints a percentage label at the end of each bar.

- label_accuracy:

  Rounding accuracy of labels. `1` gives whole numbers, `0.1` one
  decimal place.

- label_suffix:

  Suffix appended to labels. Defaults to `""`.

- label_hide:

  Proportions below this threshold are left unlabelled.

- errorbar:

  If `TRUE`, adds 95% confidence interval error bars.

## Value

A ggplot2 object.

## Examples

``` r
if (FALSE) { # \dontrun{
stem_barplot(trust, government)
stem_barplot(trust, police, group = eu_index, weight = W)
} # }
```
