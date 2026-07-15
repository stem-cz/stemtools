# Compute plotting frequencies for a categorical variable

Connects the frequency engine
[`stem_summarise_cat()`](https://stem-cz.github.io/stemtools/reference/stem_summarise_cat.md)
to the Stem plotting functions. Returns (possibly weighted) proportions,
their 95% confidence intervals and a preformatted percentage label.
Proportions are computed within `group` when a grouping variable is
supplied.

## Usage

``` r
stem_plot_data(
  data,
  item,
  group = NULL,
  weight = NULL,
  collapse_item = NULL,
  collapse_group = NULL,
  label_accuracy = 1,
  label_suffix = "",
  label_hide = 0
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

- label_accuracy:

  Rounding accuracy of labels. `1` gives whole numbers, `0.1` one
  decimal place.

- label_suffix:

  Suffix appended to labels. Defaults to `""`.

- label_hide:

  Proportions below this threshold are left unlabelled. Defaults to
  `0.05` when a `group` is supplied (to keep small stacked segments
  unlabelled) and `0` otherwise.

## Value

A tidy data frame with the item (and group) variable and columns `freq`,
`freq_low`, `freq_upp` and `stem_label`.
