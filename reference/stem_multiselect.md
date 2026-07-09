# Bar plot for a set of multiple-choice items

Plots a set of "select all that apply" items, showing the share of
respondents that picked each option. The items are reshaped to long
format and their (possibly weighted) frequencies are rescaled by the
number of items so that the bars express percentages of respondents
rather than of responses.

## Usage

``` r
stem_multiselect(
  data,
  items,
  group = NULL,
  weight = NULL,
  palette = "div1",
  direction = 1,
  labels = TRUE,
  label_accuracy = 1,
  label_suffix = "",
  label_hide = 0,
  infreq_order = TRUE,
  background = TRUE,
  background_fill = "grey85",
  background_alpha = 1
)
```

## Arguments

- data:

  Data frame holding the item (and group) variables.

- items:

  Items to plot. Supports tidyselect helpers (e.g.
  [`dplyr::starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html)).

- group:

  Optional grouping variable. When supplied, bars are dodged and
  coloured by group and proportions are computed within each group.

- weight:

  Optional survey weights.

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

- infreq_order:

  If `TRUE` (default), options are ordered by frequency.

- background:

  If `TRUE` (default, ungrouped plots only), draws a full-width
  background bar behind each option.

- background_fill:

  Fill colour of the background bars.

- background_alpha:

  Opacity of the background bars.

## Value

A ggplot2 object.

## Examples

``` r
if (FALSE) { # \dontrun{
stem_multiselect(trust, items = dplyr::starts_with("biggest_concern"))
stem_multiselect(trust, items = dplyr::starts_with("biggest_concern"), weight = W)
} # }
```
