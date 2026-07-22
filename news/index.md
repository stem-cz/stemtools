# Changelog

## stemtools 0.1.2

- [`stem_barplot()`](https://stem-cz.github.io/stemtools/reference/stem_barplot.md)
  and
  [`stem_inline()`](https://stem-cz.github.io/stemtools/reference/stem_inline.md)
  gain a `title_show` argument that adds a plot title taken from the
  item’s `"label"` attribute, falling back to the variable name when no
  label is present, and a `title_quote` argument that wraps the title in
  low/high double quotation marks (`„`/`“`). Both default to `FALSE`. A
  `title_wrap` argument (default `80`) wraps long titles onto several
  lines so they do not overflow the plot.
- [`theme_stem()`](https://stem-cz.github.io/stemtools/reference/theme_stem.md)
  now draws the plot title (`plot.title`) in bold.

## stemtools 0.1.1

- [`stem_barplot()`](https://stem-cz.github.io/stemtools/reference/stem_barplot.md)
  now draws a stacked horizontal bar per group category (with the item
  mapped to fill, each bar summing to 100%) when a `group` variable is
  supplied, replacing the previous dodged-bars layout. `label_hide`
  defaults to `0.05` in this grouped mode to keep small segments
  unlabelled.
- Removed `stem_barstack()`, whose behaviour is now covered by grouped
  [`stem_barplot()`](https://stem-cz.github.io/stemtools/reference/stem_barplot.md).

## stemtools 0.1.0

- Added a `testthat` test suite covering aggregation, palettes, scales,
  theme, plotting and utility helpers, plus an `R-CMD-check` GitHub
  Actions workflow.
- [`theme_stem()`](https://stem-cz.github.io/stemtools/reference/theme_stem.md)
  gains a `family` argument to control (or disable) the font family;
  pass `family = ""` on machines without Calibri.
- [`stem_summarise_cat()`](https://stem-cz.github.io/stemtools/reference/stem_summarise_cat.md)
  and
  [`stem_summarise_num()`](https://stem-cz.github.io/stemtools/reference/stem_summarise_num.md)
  now always return a tibble, so the weighted and unweighted paths share
  the same output type.
- Updated
  [`scale_colour_stem()`](https://stem-cz.github.io/stemtools/reference/scale_colour_stem.md)/[`scale_fill_stem()`](https://stem-cz.github.io/stemtools/reference/scale_colour_stem.md)
  for ggplot2 (\>= 3.5.0), removing the deprecated `scale_name`
  argument.
- Renamed the misspelled `collaps_cats` argument of
  [`collapse_cats()`](https://stem-cz.github.io/stemtools/reference/collapse_cats.md)
  to `collapse_cats`.
- Fixed the standard-error formula shown in
  [`?se_prop`](https://stem-cz.github.io/stemtools/reference/se_prop.md).
- [`create_project()`](https://stem-cz.github.io/stemtools/reference/create_project.md)
  no longer errors when called directly without the wizard arguments,
  writes a starter `README.md`, and drops the `renv` integration. Fixed
  a no-op [`paste()`](https://rdrr.io/r/base/paste.html) in the
  `.gitignore` writer.
- Dropped the unused `rstudioapi`, `stringr`, `ragg` and `renv`
  dependencies.

## stemtools 0.0.5

- Added Rstudio project template

## stemtools 0.0.4

- Adjusted defaults for `stem_plot_barstack()` and `stem_plot_battery()`
  to be in line with how they are commonly used.

## stemtools 0.0.3

- Added `stem_plot_multichoice()` for plotting multiple choice items

## stemtools 0.0.2

- Added basic ploting functions: `stem_plot_single`,
  `stem_plot_multiple`, `stem_plot_bar`, `stem_plot_bartable()`.
- Added convenience function `stem_ggplot()`, which tries to export
  `stem_plot_*` figures with proper dimensions.
- When using Rstudio, `ragg` is automatically set as default graphical
  device when stemtools is loaded.

## stemtools 0.0.1

- Added `bluered_light` diverging color palette.

## stemtools 0.0.0.9000

- Added a `NEWS.md` file to track changes to the package.
