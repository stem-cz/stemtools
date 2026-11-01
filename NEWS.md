# stemtools 0.2.0

* Weighted estimates are now computed with `{srvyr}` (Taylor-series linearization via `{survey}`) instead of the discontinued `{surveycore}` and `{surveytidy}`, which are no longer dependencies, so stemtools installs from CRAN packages again. Point estimates, standard errors and confidence intervals are unchanged: the intervals are still Wald intervals (estimate ± z × SE, with the normal quantile), with Taylor-linearized SEs for weighted data. The design effect (`"deff"`, new) is computed against simple random sampling with replacement.
* `stem_summarise()`, `stem_summarise_cat()` and `stem_summarise_num()` gain:
  * `variance`, selecting any of the standard error (`"se"`), variance (`"var"`), coefficient of variation (`"cv"`), confidence interval (`"ci"`, the default), margin of error (`"moe"`) and design effect (`"deff"`), returned as e.g. `freq_se` or `mean_moe`. `variance = NULL` returns point estimates only.
  * `conf_level` (default `0.95`) for the confidence interval and margin of error.
  * `n_weighted`, adding an `n_weighted` column with the sum of weights per cell.
  * `min_cell_n` (default `30`): a warning of class `stemtools_warning_small_cell` is raised when any cell has fewer unweighted observations. The plotting functions do not raise it.
* Variables with `{haven}` value labels are converted to factors using their labels, in the summary functions and in all plotting functions. haven is only a suggested dependency, needed only when labelled data is present. `stem_summarise()` treats a labelled item as categorical.
* `stem_battery()` gains `title`, `title_quote` and `title_wrap`. A battery's title is usually the wording its items share, so it is given as a string rather than read from an item label. Like the other titles, it carries over to native charts.
* `stem_inline()`, `stem_battery()` and `stem_barplot()` gain `dk_levels` for "don't know" categories (e.g. `"Nevím"`) and `dk_color` (default `"grey70"`). These categories move to the end (the right end of stacked bars and of the legend; the bottom of single-colour bar plots), are drawn grey and do not count towards the palette size, so a 5-point scale plus "Nevím" no longer runs out of colours. Several "don't know" levels get distinguishable greys. Stacked native charts keep the same series and colours. In an ungrouped `stem_barplot()` the grey bars become separate series of the native chart (one worksheet column per "don't know" level, fully overlapping the main series, no legend), because Office charts colour whole series rather than single bars; without `dk_levels` these charts are unchanged.
* With `label_bicolor = TRUE` (the default), the labels of stacked bars now take their colour from each segment's fill: white on dark fills, `label_color` on light ones (Rec. 601 luma below 150). Native charts apply each series' own colour. **This changes existing output** for the nominal and sequential palettes, which used to get white labels on their first and last colour however light (e.g. white on pale `seq1`). The diverging palettes look as before, and a grey "don't know" segment no longer gets a white label.
* `stem_multiselect()` gains a yes/no layout: with `selected = "Ano"`, items such as `q5_1` ... `q5_8` answered "Ano"/"Ne" get one bar each, showing the (possibly weighted) share of valid answers equal to `selected`, named after the items' labels (`item_label`). Previously such items collapsed into two bars, "Ano" and "Ne", with inflated shares. A new `keep_last` argument keeps bars such as "Jiné" or "Ostatní" at the end in either layout.
* New `stem_boxplot()` draws a (possibly weighted) numeric variable as horizontal box plots in the Stem look, one per `group` category. Quartiles come from the weighted inverse empirical CDF (the first value whose cumulative weight share reaches p; with equal weights the same as `quantile(type = 1)`), whiskers reach the most extreme values within 1.5 × IQR, outliers are drawn as points and the weighted mean as a white diamond. Office has no box plot, so `stem_as_mschart()` stops with a clear message asking to save these plots as images.
* The diverging palettes (`modern`, `div1`, `div2`, `div3`) gain 6- and 7-colour versions, so 6- and 7-point scales can be stacked. The 7-colour versions add a darker colour at each end of the 5-colour palette; the 6-colour versions drop its neutral midpoint. Selections for 2-5 categories are unchanged. When a palette still runs out of colours, the error suggests `collapse_item`.
* `theme_stem()` uses a smaller type scale sized for charts exported 19 cm wide: a 13 pt base (new `base_size` argument, previously a fixed 18 pt), 15.6 pt bold titles and 12 pt data labels (`label_size`, previously 14). Relative sizes and line widths are unchanged. **This changes the size of all text in plots using the theme.** Native charts keep their fixed sizes, including 11 pt data labels.
* Single-series bars (ungrouped `stem_barplot()` and `stem_multiselect()`) are now filled with the Stem brand burgundy `#82143C`, whatever the `palette`; a new `bar_color` argument changes it. **This changes existing output**: they used to take the palette's first colour (teal `#4DA8A0` for `div1`). Native charts follow. The `theme_stem()` `accent` is burgundy too (previously teal `#35978F`).
* Colours, label colours and category counts follow the factor's full set of levels: `stem_summarise_cat()` and `stem_summarise()` gain `drop = FALSE`, returning levels nobody chose as 0 % rows (within each group, weighted or not), and the stacked plots colour every level. **This changes existing output**: summaries now include those zero rows, and an empty scale point no longer shifts the colours of the others. Empty cells don't trigger the small-cell warning. Use `drop = TRUE` for the previous behaviour.

## Bug fixes

* `stem_battery(order_by = )` scores items from the plotted shares, so missing answers no longer lower an item's score. Items with missing answers may now be ordered differently.
* `stem_battery()` stops with an error naming the items and categories that differ when the items do not share the same categories in the same order. Previously answers outside the first item's levels silently became `NA` and the bars were re-percentaged.

# stemtools 0.1.3

* `theme_stem()` gains a `label_size` argument setting the point size of the numeric labels drawn inside the plot, defaulting to the `14` used by the Stem apps.
* New `stem_as_mschart()`, `stem_export_pptx()`, `stem_export_docx()` and `stem_add_chart()` turn the Stem plots into *native* Microsoft Office charts, which carry an embedded worksheet that can be edited in Excel. The charts render like the exports produced by the Stem apps (`blockr.stem`).

# stemtools 0.1.2

* `stem_barplot()` and `stem_inline()` gain a `title_show` argument that adds a plot title taken from the item's `"label"` attribute, falling back to the variable name when no label is present, and a `title_quote` argument that wraps the title in low/high double quotation marks (`„`/`“`). Both default to `FALSE`. A `title_wrap` argument (default `80`) wraps long titles onto several lines so they do not overflow the plot.
* `theme_stem()` now draws the plot title (`plot.title`) in bold.

# stemtools 0.1.1

* `stem_barplot()` now draws a stacked horizontal bar per group category (with the item mapped to fill, each bar summing to 100%) when a `group` variable is supplied, replacing the previous dodged-bars layout. `label_hide` defaults to `0.05` in this grouped mode to keep small segments unlabelled.
* Removed `stem_barstack()`, whose behaviour is now covered by grouped `stem_barplot()`.

# stemtools 0.1.0

* Added a `testthat` test suite covering aggregation, palettes, scales, theme, plotting and utility helpers, plus an `R-CMD-check` GitHub Actions workflow.
* `theme_stem()` gains a `family` argument to control (or disable) the font family; pass `family = ""` on machines without Calibri.
* `stem_summarise_cat()` and `stem_summarise_num()` now always return a tibble, so the weighted and unweighted paths share the same output type.
* Updated `scale_colour_stem()`/`scale_fill_stem()` for ggplot2 (>= 3.5.0), removing the deprecated `scale_name` argument.
* Renamed the misspelled `collaps_cats` argument of `collapse_cats()` to `collapse_cats`.
* Fixed the standard-error formula shown in `?se_prop`.
* `create_project()` no longer errors when called directly without the wizard arguments, writes a starter `README.md`, and drops the `renv` integration. Fixed a no-op `paste()` in the `.gitignore` writer.
* Dropped the unused `rstudioapi`, `stringr`, `ragg` and `renv` dependencies.

# stemtools 0.0.5
* Added Rstudio project template

# stemtools 0.0.4

* Adjusted defaults for `stem_plot_barstack()` and `stem_plot_battery()` to be in line with how they are commonly used.

# stemtools 0.0.3

* Added `stem_plot_multichoice()` for plotting multiple choice items

# stemtools 0.0.2

* Added basic ploting functions: `stem_plot_single`, `stem_plot_multiple`, `stem_plot_bar`, `stem_plot_bartable()`.
* Added convenience function `stem_ggplot()`, which tries to export `stem_plot_*` figures with proper dimensions.
* When using Rstudio, `ragg` is automatically set as default graphical device when stemtools is loaded.

# stemtools 0.0.1

* Added `bluered_light` diverging color palette.

# stemtools 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
