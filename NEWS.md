# stemtools 0.2.1

* `theme_stem()` now uses a 14 pt base text size (previously 18 pt) and draws the axis and legend text at 14 pt, and `label_size` defaults to `12` instead of `14`, so plots on screen match their Office exports.
* `stem_export_pptx()` and `stem_export_docx()` export the axis and legend text at 14 pt (previously 11 pt) and the numeric labels at a fixed 12 pt. The labels no longer scale with `theme_stem(label_size = )`, which now only affects the plot on screen.
* `stem_export_pptx()` and `stem_export_docx()` draw a thin black box around the chart legend, when there is one.
* `stem_summarise_cat()` with a `group` now reports every observed item category in every group also for unweighted data, with zeros where the category does not occur in that group, as it already did for weighted data. Previously unweighted tables left those rows out, so the same table had fewer rows without weights.
* `stem_battery()` stops with an error when an item has answers outside the first item's categories, listing those answers. Previously it took the first item's categories for all items and silently dropped the other items' answers outside them, so their percentages were computed from the remaining answers only.
* `stem_battery()` stops with an error naming the items that share a label. Previously their answers merged into one bar, showing a mix of both items under one name.
* `stem_battery(order_by = )` stops with an error when any of its values is not exactly a category of the items (e.g. a typo or missing diacritics). Previously such values were silently ignored, so the battery stayed unsorted or was sorted by fewer categories than intended.
* `stem_battery(order_by = )` orders the items by their shares without missing answers, as shown in the bars. Previously missing answers counted in the denominator, so items with many of them were placed lower than their bars warranted.
* `stem_battery()` warns when its items have different numbers of valid (non-missing) answers, listing the unweighted n of each item, since their bars then rest on different bases.

# stemtools 0.2.0

* Weighted estimates are now computed with `{srvyr}`/`{survey}` instead of `{surveycore}`/`{surveytidy}`, which were removed from CRAN. Results are unchanged.

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
