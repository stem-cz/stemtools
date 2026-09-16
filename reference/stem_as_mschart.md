# Convert a Stem plot into a native Microsoft chart

Takes a ggplot2 object created by one of the Stem plotting functions and
returns an
[`mschart::ms_barchart()`](https://ardata-fr.github.io/mschart/reference/ms_barchart.html)
that renders as closely as possible to the ggplot original, but is a
*native* Office chart: once placed in a PowerPoint or Word file it
carries its own embedded worksheet, so the source data can be opened and
edited in Excel.

## Usage

``` r
stem_as_mschart(
  plot,
  title = NULL,
  labels = NULL,
  legend = NULL,
  num_fmt = NULL,
  axis_num_fmt = "0\"%\"",
  axis_show = TRUE,
  axis_major_unit = NULL,
  gap_width = 30,
  value_name = "%"
)
```

## Arguments

- plot:

  A ggplot2 object created by
  [`stem_barplot()`](https://stem-cz.github.io/stemtools/reference/stem_barplot.md),
  [`stem_inline()`](https://stem-cz.github.io/stemtools/reference/stem_inline.md),
  [`stem_battery()`](https://stem-cz.github.io/stemtools/reference/stem_battery.md)
  or
  [`stem_multiselect()`](https://stem-cz.github.io/stemtools/reference/stem_multiselect.md).

- title:

  Chart title. Defaults to the ggplot title (set with
  `title_show = TRUE`), or no title when the plot has none. Use `NA` to
  drop the title.

- labels:

  If `TRUE`, prints a data label on every segment. Defaults to whatever
  the ggplot does.

- legend:

  If `TRUE`, shows a legend. Defaults to `TRUE` for plots with a fill
  variable and `FALSE` otherwise, matching the ggplot.

- num_fmt:

  Excel number format code for the data labels, e.g. `"0"`, `"0.0"` or
  `"0 \"%\""`. By default the chart prints the plot's own label text
  instead of formatting the worksheet values, so the accuracy
  (`label_accuracy`) and the blanks left by `label_hide` carry over
  exactly. Give a format to label the values from the worksheet instead,
  which keeps the labels live when the data are edited in Excel.

- axis_num_fmt:

  Excel number format code for the value axis. Defaults to `"0\"%\""`,
  matching the percentage axis of the Stem plots.

- axis_show:

  If `TRUE` (default), draws the value axis.

- axis_major_unit:

  Spacing of the value axis ticks, in percentage points. Defaults to
  `25` for stacked charts and `10` otherwise.

- gap_width:

  Gap between bars, as a percentage of the bar width.

- value_name:

  Name of the value column in the embedded worksheet; it is also the
  series name of charts without a fill variable.

## Value

An object of class `ms_barchart`, ready for
[`officer::ph_with()`](https://davidgohel.github.io/officer/reference/ph_with.html)
/
[`mschart::body_add_chart()`](https://ardata-fr.github.io/mschart/reference/body_add_chart.html)
or for the Stem helpers
[`stem_add_chart()`](https://stem-cz.github.io/stemtools/reference/stem_add_chart.md),
[`stem_export_pptx()`](https://stem-cz.github.io/stemtools/reference/stem_export_pptx.md)
and
[`stem_export_docx()`](https://stem-cz.github.io/stemtools/reference/stem_export_docx.md).

## Details

The aggregated data, the category order, the palette, the label
formatting and the fonts are all read back off the ggplot object, so the
native chart needs no recomputation and stays in sync with the ggplot
version.

A few ggplot features have no Office equivalent and are dropped, with a
message: confidence interval error bars (`errorbar = TRUE`) and the
background bars of
[`stem_multiselect()`](https://stem-cz.github.io/stemtools/reference/stem_multiselect.md).

## See also

[`stem_add_chart()`](https://stem-cz.github.io/stemtools/reference/stem_add_chart.md),
[`stem_export_pptx()`](https://stem-cz.github.io/stemtools/reference/stem_export_pptx.md),
[`stem_export_docx()`](https://stem-cz.github.io/stemtools/reference/stem_export_docx.md)

## Examples

``` r
if (FALSE) { # \dontrun{
chart <- stem_as_mschart(stem_barplot(trust, government))

stem_export_pptx(
  stem_barplot(trust, government),
  stem_battery(trust, items = c(police, eu, government, army)),
  path = "charts.pptx"
)
} # }
```
