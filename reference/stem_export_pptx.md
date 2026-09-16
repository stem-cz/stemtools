# Export Stem plots to PowerPoint as native charts

Writes one slide per plot, each holding a native Office chart whose
source data can be edited in Excel.

## Usage

``` r
stem_export_pptx(
  ...,
  path,
  template = NULL,
  layout = "Title and Content",
  master = "Office Theme",
  width = NULL,
  height = NULL,
  left = NULL,
  top = NULL,
  units = "cm",
  location = NULL,
  chart_args = list()
)
```

## Arguments

- ...:

  One or more ggplot2 objects created by the Stem plotting functions (or
  `ms_barchart` objects from
  [`stem_as_mschart()`](https://stem-cz.github.io/stemtools/reference/stem_as_mschart.md)).
  A single list of plots is also accepted.

- path:

  Path of the `.pptx` file to write.

- template:

  Optional path to a `.pptx` template to build on. Defaults to officer's
  blank presentation.

- layout, master:

  Name of the slide layout and of the master to use for the new slides.

- width, height:

  Size of each chart, in `units`. Both default to `NULL`, which uses the
  body placeholder of `layout`, so a template's own geometry is
  respected. Give them to size the charts by hand.

- left, top:

  Position of each chart's top-left corner, in `units`. Only used when
  `width` and `height` are given; both default to `0`.

- units:

  Unit of `width`, `height`, `left` and `top`: `"cm"` (default), `"mm"`
  or `"in"`.

- location:

  An
  [`officer::ph_location()`](https://davidgohel.github.io/officer/reference/ph_location.html)
  giving where to place the charts. Takes precedence over
  `width`/`height`/`left`/`top`.

- chart_args:

  A named list of arguments passed on to
  [`stem_as_mschart()`](https://stem-cz.github.io/stemtools/reference/stem_as_mschart.md).

## Value

The path, invisibly.

## See also

[`stem_as_mschart()`](https://stem-cz.github.io/stemtools/reference/stem_as_mschart.md),
[`stem_export_docx()`](https://stem-cz.github.io/stemtools/reference/stem_export_docx.md)

## Examples

``` r
if (FALSE) { # \dontrun{
stem_export_pptx(
  stem_barplot(trust, government, title_show = TRUE),
  stem_inline(trust, police),
  path = "trust.pptx"
)

# A 24 x 12 cm chart, 2 cm from the left and 4 cm from the top of the slide.
stem_export_pptx(
  stem_barplot(trust, government),
  path = "trust.pptx",
  width = 24, height = 12, left = 2, top = 4
)
} # }
```
