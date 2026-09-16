# Export Stem plots to Word as native charts

Writes a `.docx` file holding one native Office chart per plot, each
with an embedded worksheet that can be edited in Excel.

## Usage

``` r
stem_export_docx(
  ...,
  path,
  template = NULL,
  width = 15,
  height = 10,
  units = "cm",
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

  Path of the `.docx` file to write.

- template:

  Optional path to a `.docx` template to build on.

- width, height:

  Size of each chart, in `units`. Defaults to 15 x 10 cm, which fits the
  text width of a portrait A4 page.

- units:

  Unit of `width`, `height`, `left` and `top`: `"cm"` (default), `"mm"`
  or `"in"`.

- chart_args:

  A named list of arguments passed on to
  [`stem_as_mschart()`](https://stem-cz.github.io/stemtools/reference/stem_as_mschart.md).

## Value

The path, invisibly.

## See also

[`stem_as_mschart()`](https://stem-cz.github.io/stemtools/reference/stem_as_mschart.md),
[`stem_export_pptx()`](https://stem-cz.github.io/stemtools/reference/stem_export_pptx.md)

## Examples

``` r
if (FALSE) { # \dontrun{
stem_export_docx(stem_barplot(trust, government), path = "trust.docx")

# Half-page charts.
stem_export_docx(
  stem_barplot(trust, government),
  path = "trust.docx",
  width = 16, height = 8
)
} # }
```
