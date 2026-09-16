# Add a Stem plot to a PowerPoint or Word document as a native chart

Converts `plot` with
[`stem_as_mschart()`](https://stem-cz.github.io/stemtools/reference/stem_as_mschart.md)
and adds it to an existing officer document. For a PowerPoint document
the chart is placed on the current slide, so add a slide with
[`officer::add_slide()`](https://davidgohel.github.io/officer/reference/add_slide.html)
first.

## Usage

``` r
stem_add_chart(
  x,
  plot,
  width = NULL,
  height = NULL,
  left = NULL,
  top = NULL,
  units = "cm",
  location = NULL,
  ...
)
```

## Arguments

- x:

  An
  [`officer::read_pptx()`](https://davidgohel.github.io/officer/reference/read_pptx.html)
  or
  [`officer::read_docx()`](https://davidgohel.github.io/officer/reference/read_docx.html)
  document.

- plot:

  A ggplot2 object created by a Stem plotting function, or an
  `ms_barchart` already produced by
  [`stem_as_mschart()`](https://stem-cz.github.io/stemtools/reference/stem_as_mschart.md).

- width, height:

  Size of the chart, in `units`. In Word both default to a readable 15 x
  10 cm. In PowerPoint both default to `NULL`, which uses the body
  placeholder of the slide layout; give them to size the chart by hand.

- left, top:

  PowerPoint only: position of the chart's top-left corner, in `units`.
  Only used when `width` and `height` are given; both default to `0`.

- units:

  Unit of `width`, `height`, `left` and `top`: `"cm"` (default), `"mm"`
  or `"in"`.

- location:

  PowerPoint only: an
  [`officer::ph_location()`](https://davidgohel.github.io/officer/reference/ph_location.html)
  giving where to place the chart. Takes precedence over
  `width`/`height`/`left`/`top`.

- ...:

  Passed to
  [`stem_as_mschart()`](https://stem-cz.github.io/stemtools/reference/stem_as_mschart.md).

## Value

The document `x`, updated.

## See also

[`stem_as_mschart()`](https://stem-cz.github.io/stemtools/reference/stem_as_mschart.md)

## Examples

``` r
if (FALSE) { # \dontrun{
doc <- officer::read_pptx()
doc <- officer::add_slide(doc, "Title and Content", "Office Theme")
doc <- stem_add_chart(doc, stem_barplot(trust, government))
print(doc, target = "chart.pptx")
} # }
```
