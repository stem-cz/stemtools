# Border width of the bars of a Stem plot

Internal helper reading the width of the separator ggplot2 draws between
the bars, in points, so the native chart can reproduce it. mschart wants
points where ggplot2 uses millimetres.

## Usage

``` r
stem_border_width(built, layer_idx)
```

## Arguments

- built:

  The result of
  [`ggplot2::ggplot_build()`](https://ggplot2.tidyverse.org/reference/ggplot_build.html)
  for the plot.

- layer_idx:

  Index of the bar layer.

## Value

The border width in points.
