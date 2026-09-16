# Point size of the numeric labels of a Stem plot

Internal helper reading back the size ggplot2 resolved for the in-plot
numeric labels. The built layer data carries the value in millimetres,
including the default that
[`theme_stem()`](https://stem-cz.github.io/stemtools/reference/theme_stem.md)
supplies through
[`ggplot2::element_geom()`](https://ggplot2.tidyverse.org/reference/element.html),
so the export follows a custom `label_size` without the caller having to
repeat it.

## Usage

``` r
stem_label_size(plot, built)
```

## Arguments

- plot:

  A ggplot object.

- built:

  The result of
  [`ggplot2::ggplot_build()`](https://ggplot2.tidyverse.org/reference/ggplot_build.html)
  for `plot`.

## Value

The label size in points. Falls back to `11` when the plot draws no
labels at all.
