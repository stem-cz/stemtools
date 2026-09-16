# Translate a ggplot theme into an mschart theme

Internal helper turning the typography and the "no gridlines, no ticks"
look of
[`theme_stem()`](https://stem-cz.github.io/stemtools/reference/theme_stem.md)
into the equivalent
[`mschart::mschart_theme()`](https://ardata-fr.github.io/mschart/reference/set_theme.html).
The font family and the colours follow the plot's theme; the font
*sizes* are the fixed export sizes (11 pt axis and legend text, 12 pt
axis titles, 14 pt chart title), matching the decks produced by the Stem
apps.

## Usage

``` r
stem_mschart_theme(spec, legend = TRUE)
```

## Arguments

- spec:

  A specification produced by
  [`stem_chart_spec()`](https://stem-cz.github.io/stemtools/reference/stem_chart_spec.md).

- legend:

  If `TRUE`, the legend is shown.

## Value

An mschart theme.
