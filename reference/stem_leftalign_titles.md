# Left-align the title of a chart

Internal helper used by
[`stem_patch_charts()`](https://stem-cz.github.io/stemtools/reference/stem_patch_charts.md).
Office centres a chart title, whereas
[`theme_stem()`](https://stem-cz.github.io/stemtools/reference/theme_stem.md)
sets `plot.title.position = "plot"`, which flushes it left above the
whole plot. mschart exposes no alignment setting, so the paragraph
property is written into the chart XML, as the Stem apps do.

## Usage

``` r
stem_leftalign_titles(xml)
```

## Arguments

- xml:

  The chart XML, as a single string.

## Value

The patched XML.
