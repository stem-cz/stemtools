# Draw a thin black box around the legend of a chart

Internal helper used by
[`stem_patch_charts()`](https://stem-cz.github.io/stemtools/reference/stem_patch_charts.md).
[`theme_stem()`](https://stem-cz.github.io/stemtools/reference/theme_stem.md)
outlines the legend; mschart has no legend border setting, so a 0.75 pt
black outline is written into the legend's shape properties. The schema
wants `<c:spPr>` right before `<c:txPr>`, which mschart always writes
for the legend text. Legends that already carry shape properties are
left alone.

## Usage

``` r
stem_box_legends(xml)
```

## Arguments

- xml:

  The chart XML, as a single string.

## Value

The patched XML.
