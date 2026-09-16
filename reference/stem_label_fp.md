# Data label colours for a native chart

Internal helper reproducing the bicolour labels of the stacked Stem
plots: the two extreme (side) categories are labelled in white,
everything else in the theme's foreground colour.

## Usage

``` r
stem_label_fp(spec, series_names)
```

## Arguments

- spec:

  A specification produced by
  [`stem_chart_spec()`](https://stem-cz.github.io/stemtools/reference/stem_chart_spec.md).

- series_names:

  Names of the chart series.

## Value

A named list of
[`officer::fp_text()`](https://davidgohel.github.io/officer/reference/fp_text.html)
objects, one per series.
