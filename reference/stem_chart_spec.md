# Describe a Stem ggplot in chart terms

Internal helper that reads a ggplot object produced by one of the Stem
plotting functions and returns everything the mschart conversion needs:
the aggregated data, which column plays which role, the chart flavour
(`"simple"`, `"stacked"` or `"dodged"`), the series colours and the
typography resolved from the active theme.

## Usage

``` r
stem_chart_spec(plot)
```

## Arguments

- plot:

  A ggplot2 object created by a Stem plotting function.

## Value

A named list describing the chart.
