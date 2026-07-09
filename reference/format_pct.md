# Format proportions as percentage labels

Internal helper used to turn proportions into ready-to-plot percentage
labels. Proportions below `hide` are returned as empty strings so that
small segments stay unlabelled.

## Usage

``` r
format_pct(x, accuracy = 1, suffix = " %", hide = 0)
```

## Arguments

- x:

  Numeric vector of proportions (between 0 and 1).

- accuracy:

  Rounding accuracy passed to
  [`scales::percent()`](https://scales.r-lib.org/reference/percent_format.html).

- suffix:

  Suffix appended to each label.

- hide:

  Proportions smaller than this are returned as empty strings.

## Value

Character vector of formatted labels.
