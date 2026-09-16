# Category levels of a plotting variable

Internal helper returning the levels of a factor (dropping unused ones)
or the values of a character/numeric column in first-appearance order,
which is the order ggplot2 and the embedded worksheet should agree on.

## Usage

``` r
stem_levels(x)
```

## Arguments

- x:

  A vector taken from the plot data.

## Value

A character vector of levels.
