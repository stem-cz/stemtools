# Computes parametric standard error for median

Computes SE for the median using the old-school
\\\sqrt{1.2533\cdot\frac{var_x}{n}}\\ formula. This assumes normal
distribution.

## Usage

``` r
se_median(x)
```

## Arguments

- x:

  numeric vector

## Value

numeric value representing SE for medians

## Examples

``` r
se_median(trust$age)
#> [1] 0.133125
```
