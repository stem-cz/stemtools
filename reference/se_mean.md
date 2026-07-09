# Computes parametric standard error for mean

Computes SE for the mean using the old-school \\\sqrt{\frac{var_x}{n}}\\
formula.

## Usage

``` r
se_mean(x)
```

## Arguments

- x:

  numeric vector

## Value

numeric value representing SE for means

## Examples

``` r
se_mean(trust$age)
#> [1] 0.1062196
```
