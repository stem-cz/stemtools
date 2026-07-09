# Computes parametric standard error for proportions

Computes SE for proportions using the old-school \\\sqrt{\frac{p (1 -
p)}{n}}\\ formula.

## Usage

``` r
se_prop(p, n)
```

## Arguments

- p:

  proportion

- n:

  sample size

## Value

numeric value representing SE for proportions

## Examples

``` r
p = 0.3
n = 200
se_prop(p = p, n = n)
#> [1] 0.0324037
```
