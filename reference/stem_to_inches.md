# Convert a chart size to inches

Internal helper. officer measures everything in inches; the Stem export
helpers take centimetres, which is what the analysts' slide templates
use.

## Usage

``` r
stem_to_inches(x, units = "cm")
```

## Arguments

- x:

  Numeric size, or `NULL`.

- units:

  `"cm"`, `"mm"` or `"in"`.

## Value

The size in inches, or `NULL` when `x` is `NULL`.
