# Colour palette generator factory

Returns a function `function(n)` that selects `n` colours from the named
palette. This factory is used internally by
[`scale_colour_stem()`](https://stem-cz.github.io/stemtools/reference/scale_colour_stem.md)
and
[`scale_fill_stem()`](https://stem-cz.github.io/stemtools/reference/scale_colour_stem.md)
to satisfy ggplot2's discrete scale interface, but can also be called
directly.

Diverging palettes use
[`select_diverging()`](https://stem-cz.github.io/stemtools/reference/select_diverging.md)
to pick colours that preserve the symmetric structure of the palette.
All other palette types return the first `n` colours, optionally
reversed.

## Usage

``` r
stem_palette_gen(palette = "div1", direction = 1)
```

## Arguments

- palette:

  Name of the palette. See
  [`stem_palette()`](https://stem-cz.github.io/stemtools/reference/stem_palette.md)
  for the full list of valid names.

- direction:

  `1` (default) returns colours in their natural order; `-1` reverses
  the order.

## Value

A `function(n)` that returns a character vector of `n` hex colour codes.

## See also

[`stem_palette()`](https://stem-cz.github.io/stemtools/reference/stem_palette.md),
[`select_diverging()`](https://stem-cz.github.io/stemtools/reference/select_diverging.md),
[`scale_colour_stem()`](https://stem-cz.github.io/stemtools/reference/scale_colour_stem.md)

## Examples

``` r
# Create a generator then call it for 3 colours
gen <- stem_palette_gen("modern")
gen(3)
#> [1] "#35978F" "#B0C89F" "#BF812D"

# Reversed order
stem_palette_gen("div1", direction = -1)(5)
#> [1] "#FC684D" "#FDA592" "#D1C9BC" "#8CD2CC" "#4DA8A0"
```
