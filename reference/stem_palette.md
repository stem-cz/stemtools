# Retrieve a stem colour palette

Returns a character vector of hex colour codes for the requested
palette. The vector carries a `type` attribute (`"nominal"`,
`"diverging"`, or `"sequential"`) used internally by
[`stem_palette_gen()`](https://stem-cz.github.io/stemtools/reference/stem_palette_gen.md).

## Usage

``` r
stem_palette(palette = "div1")
```

## Arguments

- palette:

  Name of the palette. One of:

  **Nominal** (categorical, unordered data): `"nom1"`, `"nom2"`.

  **Sequential** (ordered data from low to high): `"seq1"`, `"seq2"`,
  `"seq3"`, `"seq4"`.

  **Diverging** (data with a meaningful midpoint): `"modern"`, `"div1"`,
  `"div2"`, `"div3"`.

## Value

Character vector of hex colour codes with a `type` attribute
(`"nominal"`, `"diverging"`, or `"sequential"`).

## See also

[`stem_palette_gen()`](https://stem-cz.github.io/stemtools/reference/stem_palette_gen.md),
[`scale_colour_stem()`](https://stem-cz.github.io/stemtools/reference/scale_colour_stem.md),
[`scale_fill_stem()`](https://stem-cz.github.io/stemtools/reference/scale_colour_stem.md)

## Examples

``` r
# Returns a plain character vector
stem_palette("modern")
#> [1] "#35978F" "#80CDC1" "#B0C89F" "#DFC27D" "#BF812D"
#> attr(,"type")
#> [1] "diverging"

# Inspect the palette type
attr(stem_palette("modern"), "type")
#> [1] "diverging"

# Use with scales package for inspection
scales::show_col(stem_palette("nom1"))
```
