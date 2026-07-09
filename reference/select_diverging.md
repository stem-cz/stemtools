# Select n colours from a diverging palette

Selects `n` colours from a diverging palette while preserving its
symmetric, center-anchored structure. Rather than simply taking the
first `n` colours, this function samples symmetrically from both ends of
the palette:

- **Odd `n`**: the palette midpoint is always included, with an equal
  number of colours drawn from each side.

- **Even `n`**: colours are drawn evenly from the left and right halves.

The selected colours are always returned in their original palette
order.

## Usage

``` r
select_diverging(palette, n)
```

## Arguments

- palette:

  Character vector of hex colour codes representing the full diverging
  palette.

- n:

  Number of colours to select. Must be between 1 and `length(palette)`.

## Value

Character vector of `n` hex colour codes in original palette order.

## See also

[`stem_palette_gen()`](https://stem-cz.github.io/stemtools/reference/stem_palette_gen.md),
[`stem_palette()`](https://stem-cz.github.io/stemtools/reference/stem_palette.md)

## Examples

``` r
pal <- stem_palette("modern")

# Odd n: midpoint included, balanced sides
select_diverging(pal, n = 3)
#> [1] "#35978F" "#B0C89F" "#BF812D"

# Even n: two from each end
select_diverging(pal, n = 4)
#> [1] "#35978F" "#80CDC1" "#DFC27D" "#BF812D"
```
