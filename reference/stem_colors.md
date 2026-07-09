# stem S7 palette class

Internal S7 class used to define and validate colour palettes. Not
intended for direct use; use
[`stem_palette()`](https://stem-cz.github.io/stemtools/reference/stem_palette.md)
to retrieve palette colours.

## Usage

``` r
stem_colors(name = character(0), type = character(0), colors = logical(0))
```

## Arguments

- name:

  Character. Identifier name for the palette.

- type:

  Character. Palette type: `"nominal"`, `"diverging"`, or
  `"sequential"`.

- colors:

  Character vector of hex colour codes.

## Value

S7 object of class `"stem_palette"`.
