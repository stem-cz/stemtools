# Registry of all stem colour palettes

A named list of S7 objects of class `stem_palette`, one entry per
palette. This is the single source of truth for all palette definitions
in the package.
[`stem_palette()`](https://stem-cz.github.io/stemtools/reference/stem_palette.md),
[`stem_palette_gen()`](https://stem-cz.github.io/stemtools/reference/stem_palette_gen.md)
and
[`stem_palettes_all()`](https://stem-cz.github.io/stemtools/reference/stem_palettes_all.md)
all derive their data from this object.

To add a new palette to the package, append an entry here using
[`stem_colors()`](https://stem-cz.github.io/stemtools/reference/stem_colors.md).

## Usage

``` r
.stem_palettes
```

## Format

A named list of `stem_palette` S7 objects. Each element has three
properties:

- `name`:

  Character. The palette identifier, matching the list name.

- `type`:

  Character. One of `"nominal"`, `"diverging"`, or `"sequential"`.

- `colors`:

  Character vector of hex colour codes.
