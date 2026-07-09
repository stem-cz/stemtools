# Complete ggplot2 theme for Stem.

A *complete* ggplot2 theme carrying the Stem look. Because it is
complete, the Stem plotting functions do not apply it themselves;
instead activate it globally with
[`ggplot2::theme_set()`](https://ggplot2.tidyverse.org/reference/get_theme.html)
so every plot picks it up:

## Usage

``` r
theme_stem(
  ink = "black",
  paper = "white",
  accent = "#35978F",
  family = "Calibri",
  ...
)
```

## Arguments

- ink:

  Foreground colour used for text, titles and axis lines. Also passed to
  [`ggplot2::element_geom()`](https://ggplot2.tidyverse.org/reference/element.html)
  as the default geom foreground colour.

- paper:

  Background colour used behind the panel, plot, strips and legend keys.
  Also passed to
  [`ggplot2::element_geom()`](https://ggplot2.tidyverse.org/reference/element.html)
  as the default geom background colour. Use `NA` for a transparent
  background (useful when exporting plots to be placed on a coloured
  surface).

- accent:

  Accent colour passed to
  [`ggplot2::element_geom()`](https://ggplot2.tidyverse.org/reference/element.html)
  as the default geom accent colour (e.g. the fill of
  [`ggplot2::geom_smooth()`](https://ggplot2.tidyverse.org/reference/geom_smooth.html)).
  Defaults to the primary Stem brand colour.

- family:

  Font family for all text. Defaults to `"Calibri"`, the Stem house
  font. Pass `""` to use the graphics device's default family (useful on
  machines where Calibri is not installed).

- ...:

  Arguments to be passed to
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html),
  overriding the Stem defaults.

## Value

A complete ggplot2
[`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html)
object.

## Details

    ggplot2::theme_set(theme_stem())

It can still be added to an individual plot with `+ theme_stem()` in the
usual way. The theme is built on
[`ggplot2::theme_grey()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
with the Stem overrides layered on top.

## Examples

``` r
if (FALSE) { # \dontrun{
# Activate globally for the whole session.
ggplot2::theme_set(theme_stem())
stem_barplot(trust, government)

# Or add it to a single plot.
stem_barplot(trust, government) + theme_stem()
} # }
```
