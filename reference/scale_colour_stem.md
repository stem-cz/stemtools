# stem discrete colour and fill scales for ggplot2

Discrete ggplot2 scales that apply a stem colour palette to the `colour`
or `fill` aesthetic. Three aliases are provided:

- `scale_colour_stem()` — maps to the `colour` aesthetic (British
  spelling).

- `scale_color_stem()` — alias for `scale_colour_stem()` (American
  spelling).

- `scale_fill_stem()` — maps to the `fill` aesthetic.

All functions delegate to
[`ggplot2::discrete_scale()`](https://ggplot2.tidyverse.org/reference/discrete_scale.html)
via
[`stem_palette_gen()`](https://stem-cz.github.io/stemtools/reference/stem_palette_gen.md),
so any argument accepted by `discrete_scale()` (e.g. `name`, `breaks`,
`labels`, `na.value`, `drop`) can be passed through `...`.

## Usage

``` r
scale_fill_stem(palette = "div1", direction = 1, ...)

scale_colour_stem(palette = "div1", direction = 1, ...)

scale_color_stem(palette = "div1", direction = 1, ...)
```

## Arguments

- palette:

  Name of the palette. See
  [`stem_palette()`](https://stem-cz.github.io/stemtools/reference/stem_palette.md)
  for the full list of valid names. Defaults to `"div1"`.

- direction:

  `1` (default) uses colours in their natural order; `-1` reverses the
  order.

- ...:

  Additional arguments passed to
  [`ggplot2::discrete_scale()`](https://ggplot2.tidyverse.org/reference/discrete_scale.html).

## Value

A ggplot2 `Scale` object to be added to a plot with `+`.

## See also

[`stem_palette()`](https://stem-cz.github.io/stemtools/reference/stem_palette.md),
[`stem_palette_gen()`](https://stem-cz.github.io/stemtools/reference/stem_palette_gen.md),
[`ggplot2::discrete_scale()`](https://ggplot2.tidyverse.org/reference/discrete_scale.html)

## Examples

``` r
ggplot2::ggplot(data = mtcars,
                mapping = ggplot2::aes(x = mpg, y = hp, color = as.factor(am))) +
  ggplot2::geom_point() +
  scale_colour_stem()
```
