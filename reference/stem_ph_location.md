# Where to place a chart on a slide

Internal helper turning the size arguments of the PowerPoint helpers
into an officer location. Falls back to the layout's body placeholder
when no size is given, so a template's own geometry is respected.

## Usage

``` r
stem_ph_location(
  width = NULL,
  height = NULL,
  left = NULL,
  top = NULL,
  units = "cm"
)
```

## Arguments

- width, height:

  Size of the chart, in `units`. `NULL` uses the body placeholder of the
  slide layout.

- left, top:

  Position of the chart's top-left corner, in `units`.

- units:

  Unit of `width`, `height`, `left` and `top`.

## Value

An officer location object.
