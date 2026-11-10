# Label colour that contrasts with a fill

Internal helper. A fill whose Rec. 601 luma
(`0.299 R + 0.587 G + 0.114 B`, on a 0-255 scale) is below `threshold`
counts as dark and gets a white label; lighter fills get `light`. With
the threshold of 150 the Stem diverging palettes keep their labels
(white on the dark extremes, dark on the light middle colours).

## Usage

``` r
contrast_label_color(fill, light = "black", threshold = 150)
```

## Arguments

- fill:

  Character vector of fill colours.

- light:

  Label colour used on light fills.

- threshold:

  Luma below which a fill counts as dark.

## Value

Character vector of label colours.
