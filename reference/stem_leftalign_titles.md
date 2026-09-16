# Left-align the titles of the charts in an Office file

Internal helper patching the chart parts of a written `.pptx`/`.docx`.
Office centres a chart title, whereas
[`theme_stem()`](https://stem-cz.github.io/stemtools/reference/theme_stem.md)
sets `plot.title.position = "plot"`, which flushes it left above the
whole plot. mschart exposes no alignment setting, so the paragraph
property is written into the chart XML afterwards, as the Stem apps do.

## Usage

``` r
stem_leftalign_titles(path)
```

## Arguments

- path:

  Path of the Office file to patch.

## Value

The path, invisibly.

## Details

Silently leaves the file untouched when the `zip` package is missing or
the file cannot be rewritten: the alignment is cosmetic and never worth
failing an export over.
