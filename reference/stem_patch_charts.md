# Patch the charts of an Office file

Internal helper rewriting the chart parts of a written `.pptx`/`.docx`
for the bits of the Stem look that mschart exposes no setting for: it
applies
[`stem_leftalign_titles()`](https://stem-cz.github.io/stemtools/reference/stem_leftalign_titles.md)
and
[`stem_box_legends()`](https://stem-cz.github.io/stemtools/reference/stem_box_legends.md)
to every chart XML.

## Usage

``` r
stem_patch_charts(path)
```

## Arguments

- path:

  Path of the Office file to patch.

## Value

The path, invisibly.

## Details

Silently leaves the file untouched when the `zip` package is missing or
the file cannot be rewritten: the patches are cosmetic and never worth
failing an export over.
