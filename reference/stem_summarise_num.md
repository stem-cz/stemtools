# Summarise numeric variable using arithmetic mean (possibly segmented)

Takes one numeric variable and returns a tidy dataframe, including
relative mean and absolute size of each group. The variable can be
further segmented by another categorical variable. Can also take survey
weights. Returns 95% confidence intervals.

## Usage

``` r
stem_summarise_num(
  data,
  item,
  group = NULL,
  weight = NULL,
  long = FALSE,
  collapse_group = NULL,
  na.rm = TRUE,
  return_n = FALSE
)
```

## Arguments

- data:

  Dataframe including data to be analyzed

- item:

  Variable to be summarised

- group:

  Optional segmenting variable

- weight:

  Optional survey weights

- long:

  Returns data in long format. Useful if multiple dataframes are to be
  merged

- collapse_group:

  Named list. Optionally collapses (or renames) categories of the group
  variable

- na.rm:

  If `TRUE` (default), `NA` values in the item are dropped from the
  calculations and observations with an `NA` group are excluded. If
  `FALSE`, `NA` item values are kept (so the mean is `NA`) and `NA`
  group values form their own group.

- return_n:

  If `TRUE`, returns absolute group sizes.

## Value

Tidy dataframe

## Examples

``` r
trust |> stem_summarise_num(age,
                            group = eu_index,
                            weight = W,
         collapse_group = list(`Neutral or no opinion` = c("Neutral", "Doesn't Know")))
#> # A tibble: 3 × 4
#>   eu_index               mean mean_low mean_upp
#>   <fct>                 <dbl>    <dbl>    <dbl>
#> 1 Likes EU               39.2     38.7     39.6
#> 2 Neutral or no opinion  39.0     38.5     39.4
#> 3 Dislikes EU            38.2     37.5     38.9
```
