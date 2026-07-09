# Summarise categorical variable (possibly segmented)

Takes one categorical variable and returns a tidy dataframe, including
relative frequencies and absolute size of each group. The variable can
be further segmented by another categorical variable. Can also take
survey weights. Returns 95% confidence intervals.

## Usage

``` r
stem_summarise_cat(
  data,
  item,
  group = NULL,
  weight = NULL,
  long = FALSE,
  collapse_item = NULL,
  collapse_group = NULL,
  na.rm = TRUE,
  return_n = FALSE
)
```

## Arguments

- data:

  Dataframe including variables to be analyzed

- item:

  Variable to be summarised

- group:

  Optional segmenting variable

- weight:

  Optional survey weights

- long:

  Returns data in long format. Useful if multiple dataframes are to be
  merged

- collapse_item:

  Named list. Optionally collapes (or renames) categories of the item
  variable

- collapse_group:

  Named list. Optionally collapes (or renames) categories of the group
  variable

- na.rm:

  If `TRUE` (default), `NA` values in the item are dropped and
  observations with an `NA` group are excluded. If `FALSE`, `NA` is kept
  as its own item category (inflating the denominator so proportions
  still sum to 1) and `NA` group values form their own group.

- return_n:

  If `TRUE`, returns absolute group sizes.

## Value

Tidy dataframe

## Examples

``` r
trust |> stem_summarise_cat(item = police,
                            group = eu,
                            weight = W,
                            collapse_item = list(Agree = c("Definitely Agree", "Rather Agree")))
#> # A tibble: 20 × 5
#>    eu                         police                      freq freq_low freq_upp
#>    <fct>                      <fct>                      <dbl>    <dbl>    <dbl>
#>  1 Definitely Agree           Agree                     0.733   6.21e-1   0.846 
#>  2 Definitely Agree           Neither Agree nor Disagr… 0.104   3.05e-2   0.177 
#>  3 Definitely Agree           Rather Disagree           0.0846  1.99e-2   0.149 
#>  4 Definitely Agree           Definitely Disagree       0.0783 -5.20e-4   0.157 
#>  5 Rather Agree               Agree                     0.702   6.06e-1   0.799 
#>  6 Rather Agree               Neither Agree nor Disagr… 0.197   1.14e-1   0.279 
#>  7 Rather Agree               Rather Disagree           0.0320  9.87e-3   0.0540
#>  8 Rather Agree               Definitely Disagree       0.0688  4.01e-3   0.134 
#>  9 Neither Agree nor Disagree Agree                     0.720   6.56e-1   0.784 
#> 10 Neither Agree nor Disagree Neither Agree nor Disagr… 0.152   1.03e-1   0.201 
#> 11 Neither Agree nor Disagree Rather Disagree           0.0720  3.26e-2   0.111 
#> 12 Neither Agree nor Disagree Definitely Disagree       0.0555  2.22e-2   0.0888
#> 13 Rather Disagree            Agree                     0.750   6.71e-1   0.829 
#> 14 Rather Disagree            Neither Agree nor Disagr… 0.146   8.00e-2   0.211 
#> 15 Rather Disagree            Rather Disagree           0.0436  9.52e-3   0.0777
#> 16 Rather Disagree            Definitely Disagree       0.0606  1.71e-2   0.104 
#> 17 Definitely Disagree        Agree                     0.685   5.54e-1   0.816 
#> 18 Definitely Disagree        Neither Agree nor Disagr… 0.240   1.18e-1   0.362 
#> 19 Definitely Disagree        Rather Disagree           0.0163 -1.55e-2   0.0481
#> 20 Definitely Disagree        Definitely Disagree       0.0585 -7.77e-3   0.125 
```
