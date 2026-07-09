# Summarise numeric or categorical variable (possibly segmented)

Summarise either numeric or categerical variable, possible segment by
another categorical variable. Actually a wrapper for
[`stem_summarise_cat()`](https://stem-cz.github.io/stemtools/reference/stem_summarise_cat.md)
and
[`stem_summarise_num()`](https://stem-cz.github.io/stemtools/reference/stem_summarise_num.md).

## Usage

``` r
stem_summarise(
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
  variable (Only if item is categorical).

- collapse_group:

  Named list. Optionally collapes (or renames) categories of the group
  variable

- na.rm:

  If `TRUE` (default), `NA` values in the item are dropped and
  observations with an `NA` group are excluded. If `FALSE`, `NA` is kept
  (as its own category for categorical items, or yielding an `NA` mean
  for numeric items) and `NA` group values form their own group.

- return_n:

  If `TRUE`, returns absolute group sizes.

## Value

An aggregated data frame with point estimates (either proportions or
means) and 95% confidence intervals.

## Details

Apart from either point estimates (proportions or means), the function
also returns 95% confidence interval bounds. If unweighted, the
intervals are computed using the basic `sqrt((p * (1-p)) / n)` formula,
where `n` is the size of the (possibly segmented) sample. If the
estimated proportions are very high/low, this may lead to interval
estimates outside of the (0;1) bounds. If weights are used, the
confidence intervals come from `surveycore` (Taylor-series
linearization).

If `long = TRUE`, new column is added holding name of the item (and
group) variable. This is useful if you need to loop through multiple
variables and bind the results into a single data frame. See online
vignettes for details.

`collapse_item` and `collapse_group` can be used to collapse categories
of item and group variable. The named list should include vector of old
category names with corresponding to the new category name, e.g.
`list(Agree = c("Definitely Agree", "Rather Agree")`. You can also use
the arguments to rename categories (`list(Yes = "Agree")`) or pass other
arguments from the
[`forcats::fct_collapse()`](https://forcats.tidyverse.org/reference/fct_collapse.html)
function.

If `return_n = TRUE`, columns holding the absolute frequencies will be
added. Column `n` is the number of observations for the specific
combination of item and grouping variable, `item_n` is the frequency of
the item categories and `group_n` is the frequency of the group
categories.

## Examples

``` r
stem_summarise(data = trust, item = government)
#> # A tibble: 5 × 4
#>   government                  freq freq_low freq_upp
#>   <fct>                      <dbl>    <dbl>    <dbl>
#> 1 Definitely Agree           0.008  0.00248   0.0135
#> 2 Rather Agree               0.205  0.180     0.230 
#> 3 Neither Agree nor Disagree 0.39   0.360     0.420 
#> 4 Rather Disagree            0.201  0.176     0.226 
#> 5 Definitely Disagree        0.196  0.171     0.221 

stem_summarise(data = trust, item = age, group = eu_index)
#> # A tibble: 4 × 4
#>   eu_index      mean mean_low mean_upp
#>   <fct>        <dbl>    <dbl>    <dbl>
#> 1 Likes EU      39.2     38.8     39.5
#> 2 Neutral       38.9     38.6     39.3
#> 3 Dislikes EU   38.7     38.3     39.1
#> 4 Doesn't Know  39.7     39.1     40.4

stem_summarise(data = trust, item = government,
               collapse_item = list(Agree = c("Definitely Agree", "Rather Agree")))
#> # A tibble: 4 × 4
#>   government                  freq freq_low freq_upp
#>   <fct>                      <dbl>    <dbl>    <dbl>
#> 1 Agree                      0.213    0.188    0.238
#> 2 Neither Agree nor Disagree 0.39     0.360    0.420
#> 3 Rather Disagree            0.201    0.176    0.226
#> 4 Definitely Disagree        0.196    0.171    0.221
```
