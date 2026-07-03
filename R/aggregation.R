# stemtools aggregation ---------------------------------------------------
#
# Weighted or unweighted summaries of categorical and numeric variables.
# All three exported functions share the same output contract so their
# results can be consumed interchangeably (e.g. by the plotting functions in
# R/plots.R):
#
#   * `freq`/`mean` .............. point estimate
#   * `freq_low`/`freq_upp` ...... 95% confidence interval (proportions)
#   * `mean_low`/`mean_upp` ...... 95% confidence interval (means)
#   * `n`, `group_n`, `item_n` ... absolute sizes (only when `return_n = TRUE`)
#
# Unweighted intervals use the parametric formulas in R/utils.R; weighted
# intervals come from srvyr/survey.

#' Summarise categorical variable (possibly segmented)
#'
#' Takes one categorical variable and returns a tidy dataframe, including relative frequencies and
#' absolute size of each group. The variable can be further segmented by another categorical variable.
#' Can also take survey weights. Returns 95% confidence intervals.
#'
#'
#' @param data Dataframe including variables to be analyzed
#' @param item Variable to be summarised
#' @param group Optional segmenting variable
#' @param weight Optional survey weights
#' @param long Returns data in long format. Useful if multiple dataframes are to be merged
#' @param collapse_item Named list. Optionally collapes (or renames) categories of the item variable
#' @param collapse_group Named list. Optionally collapes (or renames) categories of the group variable
#' @param return_n If `TRUE`, returns absolute group sizes.
#'
#' @return Tidy dataframe
#' @export
#'
#' @examples
#' trust |> stem_summarise_cat(item = police,
#'                             group = eu,
#'                             weight = W,
#'                             collapse_item = list(Agree = c("Definitely Agree", "Rather Agree")))
stem_summarise_cat <- function(data, item, group = NULL, weight = NULL, long = FALSE,
                               collapse_item = NULL, collapse_group = NULL,
                               return_n = FALSE) {

  has_weight <- !rlang::quo_is_null(rlang::enquo(weight))
  has_group  <- !rlang::quo_is_null(rlang::enquo(group))

  if (!is.null(collapse_item))  data <- collapse_cats(data, {{ item }},  collapse_item)
  if (!is.null(collapse_group)) data <- collapse_cats(data, {{ group }}, collapse_group)

  # Point estimates and 95% CI (within group, if a group is supplied).
  if (has_weight) {
    counts <- data |>
      srvyr::as_survey_design(weights = {{ weight }}) |>
      srvyr::group_by({{ group }}, {{ item }}) |>
      srvyr::summarise(
        freq = srvyr::survey_prop(vartype = "ci", proportion = TRUE),
        n    = dplyr::n()
      ) |>
      dplyr::ungroup()
  } else {
    z <- stats::qnorm(0.975)
    counts <- data |>
      dplyr::count({{ group }}, {{ item }}) |>
      dplyr::mutate(
        freq     = n / sum(n),
        freq_low = freq - z * se_prop(freq, sum(n)),
        freq_upp = freq + z * se_prop(freq, sum(n)),
        .by      = {{ group }}
      )
  }

  # Absolute sizes, shared by both branches.
  counts <- counts |>
    dplyr::mutate(group_n = sum(n), .by = {{ group }}) |>
    dplyr::mutate(item_n  = sum(n), .by = {{ item }}) |>
    dplyr::relocate(n, group_n, item_n, .after = tidyr::last_col())

  if (!return_n) counts <- dplyr::select(counts, -c(n, group_n, item_n))

  # Optionally reshape to long format so several items can be row-bound together.
  # The item (and group) variable name is moved into a dedicated column.
  if (long) {
    counts <- counts |>
      tidyr::pivot_longer({{ item }}, names_to = "item", values_to = "item_cat") |>
      dplyr::relocate(item, item_cat)

    if (has_group) {
      counts <- counts |>
        tidyr::pivot_longer({{ group }}, names_to = "group", values_to = "group_cat") |>
        dplyr::relocate(group, group_cat, .before = item)
    }
  }

  counts
}

#' Summarise numeric variable using arithmetic mean (possibly segmented)
#'
#' Takes one numeric variable and returns a tidy dataframe, including relative mean and
#' absolute size of each group. The variable can be further segmented by another categorical variable.
#' Can also take survey weights. Returns 95% confidence intervals.
#'
#' @param data Dataframe including data to be analyzed
#' @param item Variable to be summarised
#' @param group Optional segmenting variable
#' @param weight Optional survey weights
#' @param long Returns data in long format. Useful if multiple dataframes are to be merged
#' @param collapse_group Named list. Optionally collapses (or renames) categories of the group variable
#' @param return_n If `TRUE`, returns absolute group sizes.
#'
#' @return Tidy dataframe
#' @export
#'
#' @examples
#' trust |> stem_summarise_num(age,
#'                             group = eu_index,
#'                             weight = W,
#'          collapse_group = list(`Neutral or no opinion` = c("Neutral", "Doesn't Know")))
stem_summarise_num <- function(data, item, group = NULL, weight = NULL, long = FALSE,
                               collapse_group = NULL, return_n = FALSE) {

  has_weight <- !rlang::quo_is_null(rlang::enquo(weight))
  has_group  <- !rlang::quo_is_null(rlang::enquo(group))

  if (has_group && !is.null(collapse_group)) {
    data <- collapse_cats(data, {{ group }}, collapse_group)
  }

  if (has_weight) {
    means <- data |>
      srvyr::as_survey_design(weights = {{ weight }}) |>
      srvyr::group_by({{ group }}) |>
      srvyr::summarise(
        n    = dplyr::n(),
        mean = srvyr::survey_mean({{ item }}, vartype = "ci")
      ) |>
      dplyr::ungroup()
  } else {
    z <- stats::qnorm(0.975)
    means <- data |>
      dplyr::summarise(
        n    = dplyr::n(),
        mean = mean({{ item }}, na.rm = TRUE),
        .se  = se_mean({{ item }}),
        .by  = {{ group }}
      ) |>
      dplyr::mutate(
        mean_low = mean - z * .se,
        mean_upp = mean + z * .se
      ) |>
      dplyr::select(-".se")
  }

  # For a numeric summary every size is the group size.
  means <- means |>
    dplyr::mutate(group_n = n, item_n = n) |>
    dplyr::relocate(n, group_n, item_n, .after = tidyr::last_col())

  if (!return_n) means <- dplyr::select(means, -c(n, group_n, item_n))

  if (long) {
    means <- means |>
      dplyr::mutate(item = names(dplyr::select(data, {{ item }}))) |>
      dplyr::relocate(item)

    if (has_group) {
      means <- means |>
        tidyr::pivot_longer({{ group }}, names_to = "group", values_to = "group_cat") |>
        dplyr::relocate(group, group_cat, .before = item)
    }
  }

  means
}

#' Summarise numeric or categorical variable (possibly segmented)
#'
#' Summarise either numeric or categerical variable, possible segment by another categorical variable.
#' Actually a wrapper for [stemtools::stem_summarise_cat()] and [stemtools::stem_summarise_num()].
#'
#' @param data Dataframe including variables to be analyzed
#' @param item Variable to be summarised
#' @param group Optional segmenting variable
#' @param weight Optional survey weights
#' @param long Returns data in long format. Useful if multiple dataframes are to be merged
#' @param collapse_item Named list. Optionally collapes (or renames) categories of the item variable (Only if item is categorical).
#' @param collapse_group Named list. Optionally collapes (or renames) categories of the group variable
#' @param return_n If `TRUE`, returns absolute group sizes.
#'
#'@details
#'Apart from either point estimates (proportions or means), the function also returns 95% confidence interval bounds.
#'If unweighted, the intervals are computed using the basic `sqrt((p * (1-p)) / n)` formula, where `n` is the size of the
#'(possibly segmented) sample. If the estimated proportions are very high/low, this may lead to interval estimates outside
#'of the (0;1) bounds. If weights are used, the confidence intervals are based on weighted logistic regression. Neither of
#'the approaches will work if the proportions are exactly zero or one.
#'
#'If `long = TRUE`, new column is added holding name of the item (and group) variable. This is useful if you need to loop through
#'multiple variables and bind the results into a single data frame. See online vignettes for details.
#'
#'`collapse_item` and `collapse_group` can be used to collapse categories of item and group variable. The named list should include
#'vector of old category names with corresponding to the new category name, e.g. `list(Agree = c("Definitely Agree", "Rather Agree")`.
#'You can also use the arguments to rename categories (`list(Yes = "Agree")`) or pass other arguments from the [forcats::fct_collapse()] function.
#'
#'If `return_n = TRUE`, columns holding the absolute frequencies will be added. Column `n` is the number of observations for the specific combination of
#'item and grouping variable, `item_n` is the frequency of the item categories and `group_n` is the frequency of the group categories.
#'
#' @return An aggregated data frame with point estimates (either proportions or means) and 95% confidence intervals.
#' @export
#'
#' @examples
#' stem_summarise(data = trust, item = government)
#'
#' stem_summarise(data = trust, item = age, group = eu_index)
#'
#' stem_summarise(data = trust, item = government,
#'                collapse_item = list(Agree = c("Definitely Agree", "Rather Agree")))
stem_summarise <- function(data, item, group = NULL, weight = NULL, long = FALSE,
                           collapse_item = NULL, collapse_group = NULL,
                           return_n = FALSE) {

  item_vec <- data[[names(dplyr::select(data, {{ item }}))]]

  if (inherits(item_vec, c("factor", "character"))) {
    stem_summarise_cat(data, item = {{ item }}, group = {{ group }}, weight = {{ weight }}, long = long,
                       collapse_item = collapse_item, collapse_group = collapse_group,
                       return_n = return_n)
  } else if (is.numeric(item_vec)) {
    stem_summarise_num(data, item = {{ item }}, group = {{ group }}, weight = {{ weight }}, long = long,
                       collapse_group = collapse_group, return_n = return_n)
  } else {
    stop("`item` must be a factor, character or numeric variable, not ",
         paste(class(item_vec), collapse = "/"), ".", call. = FALSE)
  }
}
