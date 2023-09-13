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
#' @importFrom rlang :=
stem_summarise_cat <- function(data, item, group = NULL, weight = NULL, long = FALSE,
                               collapse_item = NULL, collapse_group = NULL,
                               return_n  = FALSE) {

  weight <- rlang::enquo(weight)

  if(!is.null(collapse_item)) {
    item_var <- dplyr::select(data, {{ item }}) |> dplyr::pull()
    collapse_args <- c(list(item_var), collapse_item)

    data <- data |> dplyr::mutate({{ item }} := do.call(forcats::fct_collapse, collapse_args))
  }

  if(!is.null(collapse_group)) {
    group_var <- dplyr::select(data, {{ group }}) |> dplyr::pull()
    collapse_args <- c(list(group_var), collapse_group)

    data <- data |> dplyr::mutate({{ group }} := do.call(forcats::fct_collapse, collapse_args))
  }

  # Parses weighting variables to be passed in as_survey_design
  # (doesn't support tidy evaluation)
  #weight <- pull(data, {{ weight }})

  # Computes absolute and relative frequencies for (grouped) data and 95% CI.
  # CIs for unweighted data are based on the sqrt( (p - (1 - p)) / n) formula.

  weight_check <- rlang::enquo(weight)

  if(rlang::quo_is_null(weight_check)) {
    counts <- dplyr::count(data, {{ group }}, {{ item }}) |>
      dplyr::group_by({{ group }}) |>
      dplyr::mutate(freq = n / sum(n),
                    se = prop_se(p = freq, n = n),
                    freq_low = freq - 1.96 * se,
                    freq_upp = freq + 1.96 * se) |>
      dplyr::group_by({{ group }}) |>
      dplyr::mutate(group_n = sum(n)) |>
      dplyr::group_by({{ item }}) |>
      dplyr::mutate(item_n = sum(n)) |>
      dplyr::ungroup() |>
      dplyr::relocate(n, group_n, item_n, .after = tidyr::last_col()) |>
      dplyr::select(-se)

  } else {

    weight_name <- names(dplyr::select(data,  !!weight))

    counts <- srvyr::as_survey_design(data, weights = weight_name) |>
      srvyr::group_by({{ group }}, {{ item }}) |>
      srvyr::summarise(freq = srvyr::survey_prop(vartype = "ci", proportion = TRUE),
                       n = dplyr::n()) |>
      dplyr::group_by({{ group }}) |>
      dplyr::mutate(group_n = sum(n)) |>
      dplyr::group_by({{ item }}) |>
      dplyr::mutate(item_n = sum(n)) |>
      dplyr::ungroup()
  }

  if(!return_n) {
    counts <- dplyr::select(counts, -c(n, item_n, group_n))
  }


  # Creates columns containing item (and group) variable name and gives all columns generic names.
  # Useful when the function is called in a loop on multiple variables and the result should be a single data frame.

  group_check <- rlang::enquo(group)

  if(long) {
    counts <- counts |>
      tidyr::pivot_longer(cols = {{ item }},
                          names_to = "item",
                          values_to = "item_cat") |>
      dplyr::relocate(item, item_cat)

    if(!rlang::quo_is_null(group_check)) {
      counts <- counts |>
        tidyr::pivot_longer(cols = {{ group }},
                     names_to = "group",
                     values_to = "group_cat") |>
        dplyr::relocate(group, .before = item) |>
        dplyr::relocate(group_cat, .before = item_cat)
    }

  }

  return(counts)
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
#'
#' @importFrom rlang :=
stem_summarise_num <- function(data, item, group = NULL, weight = NULL, long = FALSE, collapse_group = NULL, return_n = FALSE) {

  # Optionally collapse factor levels in both item and group variables.
  # Because col names are passed unquoted (tidyverse non-standard evaluation),
  # they have to be deparse-substited (Is there a better way?).
  # Because fct_collapse specifies collapsing using dynamic dots, we need to
  # use do.call to allow passing arguments from outside of the function.
  weight <- rlang::enquo(weight)
  group_check <- rlang::enquo(group)

  if(!rlang::quo_is_null(group_check)) {
    group_var <- dplyr::select(data, {{ group }}) |> dplyr::pull()
    collapse_args <- c(list(group_var), collapse_group)

    data <- data |> dplyr::mutate({{ group }} := do.call(forcats::fct_collapse, collapse_args))
  }

  weight_check <- rlang::enquo(weight)

  if(rlang::quo_is_null(weight_check)) {
    means <- data |>
      dplyr::group_by({{ group }}) |>
      dplyr::summarise(group_n = dplyr::n(),
                       item_n = dplyr::n(),
                       n = group_n,
                se = se_mean({{ item }}),
                mean = mean({{ item }}, na.rm = TRUE)) |>
      dplyr::group_by({{ group }}) |>
      dplyr::mutate(mean_low = mean - 1.96 * se,
             mean_upp = mean + 1.96 * se) |>
      dplyr::select(-se) |>
      dplyr::ungroup()
  } else {
    weight_name <- names(dplyr::select(data,  !!weight ))

    means <- data |>
      srvyr::as_survey_design(weights = weight_name) |>
      srvyr::group_by({{ group }}) |>
      srvyr::summarise(group_n = dplyr::n(),
                       group_n = dplyr::n(),
                       n = group_n,
                mean = srvyr::survey_mean({{ item }}, vartype = "ci"))
  }

  if(!return_n) {
    means <- dplyr::select(means, -n)
  }


  # Creates columns containing item (and group) variable name and gives all columns generic names.
  # Useful when the function is called in a loop on multiple variables and the result should be a single data frame.

  group_check <- rlang::enquo(group)

  if(long) {

    item_name <- dplyr::select(data, {{ item }}) |> names()

    means <- means |>
      dplyr::mutate(item = item_name) |>
      dplyr::relocate(item)

    if(!rlang::quo_is_null(group_check)) {
      means <- means |>
        tidyr::pivot_longer(cols = {{ group }},
                     names_to = "group",
                     values_to = "group_cat") |>
        dplyr::relocate(group, group_cat) |>
        dplyr::relocate(item, .after = group)
    }
  }

  return(means)
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
#'If unweighted, the intervals are computed using the basic `sqrt((p * (1-p)) / n)` formula. If the estimated proportions
#'are very high/low, this may lead to interval estimates outsides of the (0;1) bounds. If weights are used, the confidence
#'intervals are based on weighted logistic regression. Neither of the approaches will work if the proportions are exactly zero or one.
#'
#'If `long = TRUE`, new column is added holding name of the item (and group) variable. This is useful if you need to loop through
#'multiple variables and bind the results into a single data frame. See online vignettes for details.
#'
#'`collapse_item` and `collapse_group` can be used to collapse categories of item and group variable. The named list should include
#'vector of old category names with corresponding to the new category name, e.g. `list(Agree = c("Definitely Agree", "Rather Agree")`.
#'You can also use the arguments to rename categories (`list(Yes = "Agree")`) or pass other arguments from the [forcats::fct_collapse()] function.
#'
#'If `return_n = TRUE`, columns holding the absolute frequencies will be added. Column `n` is the number of observations for the specific combination of
#'item and grouping variable, `n_item` is the frequency of the item categories and `n_group` is the frequency of the group categories.
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
#'
#' @importFrom rlang :=
stem_summarise <- function(data, item, group = NULL, weight = NULL, long = FALSE,
                           collapse_item = NULL, collapse_group = NULL,
                           return_n  = FALSE) {
  #weight <- rlang::enquo(weight) # evaluating of weight has to wait until the specific stem_summarise_* function.
  item_class <- dplyr::select(data, {{ item}})
  item_class <- names(item_class)
  item_class <- class(data[[item_class]])

  # Right now supports either character/factor or numeric item classes.
  if(item_class %in% c("character", "factor")) {
    summ <- stem_summarise_cat(data = data, item = {{ item }}, group = {{ group }} , weight = {{ weight }}, long = long,
                               collapse_item = collapse_item, collapse_group = collapse_group,
                               return_n  = return_n)
  } else if(item_class == "numeric") {
    summ <- stem_summarise_num(data = data, item = {{ item }}, group = {{ group }}, weight = {{ weight }}, long = long,
                               collapse_group = collapse_group, return_n = return_n)
  } else stop(paste0("Item has to be character/factor or numeric, not ", item_class))

  return(summ)
}
