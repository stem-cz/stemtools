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
                               return_n  = FALSE) {

  if(!is.null(collapse_item)) {
    collapse_args <- c(list(data[[deparse(substitute(item))]]), collapse_item)

    data[deparse(substitute(item))] <- do.call(forcats::fct_collapse, collapse_args)
  }

  if(!is.null(collapse_group)) {
    collapse_args <- c(list(data[[deparse(substitute(group))]]), collapse_group)

    data[deparse(substitute(group))] <- do.call(forcats::fct_collapse, collapse_args)
  }

  # Parses weighting variables to be passed in as_survey_design
  # (doesn't support tidy evaluation)
  #weight <- pull(data, {{ weight }})

  # Computes absolute and relative frequencies for (grouped) data and 95% CI.
  # CIs for unweighted data are based on the sqrt( (p - (1 - p)) / n) formula.

  weight_check <- deparse(substitute(weight))

  if(weight_check == "NULL") {
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

    weight_name <- names(dplyr::select(data, {{ weight }}))

    counts <- srvyr::as_survey_design(data, weights = weight_name) |>
      srvyr::group_by({{ group }}, {{ item }}) |>
      srvyr::summarise(freq = srvyr::survey_prop(vartype = "ci"),
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
  if(long) {
    counts <- counts |>
      tidyr::pivot_longer(cols = {{ item }},
                          names_to = "item",
                          values_to = "item_cat") |>
      dplyr::relocate(item, item_cat)

    if(deparse(substitute(group)) != "NULL") {
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
stem_summarise_num <- function(data, item, group = NULL, weight = NULL, long = FALSE, collapse_group = NULL, return_n = FALSE) {

  # Optionally collapse factor levels in both item and group variables.
  # Because col names are passed unquoted (tidyverse non-standard evaluation),
  # they have to be deparse-substited (Is there a better way?).
  # Because fct_collapse specifies collapsing using dynamic dots, we need to
  # use do.call to allow passing arguments from outside of the function.
  if(!is.null(collapse_group)) {
    collapse_args <- c(list(data[[deparse(substitute(group))]]),
                       collapse_group)

    data[desub(group)] <- do.call(forcats::fct_collapse, collapse_args)
  }

  # Parses weighting variables to be passed in as_survey_design
  # (doesn't support tidy evaluation)

  weight_check <- deparse(substitute(weight))

  if(weight_check == "NULL") {
    means <- data |>
      dplyr::group_by({{ group }}) |>
      dplyr::summarise(n = dplyr::n(),
                se = se_mean({{ item }}),
                mean = mean({{ item }}, na.rm = TRUE)) |>
      dplyr::group_by({{ group }}) |>
      dplyr::mutate(mean_low = mean - 1.96 * se,
             mean_upp = mean + 1.96 * se) |>
      dplyr::select(-se) |>
      dplyr::ungroup()
  } else {
    weight_name <- names(dplyr::select(data, {{ weight }}))

    means <- data |>
      srvyr::as_survey_design(weights = weight_name) |>
      srvyr::group_by({{ group }}) |>
      srvyr::summarise(n = dplyr::n(),
                mean = srvyr::survey_mean({{ item }}, vartype = "ci"))
  }

  if(!return_n) {
    means <- dplyr::select(means, -n)
  }


  # Creates columns containing item (and group) variable name and gives all columns generic names.
  # Useful when the function is called in a loop on multiple variables and the result should be a single data frame.
  if(long) {

    item_name <- deparse(substitute(item))

    means <- means |>
      dplyr::mutate(item = item_name) |>
      dplyr::relocate(item)

    if(deparse(substitute(group)) != "NULL") {
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

