# stemtools aggregation ---------------------------------------------------
#
# Weighted or unweighted summaries of categorical and numeric variables.
# All three exported functions share the same output contract so their
# results can be consumed interchangeably (e.g. by the plotting functions in
# R/plots.R):
#
#   * `freq`/`mean` ................ point estimate
#   * `freq_low`/`freq_upp` ........ confidence interval (proportions)
#   * `mean_low`/`mean_upp` ........ confidence interval (means)
#   * `<est>_se`, `_var`, `_cv`,
#     `_moe`, `_deff` .............. further uncertainty measures, on request
#   * `n`, `group_n`, `item_n` ..... absolute sizes (only when `return_n = TRUE`)
#   * `n_weighted` ................. sum of weights (only when `n_weighted = TRUE`)
#
# Both code paths only produce the point estimate, its standard error (`.se`)
# and design effect (`.deff`); `add_uncertainty()` derives every requested
# measure from those, so weighted and unweighted output is computed alike.
# Unweighted standard errors use the parametric formulas in R/utils.R; weighted
# ones come from srvyr / survey (Taylor-series linearization).

# Uncertainty measures accepted by the `variance` argument, in output order.
variance_types <- c("se", "var", "cv", "ci", "moe", "deff")

check_variance <- function(variance, conf_level) {
  bad <- setdiff(variance, variance_types)
  if (length(bad) > 0) {
    stop("`variance` must be one or more of ",
         paste0("\"", variance_types, "\"", collapse = ", "),
         ", not ", paste0("\"", bad, "\"", collapse = ", "), ".", call. = FALSE)
  }
  if (!is.numeric(conf_level) || length(conf_level) != 1 ||
      conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be a single number between 0 and 1.", call. = FALSE)
  }
}

# Replace the internal `.se` / `.deff` columns by the measures requested in
# `variance`, named after the estimate column `est` (e.g. `freq_se`).
add_uncertainty <- function(df, est, variance, conf_level) {
  x   <- df[[est]]
  se  <- df[[".se"]]
  moe <- stats::qnorm(1 - (1 - conf_level) / 2) * se

  if ("se"   %in% variance) df[[paste0(est, "_se")]]   <- se
  if ("var"  %in% variance) df[[paste0(est, "_var")]]  <- se^2
  if ("cv"   %in% variance) df[[paste0(est, "_cv")]]   <- se / x
  if ("ci"   %in% variance) {
    df[[paste0(est, "_low")]] <- x - moe
    df[[paste0(est, "_upp")]] <- x + moe
  }
  if ("moe"  %in% variance) df[[paste0(est, "_moe")]]  <- moe
  if ("deff" %in% variance) df[[paste0(est, "_deff")]] <- df[[".deff"]]

  dplyr::select(df, -dplyr::any_of(c(".se", ".deff")))
}

# Names of the haven-labelled columns (e.g. from `haven::read_sav()`) among
# `cols`. haven is only suggested: labelled data can't exist without it, so it
# is required only once such a column turns up.
labelled_cols <- function(data, cols) {
  is_labelled <- function(x) inherits(x, "haven_labelled")
  labelled <- names(dplyr::select(data, ({{ cols }}) & dplyr::where(is_labelled)))
  if (length(labelled) > 0) rlang::check_installed("haven", "to handle value labels.")
  labelled
}

# Convert haven-labelled columns among `cols` to factors, using the value
# labels as levels. Other columns are untouched.
label_values <- function(data, cols) {
  dplyr::mutate(
    data,
    dplyr::across(dplyr::all_of(labelled_cols(data, {{ cols }})), haven::as_factor)
  )
}

# Warn when any estimate rests on fewer than `min_cell_n` unweighted cases.
warn_small_cells <- function(n, min_cell_n) {
  if (is.null(min_cell_n)) return(invisible())

  n_small <- sum(n < min_cell_n, na.rm = TRUE)
  if (n_small > 0) {
    rlang::warn(
      c(sprintf("%d %s fewer than %d unweighted observations.",
                n_small, if (n_small == 1) "cell has" else "cells have", min_cell_n),
        i = "Estimates in these cells may be unreliable for reporting.",
        i = "Set `min_cell_n = NULL` to silence this warning."),
      class = "stemtools_warning_small_cell"
    )
  }
}

#' Summarise categorical variable (possibly segmented)
#'
#' Takes one categorical variable and returns a tidy dataframe, including relative frequencies and
#' absolute size of each group. The variable can be further segmented by another categorical variable.
#' Can also take survey weights. Returns confidence intervals and, on request, other uncertainty measures.
#'
#'
#' @param data Dataframe including variables to be analyzed
#' @param item Variable to be summarised
#' @param group Optional segmenting variable
#' @param weight Optional survey weights
#' @param long Returns data in long format. Useful if multiple dataframes are to be merged
#' @param collapse_item Named list. Optionally collapes (or renames) categories of the item variable
#' @param collapse_group Named list. Optionally collapes (or renames) categories of the group variable
#' @param na.rm If `TRUE` (default), `NA` values in the item are dropped and observations with an
#'   `NA` group are excluded. If `FALSE`, `NA` is kept as its own item category (inflating the
#'   denominator so proportions still sum to 1) and `NA` group values form their own group.
#' @param return_n If `TRUE`, returns absolute group sizes.
#' @param variance Uncertainty measures to return, any of `"se"` (standard error), `"var"`
#'   (variance), `"cv"` (coefficient of variation), `"ci"` (confidence interval), `"moe"` (margin of
#'   error) and `"deff"` (design effect). Defaults to `"ci"`; `NULL` returns point estimates only.
#'   Columns are named after the estimate, e.g. `freq_se`, `freq_low`/`freq_upp`, `freq_deff`.
#' @param conf_level Confidence level used by `"ci"` and `"moe"`. Defaults to `0.95`.
#' @param n_weighted If `TRUE`, adds an `n_weighted` column with the sum of weights in each cell
#'   (the estimated population count). Equals `n` for unweighted data.
#' @param min_cell_n Warn when any cell has fewer than this many unweighted observations. Defaults
#'   to `30`; `NULL` disables the warning.
#'
#' @return Tidy dataframe
#' @export
#'
#' @examples
#' trust |> stem_summarise_cat(item = police,
#'                             group = eu,
#'                             weight = W,
#'                             collapse_item = list(Agree = c("Definitely Agree", "Rather Agree")))
#'
#' trust |> stem_summarise_cat(item = police, weight = W,
#'                             variance = c("se", "ci", "deff"), n_weighted = TRUE)
stem_summarise_cat <- function(data, item, group = NULL, weight = NULL, long = FALSE,
                               collapse_item = NULL, collapse_group = NULL,
                               na.rm = TRUE, return_n = FALSE,
                               variance = "ci", conf_level = 0.95,
                               n_weighted = FALSE, min_cell_n = 30) {

  check_variance(variance, conf_level)
  has_weight <- !rlang::quo_is_null(rlang::enquo(weight))
  has_group  <- !rlang::quo_is_null(rlang::enquo(group))

  # Value labels first, so `collapse_*` can refer to the labels.
  data <- label_values(data, c({{ item }}, {{ group }}))
  if (!is.null(collapse_item))  data <- collapse_cats(data, {{ item }},  collapse_item)
  if (!is.null(collapse_group)) data <- collapse_cats(data, {{ group }}, collapse_group)

  # Point estimates and their SE (within group, if a group is supplied).
  if (has_weight) {
    design <- srvyr::as_survey_design(data, weights = {{ weight }})

    # Filtering the design (not the data) keeps the full sample for variance
    # estimation, i.e. the remaining cases are treated as a domain.
    if (na.rm) {
      design <- dplyr::filter(design, !is.na({{ item }}))
      if (has_group) design <- dplyr::filter(design, !is.na({{ group }}))
    }

    counts <- design |>
      dplyr::group_by({{ group }}, {{ item }}) |>
      dplyr::summarise(
        freq       = srvyr::survey_prop(vartype = "se", proportion = FALSE,
                                        deff = if ("deff" %in% variance) "replace" else FALSE),
        n          = srvyr::unweighted(dplyr::n()),
        n_weighted = srvyr::survey_total(vartype = NULL),
        .groups    = "drop"
      ) |>
      dplyr::rename(dplyr::any_of(c(.se = "freq_se", .deff = "freq_deff"))) |>
      tibble::as_tibble()
  } else {
    # Same NA handling as the weighted path: drop NA item (and NA group) when
    # na.rm, otherwise dplyr::count() keeps NA as its own category / group.
    if (na.rm) {
      data <- dplyr::filter(data, !is.na({{ item }}))
      if (has_group) data <- dplyr::filter(data, !is.na({{ group }}))
    }

    counts <- data |>
      dplyr::count({{ group }}, {{ item }}) |>
      dplyr::mutate(
        freq  = n / sum(n),
        .se   = se_prop(freq, sum(n)),
        .deff = 1,
        .by   = {{ group }}
      ) |>
      dplyr::mutate(n_weighted = n) |>
      tibble::as_tibble()
  }

  counts <- add_uncertainty(counts, "freq", variance, conf_level)
  warn_small_cells(counts$n, min_cell_n)

  # Absolute sizes, shared by both branches.
  counts <- counts |>
    dplyr::mutate(group_n = sum(n), .by = {{ group }}) |>
    dplyr::mutate(item_n  = sum(n), .by = {{ item }}) |>
    dplyr::relocate(n, group_n, item_n, n_weighted, .after = tidyr::last_col())

  if (!return_n)   counts <- dplyr::select(counts, -c(n, group_n, item_n))
  if (!n_weighted) counts <- dplyr::select(counts, -n_weighted)

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
#' Can also take survey weights. Returns confidence intervals and, on request, other uncertainty measures.
#'
#' @inheritParams stem_summarise_cat
#' @param data Dataframe including data to be analyzed
#' @param collapse_group Named list. Optionally collapses (or renames) categories of the group variable
#' @param na.rm If `TRUE` (default), `NA` values in the item are dropped from the calculations and
#'   observations with an `NA` group are excluded. If `FALSE`, `NA` item values are kept (so the
#'   mean is `NA`) and `NA` group values form their own group.
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
                               collapse_group = NULL, na.rm = TRUE, return_n = FALSE,
                               variance = "ci", conf_level = 0.95,
                               n_weighted = FALSE, min_cell_n = 30) {

  check_variance(variance, conf_level)
  has_weight <- !rlang::quo_is_null(rlang::enquo(weight))
  has_group  <- !rlang::quo_is_null(rlang::enquo(group))

  # A labelled numeric item is averaged on its codes; a labelled group is
  # shown by its labels.
  data <- dplyr::mutate(
    data,
    dplyr::across(dplyr::all_of(labelled_cols(data, {{ item }})), haven::zap_labels)
  )
  data <- label_values(data, {{ group }})
  if (has_group && !is.null(collapse_group)) {
    data <- collapse_cats(data, {{ group }}, collapse_group)
  }

  if (has_weight) {
    design <- srvyr::as_survey_design(data, weights = {{ weight }})

    # When na.rm, drop NA items (so `n` reflects the non-NA basis) and NA
    # groups as a domain of the full design; otherwise keep them.
    if (na.rm) {
      design <- dplyr::filter(design, !is.na({{ item }}))
      if (has_group) design <- dplyr::filter(design, !is.na({{ group }}))
    }

    means <- design |>
      dplyr::group_by({{ group }}) |>
      dplyr::summarise(
        mean       = srvyr::survey_mean({{ item }}, vartype = "se", na.rm = na.rm,
                                        deff = if ("deff" %in% variance) "replace" else FALSE),
        n          = srvyr::unweighted(dplyr::n()),
        n_weighted = srvyr::survey_total(vartype = NULL),
        .groups    = "drop"
      ) |>
      dplyr::rename(dplyr::any_of(c(.se = "mean_se", .deff = "mean_deff"))) |>
      tibble::as_tibble()
  } else {
    # Same NA handling as the weighted path.
    if (na.rm) {
      data <- dplyr::filter(data, !is.na({{ item }}))
      if (has_group) data <- dplyr::filter(data, !is.na({{ group }}))
    }

    means <- data |>
      dplyr::summarise(
        mean  = mean({{ item }}, na.rm = na.rm),
        .se   = se_mean({{ item }}),
        .deff = 1,
        n     = dplyr::n(),
        .by   = {{ group }}
      ) |>
      dplyr::mutate(n_weighted = n) |>
      tibble::as_tibble()
  }

  means <- add_uncertainty(means, "mean", variance, conf_level)
  warn_small_cells(means$n, min_cell_n)

  # For a numeric summary every size is the group size.
  means <- means |>
    dplyr::mutate(group_n = n, item_n = n) |>
    dplyr::relocate(n, group_n, item_n, n_weighted, .after = tidyr::last_col())

  if (!return_n)   means <- dplyr::select(means, -c(n, group_n, item_n))
  if (!n_weighted) means <- dplyr::select(means, -n_weighted)

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
#' @inheritParams stem_summarise_cat
#' @param collapse_item Named list. Optionally collapes (or renames) categories of the item variable (Only if item is categorical).
#' @param na.rm If `TRUE` (default), `NA` values in the item are dropped and observations with an
#'   `NA` group are excluded. If `FALSE`, `NA` is kept (as its own category for categorical items,
#'   or yielding an `NA` mean for numeric items) and `NA` group values form their own group.
#'
#'@details
#'Apart from either point estimates (proportions or means), the function returns confidence interval bounds
#'(95% by default, see `conf_level`). Further uncertainty measures can be requested via `variance`.
#'If unweighted, standard errors are computed using the basic `sqrt((p * (1-p)) / n)` formula, where `n` is the size of the
#'(possibly segmented) sample, and the design effect is 1. If the estimated proportions are very high/low, this may lead to
#'interval estimates outside of the (0;1) bounds. If weights are used, standard errors come from `srvyr`/`survey`
#'(Taylor-series linearization) and design effects are computed against simple random sampling with replacement.
#'In both cases the intervals are `estimate +/- z * se`, with `z` the normal quantile for `conf_level`.
#'
#'Variables with value labels (`haven_labelled`, e.g. read by [haven::read_sav()]) are converted to factors using the
#'labels. A numeric item with value labels is treated as categorical by `stem_summarise()`; `stem_summarise_num()`
#'averages its underlying codes.
#'
#'A warning (of class `stemtools_warning_small_cell`) is raised when any estimate is based on fewer than `min_cell_n`
#'unweighted observations.
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
#'If `n_weighted = TRUE`, column `n_weighted` holds the sum of weights for the same combination.
#'
#' @return An aggregated data frame with point estimates (either proportions or means) and the requested uncertainty measures.
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
#' stem_summarise(data = trust, item = government, weight = W,
#'                variance = c("se", "moe", "deff"), n_weighted = TRUE)
stem_summarise <- function(data, item, group = NULL, weight = NULL, long = FALSE,
                           collapse_item = NULL, collapse_group = NULL,
                           na.rm = TRUE, return_n = FALSE,
                           variance = "ci", conf_level = 0.95,
                           n_weighted = FALSE, min_cell_n = 30) {

  item_vec <- data[[names(dplyr::select(data, {{ item }}))]]

  if (inherits(item_vec, c("factor", "character", "haven_labelled"))) {
    stem_summarise_cat(data, item = {{ item }}, group = {{ group }}, weight = {{ weight }}, long = long,
                       collapse_item = collapse_item, collapse_group = collapse_group,
                       na.rm = na.rm, return_n = return_n,
                       variance = variance, conf_level = conf_level,
                       n_weighted = n_weighted, min_cell_n = min_cell_n)
  } else if (is.numeric(item_vec)) {
    stem_summarise_num(data, item = {{ item }}, group = {{ group }}, weight = {{ weight }}, long = long,
                       collapse_group = collapse_group, na.rm = na.rm, return_n = return_n,
                       variance = variance, conf_level = conf_level,
                       n_weighted = n_weighted, min_cell_n = min_cell_n)
  } else {
    stop("`item` must be a factor, character, labelled or numeric variable, not ",
         paste(class(item_vec), collapse = "/"), ".", call. = FALSE)
  }
}
