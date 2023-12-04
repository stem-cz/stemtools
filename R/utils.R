#' Computes parametric standard error for proportions
#'
#' Computes SE for proportions using the old-school \eqn{\sqrt{\frac{p - (1 - p)}{n}}} formula.
#'
#' @param p proportion
#' @param n sample size
#'
#' @return numeric value representing SE for proportions
#' @export
#'
#' @examples
#' p = 0.3
#' n = 200
#' se_prop(p = p, n = n)
se_prop <- function(p, n) {
  se <- sqrt( (p * (1-p)) / n )
}

#' Computes parametric standard error for mean
#'
#' Computes SE for the mean using the old-school \eqn{\sqrt{\frac{var_x}{n}}} formula.
#'
#' @param x numeric vector
#'
#' @return numeric value representing SE for means
#' @export
#'
#' @examples
#' se_mean(trust$age)
se_mean <- function(x) {
  stopifnot(is.numeric(x))

  sqrt(stats::var(x, na.rm = TRUE) / length(x[!is.na(x)]))
}

#' Computes parametric standard error for median
#'
#' Computes SE for the median using the old-school \eqn{\sqrt{1.2533\cdot\frac{var_x}{n}}} formula.
#' This assumes normal distribution.
#'
#' @param x numeric vector
#'
#' @return numeric value representing SE for medians
#' @export
#'
#' @examples
#' se_median(trust$age)
se_median <- function(x) {
  stopifnot(is.numeric(x))

  1.2533*sqrt(stats::var(x, na.rm = TRUE) / length(x[!is.na(x)]))
}

#' Collapses factor levels and returns a new data frame
#'
#' This function is a wrapper around \code{forcats::fct_collapse()}.
#' It takes a dataframe, a factor variable, and a list of factor levels to collapse.
#'
#' @param data dataset
#' @param item factor variable
#' @param collaps_cats list of factor levels to collapse in shape of \code{list(new_level = old_level)}
#'
#' @return new data frame with collapsed factor levels
#' @export
#'
#' @examples
#' collapse_cats(trust, police, list(Agree = c("Definitely Agree", "Rather Agree")))
collapse_cats <- function(data, item, collaps_cats) {
  group_var <- dplyr::select(data, {{ item }}) |> dplyr::pull()
  collapse_args <- c(list(group_var), collaps_cats)
  data <- data |> dplyr::mutate({{ item }} := do.call(forcats::fct_collapse, collapse_args))

  return(data)
}
