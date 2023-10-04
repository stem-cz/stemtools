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
#' prop_se(p = p, n = n)
prop_se <- function(p, n) {
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

#' Wrapper for the deparse-substitute combo
#'
#' Transforms object name to character string.
#'
#' @param x object name
#'
#' @return character string
#' @export
#'
#' @examples
#' desub(trust)
#'
desub <- function(x) {
  deparse(substitute(x))
}
