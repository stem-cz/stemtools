#' @import utils
#' @importFrom rlang .data
NULL

# Bare data-variable symbols referenced via non-standard evaluation (dplyr /
# ggplot2). Declared here so `R CMD check` does not flag them as undefined
# globals. Grouped by where they originate; keep in sync with the code.
utils::globalVariables(c(
  # surveycore::get_freqs()/get_means() output, renamed in R/aggregation.R
  "pct", "ci_low", "ci_high",
  # shared aggregation output columns
  "n", "freq", "freq_low", "freq_upp", "group_n", "item_n", ".se",
  # long-format helper columns (R/aggregation.R)
  "item", "item_cat", "group", "group_cat",
  # plotting helpers (R/plots.R)
  "stem_label", ".response", ".battery_item", "score",
  # palette overview plot (R/color-palettes.R)
  "x", "palette", "color", "type"
))
