#' @import utils
utils::globalVariables(c("n",
                         "freq",
                         "freq_label",
                         "item",
                         "response",
                         "label",
                         "ordering",
                         "value",
                         "W",
                         "group_freq",
                         "se",
                         "group_n",
                         "item_n",
                         "group_cat",
                         "item_cat",
                         "item_prop",
                         "group_prop",
                         "estimate",
                         "geom_label",
                         "estimate_low",
                         "estimate_upp",
                         "name",
                         "trust"))

ignore_unused_imports <- function() {
  ragg::agg_png
}
