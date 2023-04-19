#' Plot a Single Item in Line
#'
#' @description
#' Plots a single categorical item (usually a likert item) as a horizontal barplot.
#'
#'
#' @param data Dataframe with the item to be plotted.
#' @param item Item to plot
#' @param weight If `FALSE` raw frequencies are ploted. Can be a variable holding survey weights. For now, the name of the variable has to be quoted.
#' @param wrap_width Length of the legend categories names before they are wrapped to the next line.
#' @param stem_palette Name of a palette from [stemtools::stem_palettes()].
#' @param text_color Color of the labels inside the plot.
#' @param axis_suffix Suffixes of labels on the X axis.
#' @param segment_suffix Suffixes of labels inside the plot.
#'
#' @return A ggplot2 graph.
#' @export
#'
#' @examples
#' stem_plot_single(iris, Species)
stem_plot_single <- function(data, item, weight = FALSE, wrap_width = 17, stem_palette = "modern",
                             text_color = "black", axis_suffix = "%", segment_suffix = "%") {

  if(weight == FALSE) {
    data <- data |>
      dplyr::count(item = {{ item }}) |>
      dplyr::mutate(freq = n / sum(n),
                    freq_label = scales::percent(freq, accuracy = 1, suffix = segment_suffix))
  } else {
    data <- data |>
      srvyr::as_survey_design(weights = weight) |>
      srvyr::group_by(item = {{item}}) |>
      srvyr::summarise(freq = srvyr::survey_prop(vartype = NULL)) |>
      srvyr::mutate(freq_label = scales::percent(freq, accuracy = 1, suffix = segment_suffix))
  }

  plot <- data |>
    dplyr::mutate(item = forcats::fct_rev(item)) |>
    ggplot2::ggplot(ggplot2::aes(x = freq,
                                 y = "",
                                 fill = item,
                                 label = freq_label)) +
    ggplot2::geom_col(position = "stack") +
    ggplot2::geom_text(position = ggplot2::position_stack(vjust = 0.5),
                       color = text_color) +
    scale_fill_stem(palette = stem_palette,
                    direction = -1,
                    labels = scales::label_wrap(width = wrap_width)) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1, suffix = axis_suffix)) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE))

  attr(plot, "plot_fn") <- "plot_single"

  return(plot)

}


#' Plot Multiple Items in Line
#'
#' @description
#' Plot multiple categorical items (e.g. battery of likert items) in a stacked barplot.
#'
#'
#' @param data Dataframe with the items to be plotted.
#' @param items Items to plot, can be selected using dplyr's selection helpers.
#' @param weight If `NA`, returns plot with unweighted frequencies. If (quoted) name of variable holding survey weights, returns weighted frequencies.
#' @param var_labels Optional dataframe with item labels used in the plot. Has to include columns `item` and `label`.
#' @param order_by Optional vector of response category/categories. Items in the plot will be sorted based on their frequencies.
#' @param reverse_responses Reverses order of response categories. Defaults to `TRUE`, since most often we want positive responses on the left.
#' @param labels Logical value. Should plot include numerical labels for each category?
#' @param hide_labels Value between 0 and 1. Hides labels with with frequencies lower than this value.
#' @param labels_size Font size for labels inside the plot.
#' @param labels_color Color of labels inside the plot. Can be either label ("black", "white") or hexcode.
#' @param labels_suffix Suffix for labels inside the plot (e.g. "%" or " %").
#' @param axis_suffix Suffix for the x axis labels (e.g. "%" or " %").
#' @param axis_wrap Width of y axis label line before the text gets wrapped.
#'
#' @return A ggplot2 graph.
#' @export
#'
stem_plot_multiple <- function(data,
                               items,
                               weight = NA,
                               var_labels = NA,
                               order_by = NA,
                               reverse_responses = TRUE,
                               labels = TRUE,
                               hide_labels = 0.05,
                               labels_size = 5,
                               labels_color = "black",
                               labels_suffix = "%",
                               axis_suffix = "%",
                               axis_wrap = 80) {

  if(is.na(weight)) {
    data <- data |>
      dplyr::select({{items}}) |>
      tidyr::pivot_longer(cols = tidyselect::everything(),
                   names_to = "item",
                   values_to = "response") |>
      dplyr::count(item, response) |>
      dplyr::group_by(item) |>
      dplyr::mutate(freq = n / sum(n),
                    freq_label = scales::percent(freq, accuracy = 1),
                    freq_label = dplyr::if_else(freq < hide_labels,
                                                true = "",
                                                false = freq_label)) |>
      dplyr::ungroup()
  } else {
    data <- data |>
      dplyr::select({{items}}, weight) |>
      tidyr::pivot_longer(cols = -weight,
                   names_to = "item",
                   values_to = "response") |>
      srvyr::as_survey_design(weights = weight) |>
      srvyr::survey_count(item, response) |>
      dplyr::group_by(item) |>
      dplyr::mutate(freq = n / sum(n),
                    freq_label = scales::percent(freq, accuracy = 1, suffix = labels_suffix),
                    freq_label = dplyr::if_else(freq < hide_labels,
                                                true = "",
                                                false = freq_label)) |>
      dplyr::ungroup()
  }

  if(is.data.frame(var_labels)) {

    if(!any(names(var_labels) %in% c("item", "label")) ) {
      stop("Datarame with item labels has to include columns called `item` and `label`.")
    }

    data <- dplyr::left_join(data, var_labels, by = "item") |>
            dplyr::mutate(item = label) |>
            dplyr::select(-label)
  }

  if(!any(is.na(order_by))) {
    data <- data |>
      dplyr::group_by(item) |>
      dplyr::mutate(ordering = sum(freq[response %in% order_by])) |>
      dplyr::ungroup() |>
      dplyr::mutate(item = forcats::fct_reorder(item, ordering)) |>
      dplyr::select(-ordering)
  }

  if(reverse_responses) {
    data$response <- forcats::fct_rev(data$response)
  }

  plot <- ggplot2::ggplot(data = data,
                          mapping = ggplot2::aes(x = freq,
                                                 y = item,
                                                 fill = response)) +
    ggplot2::geom_col() +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1, suffix = axis_suffix)) +
    ggplot2::scale_y_discrete(labels = ~stringr::str_wrap(., width = axis_wrap))

  if(labels) {
    plot <- plot + ggplot2::geom_text(mapping = ggplot2::aes(label = freq_label),
                                                color = labels_color,
                                                size = labels_size,
                                                position = ggplot2::position_stack(vjust = 0.5))
  }

  plot <- plot +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE))

  attr(plot, "plot_fn") <- "plot_multiple"
  attr(plot, "n_items") <- length(unique(data$item))

  return(plot)

}

#' Plot Item As Simple Barplot
#'
#' @description
#' Plots a single categorical variable as a simple barplot.
#'
#'
#' @param data Dataframe with the item to be plotted.
#' @param item Items to plot.
#' @param weight If `NA`, returns plot with unweighted frequencies. If (quoted) name of variable holding survey weights, returns weighted frequencies.
#' @param label Logical value. Should plot include numerical labels for each category?
#' @param nudge_label Distance between bar and label. Can be negative to put label inside bar.
#' @param label_suffix Suffix for labels inside the plot (e.g. "%" or " %").
#' @param axis_suffix Suffix for the x axis labels (e.g. "%" or " %").
#' @param axis_wrap Width of y axis label line before the text gets wrapped.
#' @param reverse Reverses order of response categories.
#' @param coord_flip Reverses plot axes.
#'
#' @return A ggplot2 plot
#' @export
#'
#' @examples
#' stem_plot_bar(iris, Species)
stem_plot_bar <- function(data,
                          item,
                          weight = FALSE,
                          label = TRUE,
                          nudge_label = 0.02,
                          label_suffix = "%",
                          axis_suffix = "%",
                          axis_wrap = 40,
                          reverse = FALSE,
                          coord_flip = FALSE) {
  if(weight == FALSE) {
    data <- data |>
      dplyr::count(item = {{ item }}) |>
      dplyr::mutate(freq = n / sum(n),
                    freq_label = scales::percent(freq, accuracy = 1, suffix = label_suffix))
  } else {
    data <- data |>
      srvyr::as_survey_design(weights = weight) |>
      srvyr::group_by(item = {{item}}) |>
      srvyr::summarise(freq = srvyr::survey_prop(vartype = NULL)) |>
      srvyr::mutate(freq_label = scales::percent(freq, accuracy = 1, suffix = label_suffix))
  }

  if(reverse) {
    data$item <- forcats::fct_rev(data$item)
  }

  plot <- ggplot2::ggplot(data = data,
                          ggplot2::aes(x = item,
                                       y = freq,
                                       label = freq_label)) +
    ggplot2::geom_col() +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1,
                                                                suffix = axis_suffix)) +
    ggplot2::scale_x_discrete(labels = ~stringr::str_wrap(., axis_wrap))

  if(label) {
    plot <- plot + ggplot2::geom_text(nudge_y = nudge_label)
  }

  if(coord_flip) {
    plot <- plot + ggplot2::coord_flip()
    attr(plot, "plot_fn") <- "plot_bar_v"
  } else {
    attr(plot, "plot_fn") <- "plot_bar_h"
  }

  attr(plot, "n_items") <- length(unique(data$item))

  return(plot)

}
