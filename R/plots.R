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
