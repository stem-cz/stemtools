# ggplot2 theme -----------------------------------------------------------

#' Basic ggplot2 theme for Stem.
#'
#' @param ... Arguments to be passed to [ggplot2::theme()].
#'
#' @return ggplot2 [ggplot2::theme()] object.
#' @export
#'
#' @examples \dontrun{
#' stem_plot_bar(trust, government) + theme_stem()
#'}
theme_stem <- function(...) {
  ggplot2::theme(rect = ggplot2::element_rect(fil = NA, color = NA),
                 panel.background = ggplot2::element_rect(fill = NA),
                 strip.background = ggplot2::element_rect(fill = NA),
                 legend.position = "top",
                 legend.background = ggplot2::element_rect(linewidth = 0.2, colour = "grey"),
                 legend.key = ggplot2::element_rect(fill = NA),
                 panel.grid.minor = ggplot2::element_blank(),
                 axis.ticks.y = ggplot2::element_blank(),
                 title = ggplot2::element_text(face = "bold"),
                 axis.title = ggplot2::element_blank(),
                 legend.title = ggplot2::element_blank(),
                 plot.title.position = "plot",
                 plot.caption.position = "plot",
                 text = ggplot2::element_text(family = "Calibri", size = 18)) +
    ggplot2::theme(...)
}


# Exporting Functions -----------------------------------------------------

#' Export Stem plots
#'
#' @description
#' Wrapper around [ggplot2::ggsave()] which tries to export plots in a format appropriate for typical report.
#' Only works for plots created by `stem_plot_*` family of functions.
#'
#'
#' @param plot Plot created by `stem_plot_*`, e.g. [stemtools::stem_plot_bar()].
#' @param name Name of exported file, which has to include extension (e.g. ".png"). If `NA`, will be the same as the plot object (extension not needed then).
#' @param path Optional path to export the plot to.
#' @param device Device to be used for exporting, e.g. "png" or "pdf".
#' @param dpi Resultion of the exported plot. Usually 300 for "medium" resolution, 600 for "high".
#'
#'@details
#'This function aims at exporting common types of plots in correct size and resolution with minimal user input.
#'Each plot created by `stem_plot_*` function has `plot_fn` attribute used to set default width and heigh of the output.
#'For example, plots created by [stemtools::stem_plot_bar()] have attribute value of `"stem_plot_bar"`, which leads to
#'them being exported in 16x4 cm dimensions.
#'
#'
#' @return File with exported plot.
#' @export
#'
#' @examples
#' \dontrun{
#' species_plot <- stem_plot_bar(trust, government)
#'
#' stem_ggsave(species_plot)
#' }
#'

stem_ggsave <- function(plot, name = NA, path = NULL, device = "png" ,dpi = 300) {

  type <- attr(plot, "plot_fn")
  if(is.null(type)) {stop("Unknown plot type. Only plots created with stem_plot_* functions can be used.")}

  if(is.na(name)) {name = paste0(deparse(substitute(plot)),".",device)}

  if(type == "plot_single") {
    ggplot2::ggsave(plot = plot,
                    filename = name,
                    device = device,
                    path = path,
                    dpi = dpi,
                    units = "cm",
                    width = 16,
                    height = 4)
  } else if(type == "plot_multiple") {
    ggplot2::ggsave(plot = plot,
                    filename = name,
                    device = device,
                    path = path,
                    dpi = dpi,
                    units = "cm",
                    width = 16,
                    height = 3 + 1 * attr(plot, "n_items"))
  } else if(type == "plot_bar_v") {
    ggplot2::ggsave(plot = plot,
                    filename = name,
                    device = device,
                    path = path,
                    dpi = dpi,
                    units = "cm",
                    width = 16,
                    height = 1.4 * attr(plot, "n_items"))
  } else if(type == "plot_bar_h") {
    ggplot2::ggsave(plot = plot,
                    filename = name,
                    device = device,
                    path = path,
                    dpi = dpi,
                    units = "cm",
                    width = 3.2 * attr(plot, "n_items"),
                    height = 1.4 * attr(plot, "n_items"))
  } else if(type == "plot_bar_grouped_h") {
    ggplot2::ggsave(plot = plot,
                    filename = name,
                    device = device,
                    path = path,
                    dpi = dpi,
                    units = "cm",
                    width = 4.8 * attr(plot, "n_items"),
                    height = 2.1 * attr(plot, "n_items"))
  } else if(type == "plot_bar_grouped_v"){
    ggplot2::ggsave(plot = plot,
                    filename = name,
                    device = device,
                    path = path,
                    dpi = dpi,
                    units = "cm",
                    width = 16,
                    height = 2 + 1 * attr(plot, "n_items"))
  } else if(type == "plot_bartable") {
    ggplot2::ggsave(plot = plot,
                    filename = name,
                    device = device,
                    path = path,
                    dpi = dpi,
                    units = "cm",
                    width = 14,
                    height = 12)
  } else if(type == "plot_grouped_bar_v") {
    ggplot2::ggsave(plot = plot,
                    filename = name,
                    device = device,
                    path = path,
                    dpi = dpi,
                    units = "cm",
                    width = 16,
                    height = 1.4 * attr(plot, "n_items"))
  } else if(type == "plot_grouped_bar_h") {
    ggplot2::ggsave(plot = plot,
                    filename = name,
                    device = device,
                    path = path,
                    dpi = dpi,
                    units = "cm",
                    width = 3.2 * attr(plot, "n_items"),
                    height = 1.5 * attr(plot, "n_items"))
  } else {print("Unknown plot type.")}

}
