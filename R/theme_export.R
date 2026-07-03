# ggplot2 theme -----------------------------------------------------------

#' Basic ggplot2 theme for Stem.
#'
#' @param ink Foreground colour used for text, titles and axis lines. Also
#'   passed to [ggplot2::element_geom()] as the default geom foreground colour.
#' @param paper Background colour used behind the panel, plot, strips and
#'   legend keys. Also passed to [ggplot2::element_geom()] as the default geom
#'   background colour. Use `NA` for a transparent background (useful when
#'   exporting plots to be placed on a coloured surface).
#' @param accent Accent colour passed to [ggplot2::element_geom()] as the
#'   default geom accent colour (e.g. the fill of [ggplot2::geom_smooth()]).
#'   Defaults to the primary Stem brand colour.
#' @param ... Arguments to be passed to [ggplot2::theme()].
#'
#' @return ggplot2 [ggplot2::theme()] object.
#' @export
#'
#' @examples \dontrun{
#' stem_plot_bar(trust, government) + theme_stem()
#'}
theme_stem <- function(
  ink = "black",
  paper = "white",
  accent = "#35978F",
  ...
) {
  ggplot2::theme(
    geom = ggplot2::element_geom(ink = ink, paper = paper, accent = accent),
    rect = ggplot2::element_rect(fill = paper, colour = NA),
    text = ggplot2::element_text(family = "Calibri", size = 18, colour = ink),
    panel.background = ggplot2::element_rect(fill = paper),
    strip.background = ggplot2::element_rect(fill = paper),
    legend.position = "top",
    legend.background = ggplot2::element_rect(linewidth = 0.2, colour = "grey"),
    legend.key = ggplot2::element_rect(fill = paper),
    panel.grid = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    title = ggplot2::element_text(face = "bold", colour = ink),
    axis.title = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  ) +
    ggplot2::theme(...)
}
