# ggplot2 theme -----------------------------------------------------------

#' Complete ggplot2 theme for Stem.
#'
#' A *complete* ggplot2 theme carrying the Stem look. Because it is complete,
#' the Stem plotting functions do not apply it themselves; instead activate it
#' globally with [ggplot2::theme_set()] so every plot picks it up:
#'
#' ```r
#' ggplot2::theme_set(theme_stem())
#' ```
#'
#' It can still be added to an individual plot with `+ theme_stem()` in the
#' usual way. The theme is built on [ggplot2::theme_grey()] with the Stem
#' overrides layered on top.
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
#' @param family Font family for all text. Defaults to `"Calibri"`, the Stem
#'   house font. Pass `""` to use the graphics device's default family (useful
#'   on machines where Calibri is not installed).
#' @param ... Arguments to be passed to [ggplot2::theme()], overriding the Stem
#'   defaults.
#'
#' @return A complete ggplot2 [ggplot2::theme()] object.
#' @export
#'
#' @examples \dontrun{
#' # Activate globally for the whole session.
#' ggplot2::theme_set(theme_stem())
#' stem_barplot(trust, government)
#'
#' # Or add it to a single plot.
#' stem_barplot(trust, government) + theme_stem()
#'}
theme_stem <- function(
  ink = "black",
  paper = "white",
  accent = "#35978F",
  family = "Calibri",
  ...
) {
  ggplot2::theme_grey(base_family = family) +
    ggplot2::theme(
      geom = ggplot2::element_geom(ink = ink, paper = paper, accent = accent),
      rect = ggplot2::element_rect(fill = paper, colour = NA),
      text = ggplot2::element_text(family = family, size = 18, colour = ink),
      panel.background = ggplot2::element_rect(fill = paper),
      strip.background = ggplot2::element_rect(fill = paper),
      legend.position = "top",
      legend.background = ggplot2::element_rect(
        linewidth = 0.2,
        colour = "grey"
      ),
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
