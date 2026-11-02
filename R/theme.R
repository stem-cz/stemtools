# ggplot2 theme -----------------------------------------------------------

#' Complete ggplot2 theme for Stem.
#'
#' A *complete* ggplot2 theme carrying the Stem look. The Stem plotting
#' functions attach it to every plot themselves (their `theme` argument), so
#' you only call it to change it, e.g. `stem_barplot(..., theme =
#' theme_stem(base_size = 18))` or `+ theme_stem(paper = NA)`. With `theme =
#' NULL` the plotting functions attach no theme, and one activated with
#' [ggplot2::theme_set()] applies instead. It also works on any other ggplot.
#'
#' The theme is built on [ggplot2::theme_grey()] with the Stem overrides
#' layered on top.
#'
#' @param ink Foreground colour used for text, titles and axis lines. Also
#'   passed to [ggplot2::element_geom()] as the default geom foreground colour.
#' @param paper Background colour used behind the panel, plot, strips and
#'   legend keys. Also passed to [ggplot2::element_geom()] as the default geom
#'   background colour. Use `NA` for a transparent background (useful when
#'   exporting plots to be placed on a coloured surface).
#' @param accent Accent colour passed to [ggplot2::element_geom()] as the
#'   default geom accent colour (e.g. the fill of [ggplot2::geom_smooth()]).
#'   Defaults to the Stem brand burgundy `"#82143C"`, also the default fill of
#'   single-series bars (`bar_color` in [stem_barplot()]).
#' @param family Font family for all text. Defaults to `"Calibri"`, the Stem
#'   house font, or to the `stemtools.family` option when set. Pass `""` to use
#'   the graphics device's default family (useful on machines where Calibri is
#'   not installed); `options(stemtools.family = "")` does that for every plot
#'   of the session.
#' @param base_size Base font size in points. Defaults to `13`, sized for
#'   charts exported 19 cm wide. Other text sizes are relative to it (e.g. the
#'   title is 1.2 × `base_size`, 15.6 pt by default; axis and legend text
#'   0.8 ×).
#' @param label_size Point size of the numeric labels drawn inside the plot
#'   (the `stem_label` values printed by [stem_barplot()] and friends). Passed
#'   to [ggplot2::element_geom()] as `fontsize`, so it is given in points, like
#'   the other text sizes of the theme. Defaults to `12`.
#' @param ... Arguments to be passed to [ggplot2::theme()], overriding the Stem
#'   defaults.
#'
#' @return A complete ggplot2 [ggplot2::theme()] object.
#' @export
#'
#' @examples \dontrun{
#' # The Stem plotting functions attach it themselves.
#' stem_barplot(trust, government)
#'
#' # A variant, for one plot.
#' stem_barplot(trust, government, theme = theme_stem(base_size = 18))
#'
#' # No Calibri on this machine: use the device's font for the whole session.
#' options(stemtools.family = "")
#'
#' # Any other ggplot.
#' ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) + ggplot2::geom_point() + theme_stem()
#'}
theme_stem <- function(
  ink = "black",
  paper = "white",
  accent = "#82143C",
  family = getOption("stemtools.family", "Calibri"),
  label_size = 12,
  base_size = 13,
  ...
) {
  # Line widths stay at theme_grey()'s defaults (11 / 22), whatever the type
  # size, so bar separators and native chart borders keep their width.
  ggplot2::theme_grey(
    base_size = base_size,
    base_family = family,
    base_line_size = 11 / 22,
    base_rect_size = 11 / 22
  ) +
    ggplot2::theme(
      geom = ggplot2::element_geom(
        ink = ink,
        paper = paper,
        accent = accent,
        fontsize = label_size
      ),
      rect = ggplot2::element_rect(fill = paper, colour = NA),
      text = ggplot2::element_text(family = family, size = base_size, colour = ink),
      panel.background = ggplot2::element_rect(fill = paper),
      strip.background = ggplot2::element_rect(fill = paper),
      legend.position = "top",
      # Left-aligned with the title, which also starts at the plot's edge.
      legend.location = "plot",
      legend.justification.top = "left",
      # Room on the right for the "100 %" axis label, which sticks out half its
      # width (about 12 pt) past the panel.
      plot.margin = ggplot2::margin(t = 6.5, r = 18, b = 6.5, l = 6.5),
      legend.background = ggplot2::element_rect(
        linewidth = 0.2,
        colour = "grey"
      ),
      legend.key = ggplot2::element_rect(fill = paper),
      # A compact legend, so that most 5-point scales fit one row of a 19 cm
      # chart (measured in Calibri: about 20 pt per entry besides its text).
      legend.key.width = ggplot2::unit(10, "pt"),
      legend.key.height = ggplot2::unit(10, "pt"),
      legend.key.spacing.x = ggplot2::unit(6, "pt"),
      legend.text = ggplot2::element_text(margin = ggplot2::margin(l = 3)),
      legend.margin = ggplot2::margin(3, 4, 3, 4),
      panel.grid = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      title = ggplot2::element_text(face = "bold", colour = ink),
      plot.title = ggplot2::element_text(face = "bold"),
      axis.title = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.caption.position = "plot"
    ) +
    ggplot2::theme(...)
}
