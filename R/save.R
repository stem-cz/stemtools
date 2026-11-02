# Image export ------------------------------------------------------------

#' Save a Stem plot as an image
#'
#' Saves a plot at the size the Stem charts are designed for (19 cm wide), with
#' a graphics device that renders the house font. [ggplot2::ggsave()] picks the
#' ragg device whenever it is installed, and ragg has been seen to draw plots in
#' Calibri as blank images on Windows; `stem_save()` uses the system's own
#' devices instead, which render Calibri (Czech diacritics included).
#'
#' The format follows the file extension:
#'
#' * `.png`: [grDevices::png()], with `type = "quartz"` on macOS and
#'   `type = "cairo"` elsewhere,
#' * `.svg`: [svglite::svglite()] when installed (text stays text, so the
#'   viewer needs the font), otherwise [grDevices::svg()] (text as outlines),
#' * `.pdf`: [grDevices::quartz()] on macOS and [grDevices::cairo_pdf()]
#'   elsewhere, both embedding the fonts.
#'
#' On macOS the Cairo devices need XQuartz, which most Macs lack, so the native
#' Quartz devices are used there.
#'
#' Box plots and other charts that cannot become native Office charts (see
#' [stem_as_mschart()]) are meant to go into decks this way.
#'
#' @param plot A ggplot2 object, or a list of them (e.g. from
#'   `stem_battery(items_per_chart = )`), which are saved as `path` with
#'   `_1`, `_2`, ... inserted before the extension.
#' @param path File path ending in `.png`, `.svg` or `.pdf`.
#' @param width,height Size of the image in `units`. Defaults to 19 × 10 cm.
#' @param units `"cm"` (default), `"mm"` or `"in"`.
#' @param dpi Resolution of PNG images. Defaults to `300`.
#' @param bg Background colour. Defaults to `"white"`.
#'
#' @return The path(s) written, invisibly.
#' @export
#' @seealso [stem_export_pptx()] for native Office charts.
#'
#' @examples \dontrun{
#' ggplot2::theme_set(theme_stem())
#' stem_save(stem_barplot(trust, police, group = eu_index), "police.png")
#' stem_save(stem_boxplot(trust, age, group = eu_index), "age.svg", height = 8)
#' }
stem_save <- function(
  plot,
  path,
  width = 19,
  height = 10,
  units = "cm",
  dpi = 300,
  bg = "white"
) {
  if (is.list(plot) && !inherits(plot, "gg")) {
    paths <- vapply(seq_along(plot), function(i) {
      numbered <- sub("(\\.[^.]+)$", paste0("_", i, "\\1"), path)
      stem_save(plot[[i]], numbered, width, height, units, dpi, bg)
    }, character(1))
    return(invisible(paths))
  }

  if (!inherits(plot, "gg")) {
    stop("`plot` must be a ggplot2 object or a list of them.", call. = FALSE)
  }

  ext <- tolower(tools::file_ext(path))
  width_in <- stem_to_inches(width, units)
  height_in <- stem_to_inches(height, units)
  # On macOS the Cairo devices depend on XQuartz, which is usually missing
  # (R still reports Cairo as available); the native Quartz devices aren't.
  on_mac <- identical(Sys.info()[["sysname"]], "Darwin")

  switch(
    ext,
    png = grDevices::png(
      path,
      width = width_in,
      height = height_in,
      units = "in",
      res = dpi,
      bg = bg,
      type = if (on_mac) {
        "quartz"
      } else if (isTRUE(capabilities("cairo"))) {
        "cairo"
      } else {
        getOption("bitmapType")
      }
    ),
    svg = if (requireNamespace("svglite", quietly = TRUE)) {
      svglite::svglite(path, width = width_in, height = height_in, bg = bg)
    } else {
      grDevices::svg(path, width = width_in, height = height_in, bg = bg)
    },
    pdf = if (on_mac) {
      grDevices::quartz(type = "pdf", file = path, width = width_in, height = height_in, bg = bg)
    } else {
      grDevices::cairo_pdf(path, width = width_in, height = height_in, bg = bg)
    },
    stop("`path` must end in .png, .svg or .pdf, not \".", ext, "\".", call. = FALSE)
  )
  on.exit(grDevices::dev.off(), add = TRUE)
  print(plot)

  invisible(path)
}
