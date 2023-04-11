# Palette Creation --------------------------------------------------------

#' Select a Color Palette
#'
#' @param palette Name of a palette.
#' @param n Number of colors to extract. If `NA`, extracts all of them.
#'
#' @description
#' Returns a vector of colors forming a cohesive palette. Current palettes include:
#' * Nominal: `gruvbox`,
#' * Diverging: `gruvbox_div`, `modern`, `bluered_light`
#'
#' @return A vector of color hexcodes
#' @export
#'
#' @examples
#' stem_palettes(palette = "gruvbox")
#' stem_palettes(palette = "modern", n = 3)
stem_palettes <- function(palette = "modern", n = NA) {

  if(!is.na(n) & !is.numeric(n)) stop("n has to be an integer")
  if(!is.na(n) & (n < 1L)) stop("n has to be positive integer")

  palettes <- list(
    # Nominal
    gruvbox = c("#fb4934", "#b8bb26", "#83a598", "#fabd2f", "#d3869b", "#8ec07c", "#fe8019"),

    # Diverging
    gruvbox_div = c("#b9211a", "#cc241d", "#fb4934", "#fac64c", "#83a598", "#649590", "#458588"),
    modern = c("#35978F", "#80CDC1", "#B0C89F" ,"#DFC27D", "#BF812D"),
    bluered_light = c("#2166AC", "#92C5DE", "#E8DC99", "#F4A582", "#B2182B")
  )

  if (!is.na(n)) {
    palettes[[palette]][1:n]
  } else {
    palettes[[palette]]
  }

}

# Internal Helper functions -----------------------------------------------

#' Prepares Color Palette for ggplot2
#'
#' @param palette Name of a palette.
#' @param direction If `-1`, reverses the order of colors.
#'
#' @return A vector of color hex codes to use with ggplot2
#' @keywords internal
#'
#' @examples \dontrun{
#' stem_palette_generator()
#' }
stem_palette_generator <- function(palette = "modern", direction = 1) {

  diverging <- c("gruvbox_div", "modern", "bluered_light")

  function(n) {

    if (n > length(stem_palettes(palette))) {
      stop("Not enough colors in this palette!")
    }

    if(any(diverging %in% palette)) {
      colors <- diverging_color_extractor(palette = palette, n = n)
    } else {
      colors <- stem_palettes(palette)
      colors <- colors[1:n]
    }

    colors <- if (direction >= 0) colors else rev(colors)

  }
}

#' Diverging Color Palette Extractor
#'
#' @param palette Name of a palette.
#' @param n Number of colors to extract.
#'
#' @keywords internal
#' @description
#' For diverging palettes, colors should be picked from the both ends in alternating pattern, not sequentially.
#'
#' @examples \dontrun{
#' diverging_color_extractor(stem_palettes(palette = "modern"), n = 3)
#' }
diverging_color_extractor <- function(palette, n) {

  palette <- stem_palettes(palette)

  if(n %% 2) {
    left <- c(1:floor(n/2))
    mid <- ceiling(length(palette) / 2)
    right <- c((length(palette) - floor(n/2) + 1): length(palette))

    palette[c(left, mid, right)]

  } else {
    left <- c(1:floor(n/2))
    right <- c((length(palette) - floor(n/2) + 1): length(palette))

    palette[c(left, right)]

  }

}

# ggplot2 Scale Functions -------------------------------------------------

#' Sequential, diverging and qualitative color scales from `stemtools`
#'
#' @param palette Name of a palette to use.
#' @param direction Orientation of the palette. If `-1`, the order of colors is reversed.
#' @param ... Additional arguments to be passed to [ggplot2::discrete_scale()]
#'
#' @description
#' Returns a vector of colors forming a cohesive palette. Current palettes include:
#' * Nominal: `gruvbox`
#' * Diverging: `gruvbox_div`, `modern`
#'
#' @export
#' @examples
#' ggplot2::ggplot(data = mtcars,
#'        mapping = ggplot2::aes(x = mpg, y = hp, color = as.factor(am))) +
#'        ggplot2::geom_point() +
#'        scale_color_stem()
scale_colour_stem <- function(palette = "modern", direction = 1, ...) {

  ggplot2::discrete_scale(
    "colour", "stem",
    stem_palette_generator(palette, direction),
    ...
  )
}

#' @rdname scale_colour_stem
#' @export
scale_color_stem <- scale_colour_stem

#' @rdname scale_colour_stem
#' @export
scale_fill_stem <- function(palette = "modern", direction = 1, ...) {

  ggplot2::discrete_scale(
    "fill", "stem",
    stem_palette_generator(palette, direction),
    ...
  )
}
