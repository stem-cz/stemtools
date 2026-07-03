# Palette class -----------------------------------------------------------

#' stem S7 palette class
#'
#' @description
#' Internal S7 class used to define and validate colour palettes. Not intended for direct use;
#' use [stem_palette()] to retrieve palette colours.
#'
#' @param name Character. Identifier name for the palette.
#' @param type Character. Palette type: `"nominal"`, `"diverging"`, or `"sequential"`.
#' @param colors Character vector of hex colour codes.
#'
#' @return S7 object of class `"stem_palette"`.
#' @keywords internal
stem_colors <- S7::new_class(
  name = "stem_palette",
  properties = list(
    name = S7::class_character,
    type = S7::class_character,
    colors = S7::class_vector
  ),
  validator = function(self) {
    if (!self@type %in% c("nominal", "diverging", "sequential")) {
      "@type must be one of 'nominal', 'diverging', 'sequential'"
    }
  }
)

# Palette registry --------------------------------------------------------

#' Registry of all stem colour palettes
#'
#' @description
#' A named list of S7 objects of class `stem_palette`, one entry per palette.
#' This is the single source of truth for all palette definitions in the package.
#' [stem_palette()], [stem_palette_gen()] and [stem_palettes_all()] all derive
#' their data from this object.
#'
#' To add a new palette to the package, append an entry here using `stem_colors()`.
#'
#' @format A named list of `stem_palette` S7 objects. Each element has three properties:
#' \describe{
#'   \item{`name`}{Character. The palette identifier, matching the list name.}
#'   \item{`type`}{Character. One of `"nominal"`, `"diverging"`, or `"sequential"`.}
#'   \item{`colors`}{Character vector of hex colour codes.}
#' }
#'
#' @keywords internal
.stem_palettes <- list(
  # Nominal palettes
  gruvbox = stem_colors(
    name = "gruvbox",
    type = "nominal",
    colors = c("#fb4934", "#b8bb26", "#83a598", "#fabd2f", "#d3869b", "#8ec07c", "#fe8019")
  ),
  nom1 = stem_colors(
    name = "nom1",
    type = "nominal",
    colors = c("#FFEB38", "#FF9800", "#F44336", "#9B4363", "#996633")
  ),
  nom2 = stem_colors(
    name = "nom2",
    type = "nominal",
    colors = c("#8BC34A", "#3D918A", "#6094D5", "#3F51B3", "#BCBCBC", "#3C3C3C")
  ),

  # Sequential palettes
  seq1 = stem_colors(
    name = "seq1",
    type = "sequential",
    colors = c("#DBEEEC", "#BEE2DF", "#8CD2CC", "#68C1B9", "#4DA8A0", "#3D918A")
  ),
  seq2 = stem_colors(
    name = "seq2",
    type = "sequential",
    colors = c("#DFEAF7", "#C9D9EC", "#BAD1EC", "#80A9DD", "#6094D5", "#487EC1")
  ),
  seq3 = stem_colors(
    name = "seq3",
    type = "sequential",
    colors = c("#E6D0D8", "#D4B2BB", "#C5919D", "#9B4363", "#82143C", "#71052D")
  ),
  seq4 = stem_colors(
    name = "seq4",
    type = "sequential",
    colors = c("#FCEFE2", "#F9DFC6", "#F7CFA9", "#F4BF8D", "#F1AF70", "#D5904E")
  ),

  # Diverging palettes
  gruvbox_div = stem_colors(
    name = "gruvbox_div",
    type = "diverging",
    colors = c("#b9211a", "#cc241d", "#fb4934", "#fac64c", "#83a598", "#649590", "#458588")
  ),
  modern = stem_colors(
    name = "modern",
    type = "diverging",
    colors = c("#35978F", "#80CDC1", "#B0C89F", "#DFC27D", "#BF812D")
  ),
  div1 = stem_colors(
    name = "div1",
    type = "diverging",
    colors = c("#4DA8A0", "#8CD2CC", "#D1C9BC", "#FDA592", "#FC684D")
  ),
  div2 = stem_colors(
    name = "div2",
    type = "diverging",
    colors = c("#6094D5", "#BAD1EC", "#DCBDC7", "#FDA592", "#FC684D")
  ),
  div3 = stem_colors(
    name = "div3",
    type = "diverging",
    colors = c("#82143C", "#C5919D", "#CAB2B9", "#A6A7A6", "#646464")
  )
)

# Palette accessors -------------------------------------------------------

#' Retrieve a stem colour palette
#'
#' @description
#' Returns a character vector of hex colour codes for the requested palette. The vector carries
#' a `type` attribute (`"nominal"`, `"diverging"`, or `"sequential"`) used internally by
#' [stem_palette_gen()].
#'
#' @param palette Name of the palette. One of:
#'
#' **Nominal** (categorical, unordered data): `"gruvbox"`, `"nom1"`, `"nom2"`.
#'
#' **Sequential** (ordered data from low to high): `"seq1"`, `"seq2"`, `"seq3"`, `"seq4"`.
#'
#' **Diverging** (data with a meaningful midpoint): `"gruvbox_div"`, `"modern"`,
#' `"div1"`, `"div2"`, `"div3"`.
#'
#' @return Character vector of hex colour codes with a `type` attribute
#'   (`"nominal"`, `"diverging"`, or `"sequential"`).
#'
#' @export
#' @seealso [stem_palette_gen()], [scale_colour_stem()], [scale_fill_stem()]
#'
#' @examples
#' # Returns a plain character vector
#' stem_palette("modern")
#'
#' # Inspect the palette type
#' attr(stem_palette("modern"), "type")
#'
#' # Use with scales package for inspection
#' scales::show_col(stem_palette("gruvbox"))
stem_palette <- function(palette = "modern") {
  pal <- .stem_palettes[[palette]]

  if (is.null(pal)) {
    stop("Unknown palette '", palette, "'. See `?stem_palette` for available palettes.",
         call. = FALSE)
  }

  colors <- pal@colors
  attr(colors, "type") <- pal@type
  colors
}

#' Colour palette generator factory
#'
#' @description
#' Returns a function `function(n)` that selects `n` colours from the named palette.
#' This factory is used internally by [scale_colour_stem()] and [scale_fill_stem()] to
#' satisfy ggplot2's discrete scale interface, but can also be called directly.
#'
#' Diverging palettes use [select_diverging()] to pick colours that preserve the
#' symmetric structure of the palette. All other palette types return the first `n` colours,
#' optionally reversed.
#'
#' @param palette Name of the palette. See [stem_palette()] for the full list of valid names.
#' @param direction `1` (default) returns colours in their natural order; `-1` reverses the order.
#'
#' @return A `function(n)` that returns a character vector of `n` hex colour codes.
#'
#' @export
#' @seealso [stem_palette()], [select_diverging()], [scale_colour_stem()]
#'
#' @examples
#' # Create a generator then call it for 3 colours
#' gen <- stem_palette_gen("modern")
#' gen(3)
#'
#' # Reversed order
#' stem_palette_gen("div1", direction = -1)(5)
stem_palette_gen <- function(palette = "modern", direction = 1) {
  function(n) {
    all_colors <- stem_palette(palette)

    if (n > length(all_colors)) {
      stop("Not enough colours in palette '", palette, "' (needs ", n,
           ", has ", length(all_colors), ").", call. = FALSE)
    }

    if (attr(all_colors, "type") == "diverging") {
      all_colors <- select_diverging(all_colors, n)
    }

    all_colors <- all_colors[seq_len(n)]

    if (direction < 0) all_colors <- rev(all_colors)

    all_colors
  }
}

#' Select n colours from a diverging palette
#'
#' @description
#' Selects `n` colours from a diverging palette while preserving its symmetric, center-anchored
#' structure. Rather than simply taking the first `n` colours, this function samples
#' symmetrically from both ends of the palette:
#'
#' - **Odd `n`**: the palette midpoint is always included, with an equal number of colours
#'   drawn from each side.
#' - **Even `n`**: colours are drawn evenly from the left and right halves.
#'
#' The selected colours are always returned in their original palette order.
#'
#' @param palette Character vector of hex colour codes representing the full diverging palette.
#' @param n Number of colours to select. Must be between 1 and `length(palette)`.
#'
#' @return Character vector of `n` hex colour codes in original palette order.
#'
#' @export
#' @seealso [stem_palette_gen()], [stem_palette()]
#'
#' @examples
#' pal <- stem_palette("modern")
#'
#' # Odd n: midpoint included, balanced sides
#' select_diverging(pal, n = 3)
#'
#' # Even n: two from each end
#' select_diverging(pal, n = 4)
select_diverging <- function(palette, n) {
  if (n < 1 || n > length(palette)) {
    stop("n must be between 1 and ", length(palette), call. = FALSE)
  }

  if (n == length(palette)) {
    return(palette)
  }

  total_length <- length(palette)
  middle_pos <- ceiling(total_length / 2)
  selected_positions <- numeric(n)

  if (n %% 2 == 1) {
    # Odd n: anchor the midpoint, then fill symmetrically outward.
    middle_index <- ceiling(n / 2)
    selected_positions[middle_index] <- middle_pos

    left_count <- middle_index - 1
    right_count <- n - middle_index

    if (left_count > 0) {
      for (i in seq_len(left_count)) selected_positions[i] <- i
    }
    if (right_count > 0) {
      for (i in seq_len(right_count)) {
        selected_positions[middle_index + i] <- total_length - right_count + i
      }
    }
  } else {
    # Even n: draw evenly from both ends.
    half_n <- n / 2
    for (i in seq_len(half_n)) {
      selected_positions[i] <- i
      selected_positions[n - i + 1] <- total_length - i + 1
    }
  }

  palette[sort(selected_positions)]
}

# ggplot2 scales ----------------------------------------------------------

#' stem discrete colour and fill scales for ggplot2
#'
#' @description
#' Discrete ggplot2 scales that apply a stem colour palette to the `colour` or `fill` aesthetic.
#' Three aliases are provided:
#' - `scale_colour_stem()` — maps to the `colour` aesthetic (British spelling).
#' - `scale_color_stem()` — alias for `scale_colour_stem()` (American spelling).
#' - `scale_fill_stem()` — maps to the `fill` aesthetic.
#'
#' All functions delegate to [ggplot2::discrete_scale()] via [stem_palette_gen()], so any argument
#' accepted by `discrete_scale()` (e.g. `name`, `breaks`, `labels`, `na.value`, `drop`) can
#' be passed through `...`.
#'
#' @param palette Name of the palette. See [stem_palette()] for the full list of valid names.
#'   Defaults to `"modern"`.
#' @param direction `1` (default) uses colours in their natural order; `-1` reverses the order.
#' @param ... Additional arguments passed to [ggplot2::discrete_scale()].
#'
#' @return A ggplot2 `Scale` object to be added to a plot with `+`.
#'
#' @export
#' @seealso [stem_palette()], [stem_palette_gen()], [ggplot2::discrete_scale()]
#'
#' @examples
#' ggplot2::ggplot(data = mtcars,
#'                 mapping = ggplot2::aes(x = mpg, y = hp, color = as.factor(am))) +
#'   ggplot2::geom_point() +
#'   scale_colour_stem()
scale_colour_stem <- function(palette = "modern", direction = 1, ...) {
  ggplot2::discrete_scale(
    "colour",
    "stem",
    stem_palette_gen(palette, direction),
    ...
  )
}

#' @rdname scale_colour_stem
#' @export
#' @order 2
scale_fill_stem <- function(palette = "modern", direction = 1, ...) {
  ggplot2::discrete_scale(
    "fill",
    "stem",
    stem_palette_gen(palette, direction),
    ...
  )
}

#' @rdname scale_colour_stem
#' @export
scale_color_stem <- scale_colour_stem

# Palette overview --------------------------------------------------------

#' Display all available stem palettes
#'
#' @description
#' Plots a visual overview of all available stem colour palettes, grouped by type
#' (nominal, diverging, sequential). Each row shows one palette's colours as tiles,
#' similar to `RColorBrewer::display.brewer.all()`.
#'
#' @return A ggplot2 object.
#'
#' @export
#' @seealso [stem_palette()], [scale_colour_stem()]
#'
#' @examples
#' stem_palettes_all()
stem_palettes_all <- function() {
  palette_names <- names(.stem_palettes)

  df <- do.call(
    rbind,
    lapply(palette_names, function(name) {
      colors <- stem_palette(name)
      data.frame(
        palette = name,
        type = attr(colors, "type"),
        x = seq_along(colors),
        color = as.character(colors),
        stringsAsFactors = FALSE
      )
    })
  )

  df$palette <- factor(df$palette, levels = rev(palette_names))
  df$type <- factor(df$type, levels = c("sequential", "diverging", "nominal"))

  ggplot2::ggplot(df, ggplot2::aes(x = x, y = palette, fill = I(color))) +
    ggplot2::geom_tile(color = "white", linewidth = 0.6) +
    ggplot2::facet_grid(type ~ ., scales = "free_y", space = "free_y") +
    ggplot2::scale_x_continuous(
      breaks = NULL,
      expand = ggplot2::expansion(add = 0.5)
    ) +
    ggplot2::labs(x = NULL, y = NULL, title = "stem colour palettes") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      strip.text.y = ggplot2::element_text(angle = 0, hjust = 0, face = "bold"),
      axis.text.y = ggplot2::element_text(hjust = 1),
      plot.title = ggplot2::element_text(
        face = "bold",
        margin = ggplot2::margin(b = 8)
      )
    )
}
