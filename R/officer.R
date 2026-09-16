# Native Microsoft Office charts ------------------------------------------

# The functions below turn a ggplot2 object produced by one of the Stem
# plotting functions (`stem_barplot()`, `stem_inline()`, `stem_battery()`,
# `stem_multiselect()`) into a *native* Microsoft Office chart (mschart), so
# that the resulting PowerPoint/Word file carries an embedded worksheet and the
# source data can be edited in Excel.
#
# Nothing here recomputes statistics: the aggregated data, the colours, the
# labels and the typography are all read back off the ggplot object, so the
# native chart follows the same single source of truth as the ggplot version.

#' Name of the data column behind an aesthetic
#'
#' Internal helper. The Stem plotting functions map aesthetics either as bare
#' symbols (`.response`) or through the pronoun (`.data[["government"]]`). This
#' returns the column name for both forms, and `NULL` when the aesthetic is a
#' constant (e.g. the `y = ""` of an inline bar) or absent.
#'
#' @param quo A quosure taken from a ggplot mapping.
#' @param data The plot data.
#'
#' @return A length-one character vector, or `NULL`.
#' @keywords internal
stem_aes_name <- function(quo, data) {
  if (is.null(quo) || !rlang::is_quosure(quo)) {
    return(NULL)
  }

  expr <- rlang::quo_get_expr(quo)

  # `.data[["x"]]` / `.data$x`
  if (rlang::is_call(expr, c("[[", "$"))) {
    target <- expr[[2]]
    if (identical(target, quote(.data))) {
      nm <- expr[[3]]
      nm <- if (is.character(nm)) nm else rlang::as_string(nm)
      return(nm)
    }
  }

  if (rlang::is_symbol(expr)) {
    nm <- rlang::as_string(expr)
    if (nm %in% names(data)) {
      return(nm)
    }
  }

  NULL
}

#' Derive an Excel number format from Stem percentage labels
#'
#' Internal helper. Reads the preformatted `stem_label` column back to work out
#' how many decimal places and which suffix the ggplot labels use, and returns
#' the matching Excel number format code. Values handed to the chart are
#' percentages (0-100), so the `%` sign is escaped rather than applied as a
#' format multiplier.
#'
#' @param labels Character vector of formatted labels (the `stem_label` column).
#' @param fallback Number format used when no label can be parsed.
#'
#' @return A length-one character vector holding an Excel number format code.
#' @keywords internal
stem_num_fmt <- function(labels, fallback = "0") {
  labels <- labels[!is.na(labels) & nzchar(labels)]
  if (!length(labels)) {
    return(fallback)
  }

  parts <- regmatches(
    labels,
    regexec("^-?[0-9]+(?:([.,])([0-9]+))?(.*)$", labels)
  )
  parts <- parts[lengths(parts) == 4L]
  if (!length(parts)) {
    return(fallback)
  }

  decimals <- max(vapply(parts, function(x) nchar(x[[3]]), integer(1)))
  suffix <- parts[[1]][[4]]

  fmt <- if (decimals > 0) {
    paste0("0.", strrep("0", decimals))
  } else {
    "0"
  }

  if (nzchar(suffix)) {
    fmt <- paste0(fmt, "\"", suffix, "\"")
  }

  fmt
}

#' Category levels of a plotting variable
#'
#' Internal helper returning the levels of a factor (dropping unused ones) or
#' the values of a character/numeric column in first-appearance order, which is
#' the order ggplot2 and the embedded worksheet should agree on.
#'
#' @param x A vector taken from the plot data.
#'
#' @return A character vector of levels.
#' @keywords internal
stem_levels <- function(x) {
  if (is.factor(x)) {
    return(levels(droplevels(x)))
  }
  as.character(sort(unique(x)))
}

#' Describe a Stem ggplot in chart terms
#'
#' Internal helper that reads a ggplot object produced by one of the Stem
#' plotting functions and returns everything the mschart conversion needs:
#' the aggregated data, which column plays which role, the chart flavour
#' (`"simple"`, `"stacked"` or `"dodged"`), the series colours and the
#' typography resolved from the active theme.
#'
#' @param plot A ggplot2 object created by a Stem plotting function.
#'
#' @return A named list describing the chart.
#' @keywords internal
stem_chart_spec <- function(plot) {
  if (!inherits(plot, "ggplot")) {
    stop("`plot` must be a ggplot2 object.", call. = FALSE)
  }

  data <- plot$data
  if (!is.data.frame(data) || !"freq" %in% names(data)) {
    stop(
      "`plot` does not look like a Stem plot: no `freq` column found. ",
      "Use a plot created by stem_barplot(), stem_inline(), stem_battery() ",
      "or stem_multiselect().",
      call. = FALSE
    )
  }

  mapping <- plot$mapping
  cat_name <- stem_aes_name(mapping$y, data)
  fill_name <- stem_aes_name(mapping$fill, data)

  # Inline bars map `y = ""`; give the single bar a category column of its own.
  if (is.null(cat_name)) {
    cat_name <- ".stem_bar"
    data[[cat_name]] <- ""
  }

  col_layers <- vapply(
    plot$layers,
    function(l) inherits(l$geom, "GeomCol") || inherits(l$geom, "GeomBar"),
    logical(1)
  )
  if (!any(col_layers)) {
    stop("`plot` has no bar layer to convert.", call. = FALSE)
  }
  # The last bar layer holds the data; earlier ones are decoration (e.g. the
  # background bars of stem_multiselect()).
  col_layer_idx <- max(which(col_layers))
  layer <- plot$layers[[col_layer_idx]]

  text_layers <- vapply(
    plot$layers,
    function(l) inherits(l$geom, "GeomText"),
    logical(1)
  )
  has_errorbar <- any(vapply(
    plot$layers,
    function(l) inherits(l$geom, "GeomErrorbar"),
    logical(1)
  ))

  type <- if (is.null(fill_name)) {
    "simple"
  } else if (inherits(layer$position, "PositionDodge2") ||
    inherits(layer$position, "PositionDodge")) {
    "dodged"
  } else {
    "stacked"
  }

  # Build the plot once: it trains the scales, which is what makes the
  # level -> colour mapping readable below, and resolves the axis limits.
  built <- ggplot2::ggplot_build(plot)
  scales <- built$plot$scales

  # Colours: ask the fill scale directly, so the mapping level -> colour is
  # exact even when ggplot's internal group indices interact several variables.
  if (is.null(fill_name)) {
    series_levels <- NULL
    colors <- unname(layer$aes_params$fill %||% stem_palette()[1])
  } else {
    scale <- scales$get_scales("fill")
    series_levels <- stem_levels(data[[fill_name]])
    colors <- if (is.null(scale)) {
      rep(stem_palette()[1], length(series_levels))
    } else {
      unname(scale$map(series_levels))
    }
    names(colors) <- series_levels
  }

  # `stem_stack()` and `stem_barplot()` reverse the discrete y scale so the
  # first category sits at the top; `stem_multiselect()` does not. The panel's
  # y labels run bottom-to-top, so comparing them with the natural category
  # order tells the two apart.
  cat_levels <- stem_levels(data[[cat_name]])
  y_labels <- tryCatch(
    as.character(built$layout$panel_params[[1]]$y$get_labels()),
    error = function(e) NULL
  )
  reverse_cat <- !is.null(y_labels) && identical(y_labels, rev(cat_levels))

  theme <- ggplot2::complete_theme(plot$theme)
  family <- theme$text$family %||% ""
  if (!nzchar(family)) {
    family <- "Calibri"
  }

  list(
    data = data,
    cat = cat_name,
    cat_levels = cat_levels,
    value = "freq",
    series = fill_name,
    series_levels = series_levels,
    type = type,
    reverse_cat = reverse_cat,
    colors = colors,
    border = layer$aes_params$colour %||% NA_character_,
    border_width = stem_border_width(built, col_layer_idx),
    labels = any(text_layers),
    # The preformatted ggplot labels, so the exported chart prints exactly the
    # text the plot shows, blanks for `label_hide` included.
    label_text = as.character(data$stem_label %||% NA_character_),
    label_color = data[[".label_color"]],
    num_fmt = stem_num_fmt(data$stem_label),
    title = plot$labels$title,
    has_errorbar = has_errorbar,
    font_family = family,
    font_size = theme$text$size %||% 12,
    # Half-point steps, as PowerPoint itself uses.
    label_size = round(
      2 * stem_label_size(plot, built) * stem_label_export_scale
    ) / 2,
    ink = theme$text$colour %||% "black",
    paper = theme$plot.background$fill %||% "transparent",
    legend_position = theme$legend.position %||% "top"
  )
}

# PowerPoint renders chart text noticeably larger than the ggplot preview does
# at the same nominal size, so the Stem apps draw the numeric labels at 14 pt on
# screen and export them at 11 pt. `stem_label_export_scale` keeps that ratio
# whatever `theme_stem(label_size = )` is set to.
stem_label_export_scale <- 11 / 14

# Typography of the exported charts, in points. These are pinned rather than
# taken from the ggplot theme for the same reason as the label scale above: the
# theme's base size is chosen for an on-screen plot, and carrying it into Office
# makes the axis and legend text far larger than the deck needs.
stem_export_axis_size <- 11
stem_export_axis_title_size <- 12
stem_export_title_size <- 14

#' Border width of the bars of a Stem plot
#'
#' Internal helper reading the width of the separator ggplot2 draws between the
#' bars, in points, so the native chart can reproduce it. mschart wants points
#' where ggplot2 uses millimetres.
#'
#' @param built The result of [ggplot2::ggplot_build()] for the plot.
#' @param layer_idx Index of the bar layer.
#'
#' @return The border width in points.
#' @keywords internal
stem_border_width <- function(built, layer_idx) {
  linewidth <- built$data[[layer_idx]]$linewidth
  linewidth <- stats::median(linewidth, na.rm = TRUE)
  if (!is.finite(linewidth) || linewidth <= 0) {
    linewidth <- 0.5
  }
  linewidth * ggplot2::.pt
}

#' Point size of the numeric labels of a Stem plot
#'
#' Internal helper reading back the size ggplot2 resolved for the in-plot
#' numeric labels. The built layer data carries the value in millimetres,
#' including the default that [theme_stem()] supplies through
#' [ggplot2::element_geom()], so the export follows a custom `label_size`
#' without the caller having to repeat it.
#'
#' @param plot A ggplot object.
#' @param built The result of [ggplot2::ggplot_build()] for `plot`.
#'
#' @return The label size in points. Falls back to `11` when the plot draws no
#'   labels at all.
#' @keywords internal
stem_label_size <- function(plot, built) {
  text_layers <- which(vapply(
    plot$layers,
    function(l) inherits(l$geom, "GeomText") || inherits(l$geom, "GeomLabel"),
    logical(1)
  ))

  for (i in text_layers) {
    size <- built$data[[i]]$size
    size <- size[!is.na(size)]
    if (length(size)) {
      return(size[[1]] * ggplot2::.pt)
    }
  }

  11
}

#' Translate a ggplot theme into an mschart theme
#'
#' Internal helper turning the typography and the "no gridlines, no ticks" look
#' of [theme_stem()] into the equivalent [mschart::mschart_theme()]. The font
#' family and the colours follow the plot's theme; the font *sizes* are the
#' fixed export sizes (11 pt axis and legend text, 12 pt axis titles, 14 pt
#' chart title), matching the decks produced by the Stem apps.
#'
#' @param spec A specification produced by [stem_chart_spec()].
#' @param legend If `TRUE`, the legend is shown.
#'
#' @return An mschart theme.
#' @keywords internal
stem_mschart_theme <- function(spec, legend = TRUE) {
  base <- function(size = stem_export_axis_size, bold = FALSE, color = spec$ink) {
    officer::fp_text(
      font.family = spec$font_family,
      font.size = size,
      bold = bold,
      color = color
    )
  }

  legend_position <- if (!legend) {
    "n"
  } else {
    switch(
      as.character(spec$legend_position)[1],
      top = "t",
      bottom = "b",
      left = "l",
      right = "r",
      none = "n",
      "t"
    )
  }

  background <- if (is.na(spec$paper)) "transparent" else spec$paper

  mschart::mschart_theme(
    main_title = base(size = stem_export_title_size, bold = TRUE),
    axis_title = base(size = stem_export_axis_title_size, bold = TRUE),
    axis_text = base(),
    legend_text = base(),
    grid_major_line = officer::fp_border(width = 0),
    grid_minor_line = officer::fp_border(width = 0),
    axis_ticks = officer::fp_border(width = 0),
    chart_background = background,
    plot_background = background,
    legend_position = legend_position
  )
}

#' Convert a Stem plot into a native Microsoft chart
#'
#' Takes a ggplot2 object created by one of the Stem plotting functions and
#' returns an [mschart::ms_barchart()] that renders as closely as possible to
#' the ggplot original, but is a *native* Office chart: once placed in a
#' PowerPoint or Word file it carries its own embedded worksheet, so the source
#' data can be opened and edited in Excel.
#'
#' The aggregated data, the category order, the palette, the label formatting
#' and the fonts are all read back off the ggplot object, so the native chart
#' needs no recomputation and stays in sync with the ggplot version.
#'
#' A few ggplot features have no Office equivalent and are dropped, with a
#' message: confidence interval error bars (`errorbar = TRUE`) and the
#' background bars of [stem_multiselect()].
#'
#' @param plot A ggplot2 object created by [stem_barplot()], [stem_inline()],
#'   [stem_battery()] or [stem_multiselect()].
#' @param title Chart title. Defaults to the ggplot title (set with
#'   `title_show = TRUE`), or no title when the plot has none. Use `NA` to drop
#'   the title.
#' @param labels If `TRUE`, prints a data label on every segment. Defaults to
#'   whatever the ggplot does.
#' @param legend If `TRUE`, shows a legend. Defaults to `TRUE` for plots with a
#'   fill variable and `FALSE` otherwise, matching the ggplot.
#' @param num_fmt Excel number format code for the data labels, e.g. `"0"`,
#'   `"0.0"` or `"0 \"%\""`. By default the chart prints the plot's own label
#'   text instead of formatting the worksheet values, so the accuracy
#'   (`label_accuracy`) and the blanks left by `label_hide` carry over exactly.
#'   Give a format to label the values from the worksheet instead, which keeps
#'   the labels live when the data are edited in Excel.
#' @param axis_num_fmt Excel number format code for the value axis. Defaults to
#'   `"0\"%\""`, matching the percentage axis of the Stem plots.
#' @param axis_show If `TRUE` (default), draws the value axis.
#' @param axis_major_unit Spacing of the value axis ticks, in percentage
#'   points. Defaults to `25` for stacked charts and `10` otherwise.
#' @param gap_width Gap between bars, as a percentage of the bar width.
#' @param value_name Name of the value column in the embedded worksheet; it is
#'   also the series name of charts without a fill variable.
#'
#' @return An object of class `ms_barchart`, ready for
#'   [officer::ph_with()] / [mschart::body_add_chart()] or for the Stem helpers
#'   [stem_add_chart()], [stem_export_pptx()] and [stem_export_docx()].
#' @export
#' @seealso [stem_add_chart()], [stem_export_pptx()], [stem_export_docx()]
#'
#' @examples \dontrun{
#' chart <- stem_as_mschart(stem_barplot(trust, government))
#'
#' stem_export_pptx(
#'   stem_barplot(trust, government),
#'   stem_battery(trust, items = c(police, eu, government, army)),
#'   path = "charts.pptx"
#' )
#' }
stem_as_mschart <- function(
  plot,
  title = NULL,
  labels = NULL,
  legend = NULL,
  num_fmt = NULL,
  axis_num_fmt = "0\"%\"",
  axis_show = TRUE,
  axis_major_unit = NULL,
  gap_width = 30,
  value_name = "%"
) {
  spec <- stem_chart_spec(plot)

  if (spec$has_errorbar) {
    message(
      "Confidence interval error bars are not supported by native Office ",
      "charts and were dropped."
    )
  }

  if (is.null(labels)) {
    labels <- spec$labels
  }
  if (is.null(legend)) {
    legend <- !is.null(spec$series)
  }
  # With no explicit `num_fmt` the chart prints the plot's own label text, so
  # the accuracy and the `label_hide` blanks of the ggplot carry over.
  label_as_text <- is.null(num_fmt)
  if (is.null(title)) {
    title <- spec$title
  }
  if (!is.null(title) && (is.na(title) || !nzchar(title))) {
    title <- NULL
  }
  # ggplot wraps long titles across lines; Office wraps them itself.
  if (!is.null(title)) {
    title <- gsub("\n", " ", title)
  }

  if (is.null(axis_major_unit)) {
    axis_major_unit <- if (spec$type == "stacked") 25 else 10
  }

  # Office charts are built on percentages (0-100), not proportions, so the
  # embedded worksheet reads the way an analyst would write it by hand.
  chart_data <- spec$data
  chart_data[[value_name]] <- chart_data[[spec$value]] * 100

  # Keep the ggplot ordering; whether the first category ends up at the top or
  # the bottom is then a matter of the category axis orientation set below.
  chart_data[[spec$cat]] <- factor(
    as.character(chart_data[[spec$cat]]),
    levels = spec$cat_levels
  )

  if (!is.null(spec$series)) {
    chart_data[[spec$series]] <- factor(
      as.character(chart_data[[spec$series]]),
      levels = spec$series_levels
    )
  }

  label_name <- NULL
  if (labels && label_as_text && !anyNA(spec$label_text)) {
    label_name <- ".stem_label"
    chart_data[[label_name]] <- spec$label_text
  }

  keep <- c(spec$cat, spec$series, value_name, label_name)
  chart_data <- chart_data[order(chart_data[[spec$cat]]), keep, drop = FALSE]

  chart <- mschart::ms_barchart(
    data = chart_data,
    x = spec$cat,
    y = value_name,
    group = spec$series,
    labels = label_name
  )

  chart <- switch(
    spec$type,
    stacked = mschart::as_bar_stack(
      chart,
      dir = "horizontal",
      gap_width = gap_width
    ),
    dodged = mschart::chart_settings(
      chart,
      dir = "horizontal",
      grouping = "clustered",
      gap_width = gap_width,
      overlap = -10
    ),
    mschart::chart_settings(
      chart,
      dir = "horizontal",
      grouping = "clustered",
      gap_width = gap_width
    )
  )

  # Series colours, plus the white separator the Stem plots draw between bars.
  fills <- if (is.null(spec$series)) {
    stats::setNames(list(unname(spec$colors)[1]), value_name)
  } else {
    as.list(spec$colors)
  }
  chart <- mschart::chart_data_fill(chart, values = fills, update_stroke = FALSE)
  if (!is.na(spec$border)) {
    strokes <- lapply(fills, function(x) spec$border)
    chart <- mschart::chart_data_stroke(chart, values = strokes)
    chart <- mschart::chart_data_line_width(chart, values = spec$border_width)
  }

  chart <- mschart::chart_labels(
    chart,
    title = title,
    xlab = NULL,
    ylab = NULL
  )

  if (labels) {
    position <- if (spec$type == "stacked") "ctr" else "outEnd"
    chart <- if (is.null(label_name)) {
      mschart::chart_data_labels(
        chart,
        show_val = TRUE,
        num_fmt = num_fmt %||% spec$num_fmt,
        position = position
      )
    } else {
      mschart::chart_data_labels(chart, show_val = FALSE, position = position)
    }
    chart <- mschart::chart_labels_text(
      chart,
      values = stem_label_fp(spec, names(fills))
    )
  }

  chart <- mschart::set_theme(chart, stem_mschart_theme(spec, legend = legend))

  # `x` is the category axis of a bar chart. Office, like ggplot, draws the
  # first category at the bottom; `maxMin` flips it to match the reversed
  # discrete scale used by most of the Stem plots.
  chart <- mschart::chart_ax_x(
    chart,
    orientation = if (spec$reverse_cat) "maxMin" else "minMax",
    major_tick_mark = "none",
    minor_tick_mark = "none"
  )

  # The value axis crosses the category axis at its first category, which a
  # reversed category axis puts at the top. Cross at the last category instead,
  # so the percentage labels stay below the bars as they do in ggplot.
  chart <- mschart::chart_ax_y(
    chart,
    display = axis_show,
    num_fmt = axis_num_fmt,
    major_tick_mark = "none",
    minor_tick_mark = "none",
    crosses = if (spec$reverse_cat) "max" else "autoZero",
    limit_min = 0,
    limit_max = if (spec$type == "stacked") 100 else NULL,
    major_unit = axis_major_unit
  )

  chart
}

#' Data label colours for a native chart
#'
#' Internal helper reproducing the bicolour labels of the stacked Stem plots:
#' the two extreme (side) categories are labelled in white, everything else in
#' the theme's foreground colour.
#'
#' @param spec A specification produced by [stem_chart_spec()].
#' @param series_names Names of the chart series.
#'
#' @return A named list of [officer::fp_text()] objects, one per series.
#' @keywords internal
stem_label_fp <- function(spec, series_names) {
  colours <- stats::setNames(rep(spec$ink, length(series_names)), series_names)

  # Mirror stem_stack()'s `label_bicolor`: white on the first and last series.
  if (!is.null(spec$series) &&
    identical(spec$type, "stacked") &&
    !is.null(spec$label_color) &&
    any(spec$label_color == "white")) {
    side <- series_names[c(1L, length(series_names))]
    colours[side] <- "white"
  }

  lapply(colours, function(col) {
    officer::fp_text(
      font.family = spec$font_family,
      font.size = spec$label_size %||% spec$font_size,
      color = col
    )
  })
}

#' Left-align the titles of the charts in an Office file
#'
#' Internal helper patching the chart parts of a written `.pptx`/`.docx`.
#' Office centres a chart title, whereas [theme_stem()] sets
#' `plot.title.position = "plot"`, which flushes it left above the whole plot.
#' mschart exposes no alignment setting, so the paragraph property is written
#' into the chart XML afterwards, as the Stem apps do.
#'
#' Silently leaves the file untouched when the `zip` package is missing or the
#' file cannot be rewritten: the alignment is cosmetic and never worth failing
#' an export over.
#'
#' @param path Path of the Office file to patch.
#'
#' @return The path, invisibly.
#' @keywords internal
stem_leftalign_titles <- function(path) {
  if (!requireNamespace("zip", quietly = TRUE)) {
    return(invisible(path))
  }

  tryCatch(
    {
      dir <- tempfile()
      dir.create(dir)
      on.exit(unlink(dir, recursive = TRUE), add = TRUE)
      utils::unzip(path, exdir = dir)

      charts <- list.files(
        dir,
        pattern = "^chart[0-9a-f]*\\.xml$",
        recursive = TRUE,
        full.names = TRUE
      )

      patched <- FALSE
      for (chart in charts) {
        xml <- paste(
          readLines(chart, warn = FALSE, encoding = "UTF-8"),
          collapse = "\n"
        )
        # The first <a:pPr> inside <c:title> is the title's own paragraph.
        aligned <- sub(
          "(?s)(<c:title[ >].*?)<a:pPr>",
          "\\1<a:pPr algn=\"l\">",
          xml,
          perl = TRUE
        )
        if (!identical(xml, aligned)) {
          writeLines(aligned, chart, useBytes = TRUE)
          patched <- TRUE
        }
      }

      if (patched) {
        rezipped <- tempfile(fileext = ".zip")
        zip::zip(
          zipfile = rezipped,
          files = list.files(dir, recursive = TRUE, all.files = TRUE, no.. = TRUE),
          root = dir,
          include_directories = FALSE
        )
        file.copy(rezipped, path, overwrite = TRUE)
        unlink(rezipped)
      }
    },
    error = function(e) NULL
  )

  invisible(path)
}

# Office documents --------------------------------------------------------

#' Convert a chart size to inches
#'
#' Internal helper. officer measures everything in inches; the Stem export
#' helpers take centimetres, which is what the analysts' slide templates use.
#'
#' @param x Numeric size, or `NULL`.
#' @param units `"cm"`, `"mm"` or `"in"`.
#'
#' @return The size in inches, or `NULL` when `x` is `NULL`.
#' @keywords internal
stem_to_inches <- function(x, units = "cm") {
  if (is.null(x)) {
    return(NULL)
  }
  units <- match.arg(units, c("cm", "mm", "in"))
  switch(units, cm = x / 2.54, mm = x / 25.4, `in` = x)
}

#' Where to place a chart on a slide
#'
#' Internal helper turning the size arguments of the PowerPoint helpers into an
#' officer location. Falls back to the layout's body placeholder when no size is
#' given, so a template's own geometry is respected.
#'
#' @param width,height Size of the chart, in `units`. `NULL` uses the body
#'   placeholder of the slide layout.
#' @param left,top Position of the chart's top-left corner, in `units`.
#' @param units Unit of `width`, `height`, `left` and `top`.
#'
#' @return An officer location object.
#' @keywords internal
stem_ph_location <- function(
  width = NULL,
  height = NULL,
  left = NULL,
  top = NULL,
  units = "cm"
) {
  if (is.null(width) && is.null(height) && is.null(left) && is.null(top)) {
    return(officer::ph_location_type(type = "body"))
  }

  if (is.null(width) || is.null(height)) {
    stop(
      "Give both `width` and `height` when positioning a chart by hand.",
      call. = FALSE
    )
  }

  officer::ph_location(
    left = stem_to_inches(left %||% 0, units),
    top = stem_to_inches(top %||% 0, units),
    width = stem_to_inches(width, units),
    height = stem_to_inches(height, units)
  )
}


#' Add a Stem plot to a PowerPoint or Word document as a native chart
#'
#' Converts `plot` with [stem_as_mschart()] and adds it to an existing officer
#' document. For a PowerPoint document the chart is placed on the current
#' slide, so add a slide with [officer::add_slide()] first.
#'
#' @param x An [officer::read_pptx()] or [officer::read_docx()] document.
#' @param plot A ggplot2 object created by a Stem plotting function, or an
#'   `ms_barchart` already produced by [stem_as_mschart()].
#' @param width,height Size of the chart, in `units`. In Word both default to a
#'   readable 15 x 10 cm. In PowerPoint both default to `NULL`, which uses the
#'   body placeholder of the slide layout; give them to size the chart by hand.
#' @param left,top PowerPoint only: position of the chart's top-left corner, in
#'   `units`. Only used when `width` and `height` are given; both default to
#'   `0`.
#' @param units Unit of `width`, `height`, `left` and `top`: `"cm"` (default),
#'   `"mm"` or `"in"`.
#' @param location PowerPoint only: an [officer::ph_location()] giving where to
#'   place the chart. Takes precedence over `width`/`height`/`left`/`top`.
#' @param ... Passed to [stem_as_mschart()].
#'
#' @return The document `x`, updated.
#' @export
#' @seealso [stem_as_mschart()]
#'
#' @examples \dontrun{
#' doc <- officer::read_pptx()
#' doc <- officer::add_slide(doc, "Title and Content", "Office Theme")
#' doc <- stem_add_chart(doc, stem_barplot(trust, government))
#' print(doc, target = "chart.pptx")
#' }
stem_add_chart <- function(
  x,
  plot,
  width = NULL,
  height = NULL,
  left = NULL,
  top = NULL,
  units = "cm",
  location = NULL,
  ...
) {
  chart <- if (inherits(plot, "ms_chart")) plot else stem_as_mschart(plot, ...)

  if (inherits(x, "rpptx")) {
    if (is.null(location)) {
      location <- stem_ph_location(width, height, left, top, units)
    }
    return(officer::ph_with(x, chart, location = location))
  }

  if (inherits(x, "rdocx")) {
    return(mschart::body_add_chart(
      x,
      chart,
      width = stem_to_inches(width %||% 15, units),
      height = stem_to_inches(height %||% 10, units)
    ))
  }

  stop(
    "`x` must be an officer PowerPoint (`rpptx`) or Word (`rdocx`) document.",
    call. = FALSE
  )
}

#' Export Stem plots to PowerPoint as native charts
#'
#' Writes one slide per plot, each holding a native Office chart whose source
#' data can be edited in Excel.
#'
#' @param ... One or more ggplot2 objects created by the Stem plotting
#'   functions (or `ms_barchart` objects from [stem_as_mschart()]). A single
#'   list of plots is also accepted.
#' @param path Path of the `.pptx` file to write.
#' @param template Optional path to a `.pptx` template to build on. Defaults to
#'   officer's blank presentation.
#' @param layout,master Name of the slide layout and of the master to use for
#'   the new slides.
#' @param width,height Size of each chart, in `units`. Both default to `NULL`,
#'   which uses the body placeholder of `layout`, so a template's own geometry
#'   is respected. Give them to size the charts by hand.
#' @param left,top Position of each chart's top-left corner, in `units`. Only
#'   used when `width` and `height` are given; both default to `0`.
#' @param units Unit of `width`, `height`, `left` and `top`: `"cm"` (default),
#'   `"mm"` or `"in"`.
#' @param location An [officer::ph_location()] giving where to place the charts.
#'   Takes precedence over `width`/`height`/`left`/`top`.
#' @param chart_args A named list of arguments passed on to
#'   [stem_as_mschart()].
#'
#' @return The path, invisibly.
#' @export
#' @seealso [stem_as_mschart()], [stem_export_docx()]
#'
#' @examples \dontrun{
#' stem_export_pptx(
#'   stem_barplot(trust, government, title_show = TRUE),
#'   stem_inline(trust, police),
#'   path = "trust.pptx"
#' )
#'
#' # A 24 x 12 cm chart, 2 cm from the left and 4 cm from the top of the slide.
#' stem_export_pptx(
#'   stem_barplot(trust, government),
#'   path = "trust.pptx",
#'   width = 24, height = 12, left = 2, top = 4
#' )
#' }
stem_export_pptx <- function(
  ...,
  path,
  template = NULL,
  layout = "Title and Content",
  master = "Office Theme",
  width = NULL,
  height = NULL,
  left = NULL,
  top = NULL,
  units = "cm",
  location = NULL,
  chart_args = list()
) {
  plots <- stem_plot_list(...)
  if (is.null(location)) {
    location <- stem_ph_location(width, height, left, top, units)
  }

  doc <- if (is.null(template)) {
    officer::read_pptx()
  } else {
    officer::read_pptx(template)
  }

  for (plot in plots) {
    chart <- stem_chart_from(plot, chart_args)
    doc <- officer::add_slide(doc, layout = layout, master = master)
    doc <- stem_add_chart(doc, chart, location = location)
  }

  print(doc, target = path)
  stem_leftalign_titles(path)
  invisible(path)
}

#' Export Stem plots to Word as native charts
#'
#' Writes a `.docx` file holding one native Office chart per plot, each with an
#' embedded worksheet that can be edited in Excel.
#'
#' @inheritParams stem_export_pptx
#' @param path Path of the `.docx` file to write.
#' @param template Optional path to a `.docx` template to build on.
#' @param width,height Size of each chart, in `units`. Defaults to 15 x 10 cm,
#'   which fits the text width of a portrait A4 page.
#'
#' @return The path, invisibly.
#' @export
#' @seealso [stem_as_mschart()], [stem_export_pptx()]
#'
#' @examples \dontrun{
#' stem_export_docx(stem_barplot(trust, government), path = "trust.docx")
#'
#' # Half-page charts.
#' stem_export_docx(
#'   stem_barplot(trust, government),
#'   path = "trust.docx",
#'   width = 16, height = 8
#' )
#' }
stem_export_docx <- function(
  ...,
  path,
  template = NULL,
  width = 15,
  height = 10,
  units = "cm",
  chart_args = list()
) {
  plots <- stem_plot_list(...)

  doc <- if (is.null(template)) {
    officer::read_docx()
  } else {
    officer::read_docx(template)
  }

  for (plot in plots) {
    chart <- stem_chart_from(plot, chart_args)
    doc <- mschart::body_add_chart(
      doc,
      chart,
      width = stem_to_inches(width, units),
      height = stem_to_inches(height, units)
    )
  }

  print(doc, target = path)
  stem_leftalign_titles(path)
  invisible(path)
}

#' Collect plots passed through `...`
#'
#' Internal helper accepting either several plots in `...` or a single list of
#' plots.
#'
#' @param ... Plots, or one list of plots.
#'
#' @return A list of plots.
#' @keywords internal
stem_plot_list <- function(...) {
  plots <- list(...)
  if (length(plots) == 1L && is.list(plots[[1]]) && !inherits(plots[[1]], "gg")) {
    plots <- plots[[1]]
  }
  if (!length(plots)) {
    stop("No plots to export.", call. = FALSE)
  }
  plots
}

#' Convert a plot unless it already is a chart
#'
#' @param plot A ggplot or an `ms_chart`.
#' @param chart_args Arguments passed to [stem_as_mschart()].
#'
#' @return An `ms_chart`.
#' @keywords internal
stem_chart_from <- function(plot, chart_args = list()) {
  if (inherits(plot, "ms_chart")) {
    return(plot)
  }
  do.call(stem_as_mschart, c(list(plot), chart_args))
}
