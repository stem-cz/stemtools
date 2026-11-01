# Frequency helpers -------------------------------------------------------

#' Format proportions as percentage labels
#'
#' Internal helper used to turn proportions into ready-to-plot percentage
#' labels. Proportions below `hide` are returned as empty strings so that small
#' segments stay unlabelled.
#'
#' @param x Numeric vector of proportions (between 0 and 1).
#' @param accuracy Rounding accuracy passed to [scales::percent()].
#' @param suffix Suffix appended to each label.
#' @param hide Proportions smaller than this are returned as empty strings.
#'
#' @return Character vector of formatted labels.
#' @keywords internal
format_pct <- function(x, accuracy = 1, suffix = " %", hide = 0) {
  label <- scales::percent(x, accuracy = accuracy, suffix = suffix)
  label[x < hide] <- ""
  label
}

#' Build a plot title from a variable label or name
#'
#' Internal helper that returns the `"label"` attribute of `data[[item_name]]`,
#' falling back to `item_name` when the variable has no label. Optionally wraps
#' the title in Czech low/high double quotation marks.
#'
#' @param data Data frame holding the item variable.
#' @param item_name Name of the item column.
#' @param quote If `TRUE`, wrap the title in `„` and `“`.
#' @param wrap Maximum number of characters per line. Titles longer than this
#'   are wrapped onto several lines. Use `NULL` or `Inf` to disable wrapping.
#'
#' @return A length-one character vector.
#' @keywords internal
stem_plot_title <- function(data, item_name, quote = FALSE, wrap = 73) {
  label <- attr(data[[item_name]], "label")
  title <- if (is.null(label) || is.na(label)) {
    item_name
  } else {
    as.character(label)
  }

  format_title(title, quote = quote, wrap = wrap)
}

#' Wrap and quote a plot title
#'
#' Internal helper shared by [stem_plot_title()] and the `title` argument of
#' [stem_battery()].
#'
#' @param title A length-one character vector.
#' @inheritParams stem_plot_title
#'
#' @return A length-one character vector.
#' @keywords internal
format_title <- function(title, quote = FALSE, wrap = 73) {
  if (!is.null(wrap) && is.finite(wrap)) {
    title <- paste(strwrap(title, width = wrap), collapse = "\n")
  }

  if (quote) {
    title <- paste0("\u201e", title, "\u201c")
  }

  title
}

#' Compute plotting frequencies for a categorical variable
#'
#' Connects the frequency engine [stem_summarise_cat()] to the Stem plotting
#' functions. Returns (possibly weighted) proportions, their 95% confidence
#' intervals and a preformatted percentage label. Proportions are computed
#' within `group` when a grouping variable is supplied.
#'
#' @inheritParams stem_barplot
#'
#' @return A tidy data frame with the item (and group) variable and columns
#'   `freq`, `freq_low`, `freq_upp` and `stem_label`.
#' @keywords internal
stem_plot_data <- function(
  data,
  item,
  group = NULL,
  weight = NULL,
  collapse_item = NULL,
  collapse_group = NULL,
  label_accuracy = 1,
  label_suffix = "",
  label_hide = 0,
  dk_levels = NULL
) {
  item_name <- rlang::as_name(rlang::enquo(item))

  plot_data <- stem_summarise_cat(
    data = data,
    item = {{ item }},
    group = {{ group }},
    weight = {{ weight }},
    collapse_item = collapse_item,
    collapse_group = collapse_group,
    # Small cells are common in plots; the warning would just be noise.
    min_cell_n = NULL
  )

  if (!is.null(dk_levels)) {
    plot_data[[item_name]] <- move_dk_last(plot_data[[item_name]], dk_levels)
  }

  plot_data$stem_label <- format_pct(
    plot_data$freq,
    accuracy = label_accuracy,
    suffix = label_suffix,
    hide = label_hide
  )

  plot_data
}

#' Move "don't know" levels to the end
#'
#' Internal helper. Levels of `x` named in `dk_levels` are moved after all
#' other levels; `dk_levels` absent from `x` are ignored.
#'
#' @param x A factor or character vector.
#' @param dk_levels Character vector of "don't know" levels.
#'
#' @return A factor.
#' @keywords internal
move_dk_last <- function(x, dk_levels) {
  x <- as.factor(x)
  dk <- intersect(dk_levels, levels(x))
  if (!length(dk)) {
    return(x)
  }
  forcats::fct_relevel(x, dk, after = Inf)
}

#' Greys for "don't know" levels
#'
#' Internal helper returning `n` fill colours for "don't know" levels. When
#' `dk_color` holds fewer than `n` colours, shades are derived from its first
#' colour towards a darker tone, so that several levels can be told apart.
#'
#' @param n Number of "don't know" levels.
#' @param dk_color Colour, or vector of colours, for the "don't know" levels.
#'
#' @return Character vector of `n` hex colours.
#' @keywords internal
dk_colors <- function(n, dk_color = "grey70") {
  if (n == 0) {
    return(character())
  }
  colors <- if (length(dk_color) >= n) {
    dk_color[seq_len(n)]
  } else {
    darker <- grDevices::colorRampPalette(c(dk_color[1], "black"))(21)[8]
    grDevices::colorRampPalette(c(dk_color[1], darker))(n)
  }
  as_hex(colors)
}

#' Convert colours to hex codes
#'
#' @param x Character vector of colour names or hex codes.
#' @return Character vector of `#RRGGBB` codes.
#' @keywords internal
as_hex <- function(x) {
  grDevices::rgb(t(grDevices::col2rgb(x)), maxColorValue = 255)
}

#' Fill colours of the categories of a stacked bar
#'
#' Internal helper. The palette is spread over the substantive levels only;
#' "don't know" levels are drawn grey and do not count towards the palette
#' size.
#'
#' @param levels Character vector of fill levels, in plotting order.
#' @inheritParams stem_barplot
#'
#' @return Named character vector of colours, one per level.
#' @keywords internal
stem_fill_values <- function(
  levels,
  palette = "div1",
  direction = 1,
  dk_levels = NULL,
  dk_color = "grey70"
) {
  is_dk <- levels %in% dk_levels
  colors <- character(length(levels))
  if (any(!is_dk)) {
    colors[!is_dk] <- stem_palette_gen(palette, direction)(sum(!is_dk))
  }
  colors[is_dk] <- dk_colors(sum(is_dk), dk_color)
  stats::setNames(colors, levels)
}

#' Label colour that contrasts with a fill
#'
#' Internal helper. A fill whose Rec. 601 luma (`0.299 R + 0.587 G + 0.114 B`,
#' on a 0-255 scale) is below `threshold` counts as dark and gets a white
#' label; lighter fills get `light`. The threshold of 150 keeps the labels of
#' every Stem diverging palette as they were drawn before (white on the
#' extremes, dark on the light middle colours).
#'
#' @param fill Character vector of fill colours, possibly named.
#' @param light Label colour used on light fills.
#' @param threshold Luma below which a fill counts as dark.
#'
#' @return Character vector of label colours, with the names of `fill`.
#' @keywords internal
contrast_label_color <- function(fill, light = "black", threshold = 150) {
  rgb <- grDevices::col2rgb(fill)
  luma <- colSums(rgb * c(0.299, 0.587, 0.114))
  stats::setNames(ifelse(luma < threshold, "white", light), names(fill))
}

#' Build a stacked horizontal bar plot
#'
#' Internal renderer shared by [stem_barplot()] and [stem_inline()]. Draws a
#' single stacked horizontal bar per `y_name` category (or one bar in total when
#' `y_name` is `NULL`).
#'
#' @param plot_data Data produced by [stem_plot_data()].
#' @param fill_name Name of the item column mapped to `fill`.
#' @param y_name Name of the group column mapped to the y axis, or `NULL` for a
#'   single (inline) bar.
#' @inheritParams stem_barplot
#'
#' @return A ggplot2 object.
#' @keywords internal
stem_stack <- function(
  plot_data,
  fill_name,
  y_name = NULL,
  palette = "div1",
  direction = 1,
  labels = TRUE,
  label_color = "black",
  label_bicolor = TRUE,
  dk_levels = NULL,
  dk_color = "grey70"
) {
  # Colours follow the full scale: every level of the item, empty ones too
  # (they reach the plot as 0 % rows), so an empty point shifts no colour.
  fill_levels <- levels(as.factor(plot_data[[fill_name]]))
  fill_values <- stem_fill_values(
    fill_levels,
    palette = palette,
    direction = direction,
    dk_levels = dk_levels,
    dk_color = dk_color
  )

  # Pick each level's label colour from its fill: white on dark fills,
  # `label_color` on light ones.
  if (label_bicolor) {
    level_colors <- contrast_label_color(fill_values, light = label_color)
    plot_data$.label_color <- unname(
      level_colors[as.character(plot_data[[fill_name]])]
    )
  } else {
    plot_data$.label_color <- label_color
  }

  mapping <- if (is.null(y_name)) {
    ggplot2::aes(
      x = freq,
      y = "",
      fill = .data[[fill_name]],
      label = stem_label
    )
  } else {
    ggplot2::aes(
      x = freq,
      y = .data[[y_name]],
      fill = .data[[fill_name]],
      label = stem_label
    )
  }

  p <- ggplot2::ggplot(plot_data, mapping) +
    ggplot2::geom_col(
      color = "white",
      position = ggplot2::position_stack(reverse = TRUE)
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::label_percent(suffix = " %"),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::scale_y_discrete(limits = rev) +
    ggplot2::scale_fill_manual(values = fill_values, breaks = fill_levels) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))

  if (labels) {
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(
          colour = .data[[".label_color"]],
          group = .data[[fill_name]]
        ),
        position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE)
      ) +
      ggplot2::scale_colour_identity()
  }

  p
}

# Plotting functions ------------------------------------------------------

#' Horizontal bar plot
#'
#' Plots the (possibly weighted) distribution of a categorical variable as
#' horizontal bars. Without a `group`, one bar is drawn per item category.
#' Supplying a `group` variable instead draws one stacked horizontal bar per
#' group category, with the item mapped to fill; proportions are then computed
#' within each group, so every bar sums to 100%.
#'
#' @param data Data frame holding the item (and group) variables.
#' @param item Categorical variable to plot.
#' @param group Optional grouping variable. When supplied, one stacked
#'   horizontal bar is drawn per group category (with the item mapped to fill)
#'   and proportions are computed within each group.
#' @param weight Optional survey weights.
#' @param collapse_item Optional named list passed to [stem_summarise_cat()] to
#'   collapse (or rename) item categories.
#' @param collapse_group Optional named list to collapse (or rename) group
#'   categories.
#' @param palette Name of a Stem palette. See [stem_palette()].
#' @param direction Palette direction. Use `-1` to reverse the colours.
#' @param labels If `TRUE`, prints a percentage label on each bar.
#' @param label_accuracy Rounding accuracy of labels. `1` gives whole numbers,
#'   `0.1` one decimal place.
#' @param label_suffix Suffix appended to labels. Defaults to `""`.
#' @param label_hide Proportions below this threshold are left unlabelled.
#'   Defaults to `0.05` when a `group` is supplied (to keep small stacked
#'   segments unlabelled) and `0` otherwise.
#' @param label_color Colour of the segment labels in grouped (stacked) plots.
#'   Defaults to `"black"`.
#' @param label_bicolor If `TRUE` (default), in grouped (stacked) plots each
#'   label's colour is chosen from its fill: white on dark fills, `label_color`
#'   on light ones (by luma, see `contrast_label_color()`). With the diverging
#'   palettes this draws the extreme categories in white. Set to `FALSE` to
#'   colour every label with `label_color`.
#' @param errorbar If `TRUE`, adds 95% confidence interval error bars (ungrouped
#'   plots only).
#' @param title_show If `TRUE`, adds a plot title taken from the item's `"label"`
#'   attribute, falling back to the variable name when no label is present. The
#'   title is styled (e.g. drawn in bold) by [theme_stem()]. Defaults to `FALSE`.
#' @param title_quote If `TRUE`, wraps the title in low/high double quotation
#'   marks (`„` and `“`). Defaults to `FALSE`.
#' @param title_wrap Maximum number of characters per title line; longer titles
#'   are wrapped onto several lines so they do not overflow the plot. Use `NULL`
#'   or `Inf` to disable wrapping. Defaults to `73`, measured so that a line of
#'   15.6 pt bold Calibri (the [theme_stem()] title) fits a chart 19 cm wide.
#' @param dk_levels Optional character vector of "don't know" categories of the
#'   item (e.g. `"Nevím"`). They are moved to the end (the right end of stacked
#'   bars and of the legend, the bottom of single-colour bar charts) and drawn
#'   grey, and they do not count towards the palette size. Levels not present
#'   in the data are ignored. Defaults to `NULL`.
#' @param dk_color Fill colour of the "don't know" categories. Defaults to
#'   `"grey70"` (`#B3B3B3`). Give several colours for several "don't know"
#'   levels; otherwise distinguishable darker greys are derived from the first.
#' @param bar_color Fill of single-series bars (ungrouped [stem_barplot()] and
#'   [stem_multiselect()]). Defaults to the Stem brand burgundy `"#82143C"`,
#'   whatever the `palette`.
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples \dontrun{
#' stem_barplot(trust, government)
#' stem_barplot(trust, police, group = eu_index, weight = W)
#'
#' # A "don't know" answer is drawn grey at the end of the bar.
#' trust$police_dk <- forcats::fct_expand(trust$police, "Don't know")
#' trust$police_dk[1:80] <- "Don't know"
#' stem_inline(trust, police_dk, dk_levels = "Don't know")
#' }
stem_barplot <- function(
  data,
  item,
  group = NULL,
  weight = NULL,
  collapse_item = NULL,
  collapse_group = NULL,
  palette = "div1",
  direction = 1,
  labels = TRUE,
  label_accuracy = 1,
  label_suffix = "",
  label_hide = NULL,
  label_color = "black",
  label_bicolor = TRUE,
  errorbar = FALSE,
  title_show = FALSE,
  title_quote = FALSE,
  title_wrap = 73,
  dk_levels = NULL,
  dk_color = "grey70",
  bar_color = "#82143C"
) {
  item_name <- rlang::as_name(rlang::enquo(item))
  group_quo <- rlang::enquo(group)
  has_group <- !rlang::quo_is_null(group_quo)

  # Hide small stacked segments by default when grouped; label everything when
  # showing one bar per category.
  if (is.null(label_hide)) {
    label_hide <- if (has_group) 0.05 else 0
  }

  plot_data <- stem_plot_data(
    data = data,
    item = {{ item }},
    group = {{ group }},
    weight = {{ weight }},
    collapse_item = collapse_item,
    collapse_group = collapse_group,
    label_accuracy = label_accuracy,
    label_suffix = label_suffix,
    label_hide = label_hide,
    dk_levels = dk_levels
  )

  # With a grouping variable, draw one stacked horizontal bar per group
  # category (item mapped to fill), so each group sums to 100%.
  if (has_group) {
    group_name <- rlang::as_name(group_quo)
    p <- stem_stack(
      plot_data = plot_data,
      fill_name = item_name,
      y_name = group_name,
      palette = palette,
      direction = direction,
      labels = labels,
      label_color = label_color,
      label_bicolor = label_bicolor,
      dk_levels = dk_levels,
      dk_color = dk_color
    )
    if (title_show) {
      p <- p +
        ggplot2::labs(
          title = stem_plot_title(data, item_name, title_quote, title_wrap)
        )
    }
    return(p)
  }

  # Ungrouped: one bar per item category, all in one colour.
  dk_present <- intersect(dk_levels, as.character(plot_data[[item_name]]))
  if (!length(dk_present)) {
    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = freq, y = .data[[item_name]], label = stem_label)
    ) +
      ggplot2::geom_col(color = "white", fill = bar_color)
  } else {
    # Office charts colour whole series, not single bars, so "don't know"
    # bars form series of their own (`.series`), drawn grey. Each category
    # still has one bar: the series overlap rather than stack.
    series_levels <- c("%", dk_present)
    plot_data$.series <- factor(
      ifelse(
        as.character(plot_data[[item_name]]) %in% dk_present,
        as.character(plot_data[[item_name]]),
        "%"
      ),
      levels = series_levels
    )
    series_values <- stats::setNames(
      c(bar_color, dk_colors(length(dk_present), dk_color)),
      series_levels
    )
    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = freq,
        y = .data[[item_name]],
        fill = .data[[".series"]],
        label = stem_label
      )
    ) +
      ggplot2::geom_col(color = "white", position = ggplot2::position_identity()) +
      ggplot2::scale_fill_manual(values = series_values, guide = "none")
  }

  p <- p +
    ggplot2::scale_y_discrete(limits = rev) +
    ggplot2::scale_x_continuous(
      labels = scales::label_percent(suffix = " %"),
      expand = ggplot2::expansion(mult = c(0, 0.1))
    )

  if (errorbar) {
    p <- p +
      ggplot2::geom_errorbar(
        ggplot2::aes(xmin = freq_low, xmax = freq_upp),
        width = 0.2,
        position = "identity"
      )
  }

  if (labels) {
    p <- p +
      ggplot2::geom_text(hjust = -0.2, position = ggplot2::position_identity())
  }

  if (title_show) {
    p <- p +
      ggplot2::labs(
        title = stem_plot_title(data, item_name, title_quote, title_wrap)
      )
  }

  p
}

#' Inline stacked bar plot
#'
#' Plots the (possibly weighted) distribution of a single categorical variable
#' as one stacked horizontal bar. Useful as a compact, inline summary of a
#' single item.
#'
#' @inheritParams stem_barplot
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples \dontrun{
#' stem_inline(trust, police)
#' stem_inline(trust, police, weight = W)
#' }
stem_inline <- function(
  data,
  item,
  weight = NULL,
  collapse_item = NULL,
  palette = "div1",
  direction = 1,
  labels = TRUE,
  label_accuracy = 1,
  label_suffix = "",
  label_hide = 0.05,
  label_color = "black",
  label_bicolor = TRUE,
  title_show = FALSE,
  title_quote = FALSE,
  title_wrap = 73,
  dk_levels = NULL,
  dk_color = "grey70"
) {
  item_name <- rlang::as_name(rlang::enquo(item))

  plot_data <- stem_plot_data(
    data = data,
    item = {{ item }},
    weight = {{ weight }},
    collapse_item = collapse_item,
    label_accuracy = label_accuracy,
    label_suffix = label_suffix,
    label_hide = label_hide,
    dk_levels = dk_levels
  )

  p <- stem_stack(
    plot_data = plot_data,
    fill_name = item_name,
    y_name = NULL,
    palette = palette,
    direction = direction,
    labels = labels,
    label_color = label_color,
    label_bicolor = label_bicolor,
    dk_levels = dk_levels,
    dk_color = dk_color
  )

  if (title_show) {
    p <- p +
      ggplot2::labs(
        title = stem_plot_title(data, item_name, title_quote, title_wrap)
      )
  }

  p
}

# Multi-item plots --------------------------------------------------------

#' Extract variable labels
#'
#' Returns the `"label"` attribute of each column, or `NA` when it is missing.
#'
#' @param data A data frame.
#' @return A named character vector of labels, one per column.
#' @keywords internal
column_labels <- function(data) {
  labels <- vapply(
    data,
    function(x) {
      label <- attr(x, "label")
      if (is.null(label)) NA_character_ else as.character(label)
    },
    character(1)
  )
  stats::setNames(labels, names(data))
}

#' Display names of a set of items
#'
#' Internal helper shared by [stem_battery()] and [stem_multiselect()]: the
#' items' `"label"` attributes, or their variable names when `item_label` is
#' `FALSE` or (with a warning) when any item lacks a label.
#'
#' @param item_data A data frame holding only the items.
#' @param item_label If `TRUE`, use the labels.
#'
#' @return A character vector, one entry per item.
#' @keywords internal
item_display_names <- function(item_data, item_label = TRUE) {
  item_names <- names(item_data)
  if (!item_label) {
    return(item_names)
  }
  labels_lookup <- column_labels(item_data)
  if (anyNA(labels_lookup)) {
    warning(
      "At least one item has no `label` attribute; using variable names instead."
    )
    return(item_names)
  }
  unname(labels_lookup[item_names])
}

#' Shared response categories of a battery
#'
#' Internal helper checking that the items of a battery can be stacked into one
#' response column without losing answers. Factor items must have the same
#' levels in the same order; character items may only use those levels.
#'
#' @param item_data A data frame holding only the battery items.
#'
#' @return The shared levels, or `NULL` when no item is a factor (character
#'   items are combined as they are, so nothing can be lost).
#' @keywords internal
battery_categories <- function(item_data) {
  is_fct <- vapply(item_data, is.factor, logical(1))
  if (!any(is_fct)) {
    return(NULL)
  }

  ref_name <- names(item_data)[which(is_fct)[1]]
  ref <- levels(item_data[[ref_name]])
  quote_cats <- function(x) paste0("\"", x, "\"", collapse = ", ")

  problems <- character()
  for (name in names(item_data)) {
    x <- item_data[[name]]
    cats <- if (is.factor(x)) levels(x) else unique(as.character(x[!is.na(x)]))
    if (is.factor(x) && identical(cats, ref)) next
    extra <- setdiff(cats, ref)
    missing <- if (is.factor(x)) setdiff(ref, cats) else character()
    if (!is.factor(x) && !length(extra)) next

    detail <- c(
      if (length(missing)) paste("lacks", quote_cats(missing)),
      if (length(extra)) paste("has", quote_cats(extra))
    )
    if (!length(detail)) detail <- "has the same categories in a different order"
    problems <- c(problems, paste0("`", name, "` ", paste(detail, collapse = " and ")))
  }

  if (length(problems)) {
    stop(
      "All battery items must have the same categories in the same order.\n",
      "Compared with `", ref_name, "` (", quote_cats(ref), "):\n",
      paste0("* ", problems, collapse = "\n"),
      call. = FALSE
    )
  }

  ref
}

#' Stacked bar plot for a battery of like items
#'
#' Plots a set of categorical variables that share the same response categories
#' (e.g. a battery of Likert items) as a single chart, drawing one stacked
#' horizontal bar per item. Internally the items are reshaped to long format and
#' handed to the same machinery as [stem_barplot()], with the item taking the
#' role of the group.
#'
#' Factor items must have the same levels in the same order (and character items
#' may only use those levels); otherwise the function stops with an error naming
#' the items and categories that differ, rather than dropping answers.
#'
#' @inheritParams stem_barplot
#' @param items Items to plot. Supports tidyselect helpers (e.g.
#'   `dplyr::starts_with()`).
#' @param order_by Optional vector of response categories used to order the
#'   items, e.g. `c("Definitely Agree", "Rather Agree")` orders items by their
#'   combined share of those two categories.
#' @param item_label If `TRUE` (default), item labels (the `"label"` attribute)
#'   are used instead of variable names. Falls back to names, with a warning, if
#'   any item lacks a label.
#' @param title Optional plot title, usually the wording the items share. Shown
#'   when given; defaults to `NULL` (no title).
#' @param title_quote If `TRUE`, wraps the title in low/high double quotation
#'   marks (`„` and `“`). Defaults to `FALSE`.
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples \dontrun{
#' stem_battery(trust, items = c(police, eu, government, army))
#' stem_battery(trust,
#'              items = c(police, eu, government, army),
#'              weight = W,
#'              order_by = c("Definitely Agree", "Rather Agree"),
#'              title = "How much do you trust these institutions?")
#' }
stem_battery <- function(
  data,
  items,
  weight = NULL,
  order_by = NULL,
  item_label = TRUE,
  palette = "div1",
  direction = 1,
  labels = TRUE,
  label_accuracy = 1,
  label_suffix = "",
  label_hide = 0.05,
  label_color = "black",
  label_bicolor = TRUE,
  title = NULL,
  title_quote = FALSE,
  title_wrap = 73,
  dk_levels = NULL,
  dk_color = "grey70"
) {
  # Value labels become factor levels before the items are stacked.
  data <- label_values(data, {{ items }})
  item_data <- dplyr::select(data, {{ items }})
  item_names <- names(item_data)
  categories <- battery_categories(item_data)

  long <- data |>
    dplyr::select({{ items }}, {{ weight }}) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(item_names),
      names_to = ".battery_item",
      values_to = ".response"
    )

  # Keep the shared response categories in their original order.
  if (!is.null(categories)) {
    long$.response <- factor(long$.response, levels = categories)
  }

  # Label or name the items.
  long$.battery_item <- factor(
    long$.battery_item,
    levels = item_names,
    labels = item_display_names(item_data, item_label)
  )

  plot_data <- stem_plot_data(
    data = long,
    item = .response,
    group = .battery_item,
    weight = {{ weight }},
    label_accuracy = label_accuracy,
    label_suffix = label_suffix,
    label_hide = label_hide,
    dk_levels = dk_levels
  )

  # Optionally order items by their combined share of `order_by` categories,
  # scored from the plotted shares (so missing answers don't count).
  if (!is.null(order_by)) {
    order_levels <- plot_data |>
      dplyr::summarise(
        score = sum(freq[.response %in% order_by]),
        .by = .battery_item
      ) |>
      dplyr::arrange(dplyr::desc(score)) |>
      dplyr::pull(.battery_item)
    plot_data$.battery_item <- factor(
      plot_data$.battery_item,
      levels = as.character(order_levels)
    )
  }

  p <- stem_stack(
    plot_data = plot_data,
    fill_name = ".response",
    y_name = ".battery_item",
    palette = palette,
    direction = direction,
    labels = labels,
    label_color = label_color,
    label_bicolor = label_bicolor,
    dk_levels = dk_levels,
    dk_color = dk_color
  )

  if (!is.null(title)) {
    p <- p +
      ggplot2::labs(title = format_title(title, title_quote, title_wrap))
  }

  p
}

#' Share of respondents choosing each yes/no item
#'
#' Internal helper for the yes/no layout of [stem_multiselect()]. For each item,
#' computes with [stem_summarise_cat()] the (possibly weighted) share of valid
#' answers equal to `selected`, within `group` if given. Items or groups where
#' nobody chose `selected` get a share of 0.
#'
#' @param data Data frame holding the items.
#' @param item_names Names of the item columns.
#' @param selected The answer that marks an item as chosen.
#' @param group,weight Optional grouping variable and survey weights.
#'
#' @return A tibble with the group column (if any), `.response` (the item
#'   name), `freq`, `freq_low` and `freq_upp`.
#' @keywords internal
selected_shares <- function(data, item_names, selected, group = NULL, weight = NULL) {
  answers <- unique(unlist(lapply(data[item_names], function(x) as.character(x))))
  if (!selected %in% answers) {
    stop(
      "None of the items has the answer \"", selected, "\" (`selected`). ",
      "Answers found: ", paste0("\"", stats::na.omit(answers), "\"", collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  shares <- lapply(item_names, function(name) {
    x <- as.character(data[[name]])
    data$.answer <- factor(
      ifelse(x == selected, "selected", "other"),
      levels = c("selected", "other")
    )
    stem_summarise_cat(
      data,
      item = .answer,
      group = {{ group }},
      weight = {{ weight }},
      min_cell_n = NULL
    ) |>
      dplyr::group_by(dplyr::across(-c(.answer, freq, freq_low, freq_upp))) |>
      tidyr::complete(.answer, fill = list(freq = 0, freq_low = 0, freq_upp = 0)) |>
      dplyr::ungroup() |>
      dplyr::filter(.answer == "selected") |>
      dplyr::mutate(.response = name, .answer = NULL)
  })

  dplyr::bind_rows(shares)
}

#' Bar plot for a set of multiple-choice items
#'
#' Plots a set of "select all that apply" items, showing the share of
#' respondents that picked each option. It supports two layouts of the data:
#'
#' * **Mention columns** (default, `selected = NULL`), e.g. `biggest_concern1`,
#'   `biggest_concern2`, ... each holding one chosen option. The items are
#'   reshaped to long format and their (possibly weighted) frequencies are
#'   rescaled by the number of items, so that the bars express percentages of
#'   respondents rather than of responses. One bar is drawn per option.
#' * **Yes/no columns** (`selected` given), e.g. `q5_1` ... `q5_8`, each
#'   answered "Ano"/"Ne". One bar is drawn per item: the (possibly weighted)
#'   share of respondents with a valid answer on that item whose answer equals
#'   `selected`. Bars are named after the items' `"label"` attributes.
#'
#' @inheritParams stem_barplot
#' @param items Items to plot. Supports tidyselect helpers (e.g.
#'   `dplyr::starts_with()`).
#' @param infreq_order If `TRUE` (default), options are ordered by frequency.
#' @param background If `TRUE` (default, ungrouped plots only), draws a full-width
#'   background bar behind each option.
#' @param background_fill Fill colour of the background bars.
#' @param background_alpha Opacity of the background bars.
#' @param selected The answer that marks an item as chosen, e.g. `"Ano"`.
#'   Switches to the yes/no layout described above. Defaults to `NULL` (mention
#'   columns).
#' @param item_label Yes/no layout only. If `TRUE` (default), bars are named
#'   after the items' `"label"` attributes, falling back to variable names, with
#'   a warning, if any item lacks a label (as in [stem_battery()]).
#' @param keep_last Optional bars to keep at the end (the bottom) whatever the
#'   ordering, e.g. `c("Jiné", "Ostatní")`, in the order given. Refers to
#'   options, or in the yes/no layout to items (by label or variable name).
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples \dontrun{
#' stem_multiselect(trust, items = dplyr::starts_with("biggest_concern"))
#' stem_multiselect(trust, items = dplyr::starts_with("biggest_concern"), weight = W)
#'
#' # Yes/no columns, one bar per item.
#' stem_multiselect(survey, items = q5_1:q5_8, selected = "Ano", keep_last = "Jiné")
#' }
stem_multiselect <- function(
  data,
  items,
  group = NULL,
  weight = NULL,
  palette = "div1",
  direction = 1,
  labels = TRUE,
  label_accuracy = 1,
  label_suffix = "",
  label_hide = 0,
  infreq_order = TRUE,
  background = TRUE,
  background_fill = "grey85",
  background_alpha = 1,
  selected = NULL,
  item_label = TRUE,
  keep_last = NULL,
  bar_color = "#82143C"
) {
  group_quo <- rlang::enquo(group)
  has_group <- !rlang::quo_is_null(group_quo)

  data <- label_values(data, {{ items }})
  item_data <- dplyr::select(data, {{ items }})
  item_names <- names(item_data)
  n_items <- length(item_names)

  if (is.null(selected)) {
    long <- data |>
      dplyr::select({{ items }}, {{ weight }}, {{ group }}) |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(item_names),
        names_to = ".item",
        values_to = ".response"
      )

    plot_data <- stem_summarise_cat(
      data = long,
      item = .response,
      group = {{ group }},
      weight = {{ weight }},
      min_cell_n = NULL
    )

    # Frequencies are shares of responses; rescale to shares of respondents.
    plot_data$freq <- plot_data$freq * n_items
    plot_data$freq_low <- plot_data$freq_low * n_items
    plot_data$freq_upp <- plot_data$freq_upp * n_items
  } else {
    plot_data <- selected_shares(
      data, item_names, selected,
      group = {{ group }}, weight = {{ weight }}
    )
    display_names <- item_display_names(item_data, item_label)
    plot_data$.response <- factor(
      plot_data$.response,
      levels = item_names,
      labels = display_names
    )
    # keep_last may name items by variable name as well as by label.
    keep_last <- ifelse(
      keep_last %in% item_names,
      display_names[match(keep_last, item_names)],
      keep_last
    )
  }

  plot_data$stem_label <- format_pct(
    plot_data$freq,
    accuracy = label_accuracy,
    suffix = label_suffix,
    hide = label_hide
  )

  if (infreq_order) {
    plot_data <- dplyr::mutate(
      plot_data,
      .response = forcats::fct_reorder(.response, freq, .fun = sum)
    )
  }

  # The first level is drawn at the bottom, so bars kept last go first.
  if (length(keep_last)) {
    plot_data$.response <- as.factor(plot_data$.response)
    keep_last <- intersect(keep_last, levels(plot_data$.response))
    plot_data$.response <- forcats::fct_relevel(
      plot_data$.response,
      rev(keep_last),
      after = 0
    )
  }

  if (has_group) {
    group_name <- rlang::as_name(group_quo)
    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = freq,
        y = .response,
        fill = .data[[group_name]],
        label = stem_label
      )
    ) +
      ggplot2::geom_col(
        color = "white",
        position = ggplot2::position_dodge2(reverse = TRUE)
      ) +
      scale_fill_stem(palette = palette, direction = direction)
  } else {
    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = freq, y = .response, label = stem_label)
    )

    if (background) {
      background_data <- data.frame(
        .response = factor(
          levels(plot_data$.response),
          levels = levels(plot_data$.response)
        )
      )
      p <- p +
        ggplot2::geom_col(
          data = background_data,
          mapping = ggplot2::aes(x = 1, y = .response),
          inherit.aes = FALSE,
          fill = background_fill,
          alpha = background_alpha
        )
    }

    p <- p + ggplot2::geom_col(color = "white", fill = bar_color)
  }

  p <- p +
    ggplot2::scale_x_continuous(
      labels = scales::label_percent(suffix = " %"),
      limits = c(0, NA),
      expand = ggplot2::expansion(mult = c(0, 0.1))
    )

  if (labels) {
    p <- p +
      ggplot2::geom_text(
        hjust = -0.2,
        position = if (has_group) {
          ggplot2::position_dodge2(width = 0.9, reverse = TRUE)
        } else {
          ggplot2::position_identity()
        }
      )
  }

  p
}

# Numeric variables -------------------------------------------------------

#' Horizontal box plot of a numeric variable
#'
#' Plots the (possibly weighted) distribution of a numeric variable as a
#' horizontal box plot in the Stem look, one box per `group` category. The box
#' spans the weighted first and third quartiles with a line at the weighted
#' median; whiskers reach the most extreme values within 1.5 × IQR of the box,
#' and values beyond them are drawn as outliers. The weighted mean is marked
#' with a white diamond, which stays visible when the mean lies outside the box
#' or the box has zero width.
#'
#' Weighted quartiles come from the weighted inverse empirical CDF: the first
#' value whose cumulative weight share reaches the probability. With equal
#' weights they agree with `stats::quantile(type = 1)`.
#'
#' Office charts have no box plot, so these plots cannot be converted with
#' [stem_as_mschart()]; save them as images (e.g. with [ggplot2::ggsave()]).
#'
#' @inheritParams stem_barplot
#' @param item Numeric variable to plot. A labelled numeric variable is plotted
#'   on its codes.
#' @param group Optional grouping variable; one box is drawn per category.
#' @param palette Name of a Stem palette; the boxes use its first colour.
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples \dontrun{
#' stem_boxplot(trust, age)
#' stem_boxplot(trust, age, group = eu_index, weight = W, title_show = TRUE)
#' }
stem_boxplot <- function(
  data,
  item,
  group = NULL,
  weight = NULL,
  collapse_group = NULL,
  palette = "div1",
  title_show = FALSE,
  title_quote = FALSE,
  title_wrap = 73
) {
  item_name <- rlang::as_name(rlang::enquo(item))
  group_quo <- rlang::enquo(group)
  has_group <- !rlang::quo_is_null(group_quo)

  box <- stem_box_stats(
    data,
    item = {{ item }},
    group = {{ group }},
    weight = {{ weight }},
    collapse_group = collapse_group
  )

  y <- if (has_group) {
    ggplot2::aes(y = .data[[rlang::as_name(group_quo)]])
  } else {
    ggplot2::aes(y = "")
  }

  p <- ggplot2::ggplot(
    box$stats,
    ggplot2::aes(
      xmin = whisker_low,
      xlower = q1,
      xmiddle = median,
      xupper = q3,
      xmax = whisker_upp,
      !!!y
    )
  ) +
    ggplot2::geom_boxplot(
      stat = "identity",
      orientation = "y",
      fill = stem_palette(palette)[1],
      colour = "grey25",
      width = 0.6
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = mean),
      shape = 23,
      size = 3,
      fill = "white",
      colour = "grey10",
      stroke = 0.8
    ) +
    ggplot2::scale_y_discrete(limits = rev) +
    ggplot2::labs(x = NULL, y = NULL)

  if (nrow(box$outliers)) {
    outlier_y <- if (has_group) {
      ggplot2::aes(x = value, y = .data[[rlang::as_name(group_quo)]])
    } else {
      ggplot2::aes(x = value, y = "")
    }
    p <- p +
      ggplot2::geom_point(
        data = box$outliers,
        mapping = outlier_y,
        inherit.aes = FALSE,
        shape = 1,
        colour = "grey25"
      )
  }

  if (title_show) {
    p <- p +
      ggplot2::labs(
        title = stem_plot_title(data, item_name, title_quote, title_wrap)
      )
  }

  p
}
