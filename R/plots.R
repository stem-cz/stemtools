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
stem_plot_title <- function(data, item_name, quote = FALSE, wrap = 80) {
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
format_title <- function(title, quote = FALSE, wrap = 80) {
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
  # Only the levels present are coloured, as a discrete scale would.
  fill_levels <- levels(droplevels(as.factor(plot_data[[fill_name]])))
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
#'   or `Inf` to disable wrapping. Defaults to `80`.
#' @param dk_levels Optional character vector of "don't know" categories of the
#'   item (e.g. `"Nevím"`). They are moved to the end (the right end of stacked
#'   bars and of the legend, the bottom of single-colour bar charts) and drawn
#'   grey, and they do not count towards the palette size. Levels not present
#'   in the data are ignored. Defaults to `NULL`.
#' @param dk_color Fill colour of the "don't know" categories. Defaults to
#'   `"grey70"` (`#B3B3B3`). Give several colours for several "don't know"
#'   levels; otherwise distinguishable darker greys are derived from the first.
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
  title_wrap = 80,
  dk_levels = NULL,
  dk_color = "grey70"
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

  # Ungrouped: one bar per item category.
  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = freq, y = .data[[item_name]], label = stem_label)
  ) +
    ggplot2::geom_col(color = "white", fill = stem_palette(palette)[1]) +
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
  title_wrap = 80,
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
  title_wrap = 80,
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
  item_levels <- item_names
  item_labels <- item_names
  if (item_label) {
    labels_lookup <- column_labels(item_data)
    if (anyNA(labels_lookup)) {
      warning(
        "At least one item has no `label` attribute; using variable names instead."
      )
    } else {
      item_labels <- unname(labels_lookup[item_names])
    }
  }
  long$.battery_item <- factor(
    long$.battery_item,
    levels = item_levels,
    labels = item_labels
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

#' Bar plot for a set of multiple-choice items
#'
#' Plots a set of "select all that apply" items, showing the share of
#' respondents that picked each option. The items are reshaped to long format
#' and their (possibly weighted) frequencies are rescaled by the number of items
#' so that the bars express percentages of respondents rather than of responses.
#'
#' @inheritParams stem_barplot
#' @param items Items to plot. Supports tidyselect helpers (e.g.
#'   `dplyr::starts_with()`).
#' @param infreq_order If `TRUE` (default), options are ordered by frequency.
#' @param background If `TRUE` (default, ungrouped plots only), draws a full-width
#'   background bar behind each option.
#' @param background_fill Fill colour of the background bars.
#' @param background_alpha Opacity of the background bars.
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples \dontrun{
#' stem_multiselect(trust, items = dplyr::starts_with("biggest_concern"))
#' stem_multiselect(trust, items = dplyr::starts_with("biggest_concern"), weight = W)
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
  background_alpha = 1
) {
  group_quo <- rlang::enquo(group)
  has_group <- !rlang::quo_is_null(group_quo)

  data <- label_values(data, {{ items }})
  item_names <- names(dplyr::select(data, {{ items }}))
  n_items <- length(item_names)

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

    p <- p + ggplot2::geom_col(color = "white", fill = stem_palette(palette)[1])
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
