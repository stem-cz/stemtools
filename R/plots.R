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
  label_hide = 0
) {
  plot_data <- stem_summarise_cat(
    data = data,
    item = {{ item }},
    group = {{ group }},
    weight = {{ weight }},
    collapse_item = collapse_item,
    collapse_group = collapse_group
  )

  plot_data$stem_label <- format_pct(
    plot_data$freq,
    accuracy = label_accuracy,
    suffix = label_suffix,
    hide = label_hide
  )

  plot_data
}

#' Build a stacked horizontal bar plot
#'
#' Internal renderer shared by [stem_barstack()] and [stem_inline()]. Draws a
#' single stacked horizontal bar per `y_name` category (or one bar in total when
#' `y_name` is `NULL`).
#'
#' @param plot_data Data produced by [stem_plot_data()].
#' @param fill_name Name of the item column mapped to `fill`.
#' @param y_name Name of the group column mapped to the y axis, or `NULL` for a
#'   single (inline) bar.
#' @inheritParams stem_barstack
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
  label_color = "black"
) {
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
    ggplot2::geom_col(position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::scale_x_continuous(
      labels = scales::label_percent(suffix = " %"),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::scale_y_discrete(limits = rev) +
    scale_fill_stem(palette = palette, direction = direction) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))

  if (labels) {
    p <- p +
      ggplot2::geom_text(
        position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE),
        colour = label_color
      )
  }

  p
}

# Plotting functions ------------------------------------------------------

#' Simple horizontal bar plot
#'
#' Plots the (possibly weighted) distribution of a single categorical variable
#' as horizontal bars, one bar per category. Supplying a `group` variable draws
#' dodged bars so categories can be compared across groups.
#'
#' @param data Data frame holding the item (and group) variables.
#' @param item Categorical variable to plot.
#' @param group Optional grouping variable. When supplied, bars are dodged and
#'   coloured by group and proportions are computed within each group.
#' @param weight Optional survey weights.
#' @param collapse_item Optional named list passed to [stem_summarise_cat()] to
#'   collapse (or rename) item categories.
#' @param collapse_group Optional named list to collapse (or rename) group
#'   categories.
#' @param palette Name of a Stem palette. See [stem_palette()].
#' @param direction Palette direction. Use `-1` to reverse the colours.
#' @param labels If `TRUE`, prints a percentage label at the end of each bar.
#' @param label_accuracy Rounding accuracy of labels. `1` gives whole numbers,
#'   `0.1` one decimal place.
#' @param label_suffix Suffix appended to labels. Defaults to `""`.
#' @param label_hide Proportions below this threshold are left unlabelled.
#' @param errorbar If `TRUE`, adds 95% confidence interval error bars.
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples \dontrun{
#' stem_barplot(trust, government)
#' stem_barplot(trust, police, group = eu_index, weight = W)
#' }
stem_barplot <- function(
  data,
  item,
  group = NULL,
  weight = NULL,
  collapse_item = NULL,
  collapse_group = NULL,
  palette = "modern",
  direction = 1,
  labels = TRUE,
  label_accuracy = 1,
  label_suffix = "",
  label_hide = 0,
  errorbar = FALSE
) {
  item_name <- rlang::as_name(rlang::enquo(item))
  group_quo <- rlang::enquo(group)
  has_group <- !rlang::quo_is_null(group_quo)

  plot_data <- stem_plot_data(
    data = data,
    item = {{ item }},
    group = {{ group }},
    weight = {{ weight }},
    collapse_item = collapse_item,
    collapse_group = collapse_group,
    label_accuracy = label_accuracy,
    label_suffix = label_suffix,
    label_hide = label_hide
  )

  if (has_group) {
    group_name <- rlang::as_name(group_quo)
    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = freq,
        y = .data[[item_name]],
        fill = .data[[group_name]],
        label = stem_label
      )
    ) +
      ggplot2::geom_col(position = ggplot2::position_dodge2(reverse = TRUE)) +
      scale_fill_stem(palette = palette, direction = direction)
  } else {
    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = freq, y = .data[[item_name]], label = stem_label)
    ) +
      ggplot2::geom_col(fill = stem_palette(palette)[1])
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
        position = if (has_group) {
          ggplot2::position_dodge2(padding = 0.5)
        } else {
          "identity"
        }
      )
  }

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

#' Stacked horizontal bar plot
#'
#' Plots the (possibly weighted) distribution of a categorical `item` as a
#' stacked horizontal bar for each category of a `group` variable. Proportions
#' are computed within each group, so every bar sums to 100%.
#'
#' @inheritParams stem_barplot
#' @param group Grouping variable. One stacked bar is drawn per group category.
#' @param label_color Colour of the segment labels. Defaults to `"black"`.
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples \dontrun{
#' stem_barstack(trust, police, group = eu_index)
#' stem_barstack(trust, police, group = eu_index, weight = W)
#' }
stem_barstack <- function(
  data,
  item,
  group,
  weight = NULL,
  collapse_item = NULL,
  collapse_group = NULL,
  palette = "div1",
  direction = 1,
  labels = TRUE,
  label_accuracy = 1,
  label_suffix = "",
  label_hide = 0.05,
  label_color = "black"
) {
  item_name <- rlang::as_name(rlang::enquo(item))
  group_name <- rlang::as_name(rlang::enquo(group))

  plot_data <- stem_plot_data(
    data = data,
    item = {{ item }},
    group = {{ group }},
    weight = {{ weight }},
    collapse_item = collapse_item,
    collapse_group = collapse_group,
    label_accuracy = label_accuracy,
    label_suffix = label_suffix,
    label_hide = label_hide
  )

  stem_stack(
    plot_data = plot_data,
    fill_name = item_name,
    y_name = group_name,
    palette = palette,
    direction = direction,
    labels = labels,
    label_color = label_color
  )
}

#' Inline stacked bar plot
#'
#' Plots the (possibly weighted) distribution of a single categorical variable
#' as one stacked horizontal bar. Useful as a compact, inline summary of a
#' single item.
#'
#' @inheritParams stem_barstack
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
  label_color = "black"
) {
  item_name <- rlang::as_name(rlang::enquo(item))

  plot_data <- stem_plot_data(
    data = data,
    item = {{ item }},
    weight = {{ weight }},
    collapse_item = collapse_item,
    label_accuracy = label_accuracy,
    label_suffix = label_suffix,
    label_hide = label_hide
  )

  stem_stack(
    plot_data = plot_data,
    fill_name = item_name,
    y_name = NULL,
    palette = palette,
    direction = direction,
    labels = labels,
    label_color = label_color
  )
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

#' Stacked bar plot for a battery of like items
#'
#' Plots a set of categorical variables that share the same response categories
#' (e.g. a battery of Likert items) as a single chart, drawing one stacked
#' horizontal bar per item. Internally the items are reshaped to long format and
#' handed to the same machinery as [stem_barstack()], with the item taking the
#' role of the group.
#'
#' @inheritParams stem_barstack
#' @param items Items to plot. Supports tidyselect helpers (e.g.
#'   `dplyr::starts_with()`).
#' @param order_by Optional vector of response categories used to order the
#'   items, e.g. `c("Definitely Agree", "Rather Agree")` orders items by their
#'   combined share of those two categories.
#' @param item_label If `TRUE` (default), item labels (the `"label"` attribute)
#'   are used instead of variable names. Falls back to names, with a warning, if
#'   any item lacks a label.
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples \dontrun{
#' stem_battery(trust, items = c(police, eu, government, army))
#' stem_battery(trust,
#'              items = c(police, eu, government, army),
#'              weight = W,
#'              order_by = c("Definitely Agree", "Rather Agree"))
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
  label_color = "black"
) {
  item_data <- dplyr::select(data, {{ items }})
  item_names <- names(item_data)

  long <- data |>
    dplyr::select({{ items }}, {{ weight }}) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(item_names),
      names_to = ".battery_item",
      values_to = ".response"
    )

  # Keep the shared response categories in their original order.
  if (is.factor(item_data[[1]])) {
    long$.response <- factor(long$.response, levels = levels(item_data[[1]]))
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

  # Optionally order items by their combined share of `order_by` categories.
  if (!is.null(order_by)) {
    order_levels <- long |>
      dplyr::count(.battery_item, .response, wt = {{ weight }}) |>
      dplyr::mutate(freq = n / sum(n), .by = .battery_item) |>
      dplyr::summarise(
        score = sum(freq[.response %in% order_by]),
        .by = .battery_item
      ) |>
      dplyr::arrange(dplyr::desc(score)) |>
      dplyr::pull(.battery_item)
    long$.battery_item <- factor(
      long$.battery_item,
      levels = as.character(order_levels)
    )
  }

  plot_data <- stem_plot_data(
    data = long,
    item = .response,
    group = .battery_item,
    weight = {{ weight }},
    label_accuracy = label_accuracy,
    label_suffix = label_suffix,
    label_hide = label_hide
  )

  stem_stack(
    plot_data = plot_data,
    fill_name = ".response",
    y_name = ".battery_item",
    palette = palette,
    direction = direction,
    labels = labels,
    label_color = label_color
  )
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
  palette = "modern",
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
    weight = {{ weight }}
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
      ggplot2::geom_col(position = ggplot2::position_dodge2(reverse = TRUE)) +
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

    p <- p + ggplot2::geom_col(fill = stem_palette(palette)[1])
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
