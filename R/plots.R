#' General plotting function
#'
#'
#' @param data Dataframe including item (and group) variables
#' @param item Plotted item
#' @param group Optional; plotted grouping variable
#' @param weight Optional; survey weights
#' @param collapse_item Optional, a named list to collapse item categories
#' @param collapse_group Optional, a named list to collapse group categories
#' @param geom Geom that will represent data
#' @param geom_args Arguments for the geom function like size, color, etc.
#' @param title Should the name of item be used as plot title? FALSE by default
#' @param title_label Should title use item label instead of name? Only if item has attribute "label".
#' @param title_wrap Length of title in characters before it gets wrapped. Defaults to 50.
#' @param format_axis If `TRUE`, adds title for x axis and removes titles for other scales.
#' @param caption If `TRUE`, adds a caption with sample size.
#' @param label Should geom labels be printed? Yes by default
#' @param label_args Arguments for the labels function like vjust, postion, etc.
#' @param label_hide Hides geom labels with values smaller than this threshold. Defaults to 0.05.
#' @param label_accuracy Accuracy geom labels. `1` means no decimal places, `0.1` one decimal place.
#' @param label_scale Multiplicative scale of geom labels. `100` multiple the values by 100
#' @param label_prefix Prefix for geom labels.
#' @param label_suffix Suffix for geom labels
#' @param label_big Character to split big numbers. Whitespace by default (e.g. `1000` will be printed as "1 000").
#' @param label_decimal Character to separate decimal digits. Comma by default (e.g. `1.2` will be printed as "1,2")
#' @param group_size Should the group size be included in the group legend? Either `n` (absolute frequency), `prop` (proportions) or `none` (nothing).
#' @param group_wrap How many characters in group categories before wrapping to the next line. Defaults to `NULL` - no wrapping.
#' @param item_size Should the group size be included in the item legend? Either `n` (absolute frequency), `prop` (proportions) or `none` (nothing).
#' @param item_wrap How many characters in item categories before wrapping to the next line. Defaults to `NULL` - no wrapping.
#' @param scale_x Pass [ggplot2::scale_x_discrete()] to the plot if needed.
#' @param scale_y Pass [ggplot2::scale_y_continuous()] to the plot if needed.
#' @param scale_fill Pass `scale_fill_*` to the plot if needed.
#' @param scale_color Pass `scale_color_*` to the plot if needed.
#' @param facet Should groups be splitted into facets? No by default.
#' @param guides Pass [ggplot2::guides()] function to the plot if needed.
#' @param coord_flip If `TRUE`, switches x and y axes.
#' @param item_reverse Reverse order of item categories
#' @param group_reverse Reverse order of group categories
#'
#' @return a ggplot2 graph with custom attribute "stem_plot"
#' @export
#'
#' @examples \dontrun{
#' stem_plot(data = trust,
#' item = government,
#' label = FALSE)
#'
#' stem_plot(data = trust,
#'           item = police,
#'           group = eu_index,
#'           weight = W,
#'           geom_args = list(position = "stack"),
#'           label_args = list(position = ggplot2::position_stack(vjust = 0.5), color = "white"),
#'           label_scale = 100,
#'           label_suffix = "",
#'           item_size = "prop",
#'           group_size = "n",
#'           scale_y = ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1,
#'                                                                                 suffix = " %"))) +
#' theme_stem(legend.position = "bottom")
#' }
stem_plot <- function(data,
                      item,
                      group  = NULL,
                      weight = NULL,
                      collapse_item = NULL,
                      collapse_group = NULL,
                      geom = ggplot2::geom_col,
                      geom_args = list(position = ggplot2::position_dodge(width = 0.95)),
                      title = FALSE,
                      title_label = TRUE,
                      title_wrap = 50,
                      format_axis = TRUE,
                      caption = FALSE,
                      label = TRUE,
                      label_args = list(position = ggplot2::position_dodge(width = 0.95),
                                        color = "black",
                                         size = ggplot2::rel(5),
                                        vjust = -1),
                      label_hide = 0.05,
                      label_accuracy = 1,
                      label_scale = 1,
                      label_prefix = "",
                      label_suffix = "",
                      label_big = " ",
                      label_decimal = ",",
                      group_size = "none",
                      group_wrap = NULL,
                      item_size = "none",
                      item_wrap = NULL,
                      scale_x = ggplot2::scale_x_discrete(),
                      scale_y = ggplot2::scale_y_continuous(limits = c(0, NA)),
                      scale_fill = scale_fill_stem(palette = "div1"),
                      scale_color = scale_color_stem(palette = "div1"),
                      facet = FALSE,
                      guides = ggplot2::guides(),
                      coord_flip = FALSE,
                      item_reverse = FALSE,
                      group_reverse = FALSE) {

  if(rlang::quo_is_null(rlang::enquo(weight))){message("No weights used.")}
  summ <- stem_summarise(data, item = {{ item }}, group = {{ group }}, weight = {{ weight }},
                         return_n = TRUE, collapse_item = collapse_item, collapse_group = collapse_group)
  summ <- dplyr::rename_with(summ, ~gsub(x = ., pattern = "freq|mean", replacement = "estimate"))


  group_check <- rlang::enquo(group)

  # Extracting names weird way that also works in loops
  item_name <- data |> dplyr::select({{ item }}) |> names()
  item_label <- data |> dplyr::pull({{ item }}) |> attr("label")

  if(!rlang::quo_is_null(group_check)) {
    group_name <- data |> dplyr::select({{ group }}) |> names()
    group_label <- data |> dplyr::pull({{ group }}) |> attr("label")
  }

  # stem_summarise_num doesn't return item column by default. we added so that something can be plotted
  if(!item_name %in% names(summ)) {summ[[item_name]] <- item_name}

  # Optionally wrap item categories
  if(!is.null(item_wrap)) {
    levels(summ[[item_name]]) <- stringr::str_wrap(levels(summ[[item_name]]), item_wrap)
  }

  # Optionally wrap group categories
  if(!is.null(group_wrap)) {
    levels(summ[[group_name]]) <- stringr::str_wrap(levels(summ[[group_name]]), group_wrap)
  }

  # Add group size to break labels
  if(!rlang::quo_is_null(group_check)) {
    summ <- summ |>
      dplyr::mutate(group_prop = round(group_n / sum(n) * 100),
                    item_prop = round(item_n / sum(n) * 100))

    item_sizes <- dplyr::distinct(summ, {{ item }}, item_prop, item_n) #|> dplyr::arrange({{ group }})
    group_sizes <- dplyr::distinct(summ, {{ group }}, group_prop, group_n) #|> dplyr::arrange({{ item }})

    switch (group_size,
            "n" = levels(summ[[group_name]])    <- paste0(group_sizes[[group_name]], "\n(n = ",group_sizes$group_n, ")"),
            "prop" = levels(summ[[group_name]]) <- paste0(group_sizes[[group_name]], "\n(",group_sizes$group_prop, "%)"))

    switch (item_size,
            "n" = levels(summ[[item_name]])    <- paste0(item_sizes[[item_name]], "\n(n = ", item_sizes$item_n, ")"),
            "prop" = levels(summ[[item_name]]) <- paste0(item_sizes[[item_name]], "\n(",  item_sizes$item_prop, "%)"))
  }

  # Optionally reverse order of item and group categories
  if(item_reverse) {summ <- summ |> dplyr::mutate({{ item }} := forcats::fct_rev({{ item }}))}
  if(group_reverse & !rlang::quo_is_null(group_check)) {summ <- summ |> dplyr::mutate({{ group }} := forcats::fct_rev({{ group }}))}

  # Formatting labels
  summ <- summ |>
    dplyr::mutate(geom_label =  scales::number(estimate,
                                               accuracy = label_accuracy,
                                               scale = label_scale,
                                               prefix = label_prefix,
                                               suffix = label_suffix,
                                               big.mark = label_big,
                                               decimal.mark = label_decimal),
                  geom_label = dplyr::if_else(estimate < label_hide,
                                              true = "",
                                              false = geom_label))
  # Basic mapping
  if(rlang::quo_is_null(group_check)) {
    p <- ggplot2::ggplot(data = summ,
                         mapping = ggplot2::aes(x = {{ item }},
                                                y = estimate,
                                                ymin  = estimate_low,
                                                ymax  = estimate_upp,
                                                label = geom_label))
  } else {
    p <- ggplot2::ggplot(data = summ,
                         mapping = ggplot2::aes(x = {{group}},
                                                y = estimate,
                                                ymin  = estimate_low,
                                                ymax  = estimate_upp,
                                                fill = {{ item }},
                                                color = {{ item }},
                                                label = geom_label))
  }

  # Geom and scales
  p <- p + do.call(geom, geom_args) + scale_x + scale_y + scale_fill + scale_color + guides

  # Optionally format Axis labels
  if(format_axis) {
    p <- p + ggplot2::labs(y = ggplot2::element_blank(),
                           fill = ggplot2::element_blank(),
                           color = ggplot2::element_blank())

    if(rlang::quo_is_null(group_check)) {
      p <- p + ggplot2::labs(x = ggplot2::element_blank(),
                             y = ggplot2::element_blank())
    } else {
      if(is.null(group_label)) {
        p <- p + ggplot2::labs(x = group_name,
                               y = ggplot2::element_blank())
      } else {
        p <- p + ggplot2::labs(x = group_label,
                               y = ggplot2::element_blank())
      }
    }
  }

  # Optionally freq labels
  if(label) {p <- p + do.call(ggplot2::geom_text, label_args)}

  # Optionally title. If title_label = TRUE and the variable has attribute "label", it will be used instead of
  # the variable name.
  if(title) {
    if(title_label & !is.null(item_label)) {
      p <- p + ggplot2::ggtitle(label = item_label)
    } else {p <- p + ggplot2::ggtitle(label = item_name)}
  }

  # Optionally caption with sample size used
  if(caption) {
    total_n <- scales::number(sum(summ$n), big.mark = " ")
    p <- p + ggplot2::labs(caption = paste0("Velikost vzorku:", total_n, " respondentů."))
  }

  #facets (only if group is present)
  if(!rlang::quo_is_null(group_check) & facet == TRUE) {p <- p + ggplot2::facet_wrap(ggplot2::vars({{ group }}), scales = "free_x")}

  #Optionally switch x and y axis
  if(coord_flip) {p <- p + ggplot2::coord_flip()}

  attr(p, which = "stem_plot") <- "stem_plot"

  return(p)

}


#' Plot a simple barchart
#'
#' Wrapper around [stemtools::stem_plot()] to make creating barplots easier.
#'
#' @param data Dataframe including item (and group) variables
#' @param item Plotted item
#' @param group Optional; plotted grouping variable
#' @param weight Optional; survey weights
#' @param scale_y Format y axis using [ggplot2::scale_y_continuous()]
#' @param label Should geom labels be printed? Yes by default
#' @param label_scale Multiplicative scale of geom labels. `100` multiple the values by 100
#' @param label_hide Hides geom labels with values smaller than this threshold. Defaults to 0.
#' @param ... Other arguments passed to [stemtools::stem_plot()]
#'
#' @return A ggplot2 object with custom attribute "stem_plot"
#' @export
#'
#' @examples \dontrun{
#' stem_plot_bar(trust, government)
#' }
stem_plot_bar <- function(data,
                          item,
                          group = NULL,
                          weight = NULL,
                          scale_y = ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1, suffix = " %"),
                                                                limits = c(0, NA)),
                          label = TRUE,
                          label_scale = 100,
                          label_hide = 0,
                          ...) {
  p <- stem_plot(data = data,
                 item = {{ item }},
                 group = {{ group }},
                 weight = {{weight}},
                 scale_y = scale_y,
                 label = label,
                 label_scale = label_scale,
                 label_hide = label_hide,
                 ...)

  attr(p, which = "stem_plot") <- "stem_plot_bar"

  return(p)
}


#' Plot a stacked barchart
#'
#' Wrapper around [stemtools::stem_plot()] to make creating stacked barplots easier.
#'
#' @param data Dataframe including item (and group) variables
#' @param item Plotted item
#' @param group Optional; plotted grouping variable
#' @param weight Optional; survey weights
#' @param geom geom that will represent data
#' @param geom_args Optional, a named list to collapse group categories
#' @param label_text_color Color of text labels. Default is `black`.
#' @param label_args Arguments for the labels function like vjust, postion, etc.
#' @param scale_y Format y axis using [ggplot2::scale_y_continuous()]
#' @param label Should geom labels be printed? Yes by default
#' @param label_scale Multiplicative scale of geom labels. `100` multiple the values by 100.
#' @param label_hide Hides geom labels with values smaller than this threshold. Defaults to 0.05.
#' @param legend_rows Number of rows in legend
#' @param legend_byrow Shoul legend be filled by rows? (FALSE to fill by columns)
#' @param palette Stem palette to be used in the plot. See [stemtools::scale_color_stem()] and [stemtools::scale_fill_stem()] for details.
#' @param coorf_flip Should the x and y axis be flipped? Defaults to `TRUE`
#' @param item_reverse Should the order of item categories be reversed? Defaults to `TRUE`
#' @param ... Other arguments passed to [stemtools::stem_plot()]
#'
#' @return A ggplot2 object with custom attribute "stem_plot"
#' @export
#'
#' @examples \dontrun{
#' stem_plot_barstack(trust, police, eu_index)
#' stem_plot_barstack(trust, police)
#' }
stem_plot_barstack <- function(data,
                               item,
                               group = NULL,
                               weight = NULL,
                               geom = ggplot2::geom_col,
                               geom_args = list(position = "stack"),
                               label_text_color = "black",
                               label_args = list(position = ggplot2::position_stack(vjust = 0.5), color = label_text_color, size = ggplot2::rel(5)),
                               scale_y = ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1, suffix = " %"),
                                                                     limits = c(0, NA)),
                               label = TRUE,
                               label_scale = 100,
                               label_hide = 0.05,
                               coorf_flip = TRUE,
                               item_reverse = TRUE,
                               legend_rows = 1,
                               legend_byrow = TRUE,
                               palette = "div1",
                               ...) {

  group_check <- rlang::enquo(group)

  p <- stem_plot(data = data,
                 item = {{ item }},
                 group = {{ group }},
                 weight = {{weight}},
                 scale_y = scale_y,
                 geom_args = geom_args,
                 label_args = label_args,
                 label = label,
                 label_scale = label_scale,
                 label_hide = label_hide,
                 scale_color = ggplot2::scale_color_manual(values = "white"),
                 scale_fill = scale_fill_stem(direction = -1, palette = palette),
                 coord_flip = coorf_flip,
                 item_reverse = item_reverse,
                 ...)

  # If only item is present, x axis will have dummy value
  if(rlang::quo_is_null(group_check)) {
    p <- p + ggplot2::aes(x = '', fill = {{ item }})
  }

  p <- p + ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE, nrow = legend_rows, byrow = legend_byrow),
                    color = ggplot2::guide_legend(reverse = TRUE, nrow = legend_rows, byrow = legend_byrow))

  attr(p, which = "stem_plot") <- "stem_plot_barstack"

  suppressWarnings(print(p))
}


#' Plot battery of like items
#'
#' Plots a set of categorical variables with same response categories as a single graph.
#'
#' @param data Dataframe including item (and group) variables
#' @param items Plotted items. Can be selected using Tidyselect's selection helpers.
#' @param weight Optional; survey weights
#' @param order_by Vector of response categories to order items by, e.g. `c("Definitely Agree", "Rather Agree")`.
#' @param geom_args Arguments for the geom function like size, color, etc.
#' @param label_args Arguments for the labels function like vjust, postion, etc.
#' @param label_scale Multiplicative scale of geom labels. `100` multiple the values by 100.
#' @param label_text_color Color of text labels. Default is `black`.
#' @param label_hide Hides geom labels with values smaller than this threshold. Defaults to 0.05.
#' @param item_label If `TRUE`, item labels are used instead of names (all items must have a label).
#' @param group_size Should the group size be included in the group legend? Either `n` (absolute frequency), `prop` (proportions) or `none` (nothing).
#' @param scale_y Format y axis using [ggplot2::scale_y_continuous()]
#' @param coord_flip If `TRUE`, switches x and y axes.
#' @param legend_rows Number of rows in legend
#' @param legend_byrow Shoul legend be filled by rows? (FALSE to fill by columns)
#' @param palette Stem palette to be used in the plot. See [stemtools::scale_color_stem()] and [stemtools::scale_fill_stem()] for details.
#' @param item_reverse Should the order of item categories be reversed? Defaults to `TRUE`
#' @param group_reverse Should the order of group categories be reversed? Defaults to `TRUE`
#' @param ... Other arguments passed to [stemtools::stem_plot()]
#'
#' @return A ggplot2 object with custom attribute "stem_plot"
#' @export
#'
#' @examples \dontrun{
#' stem_plot_battery(trust,
#'                   items = c(police, eu, government, army),
#'                   weight = W,
#'                   order_by = c("Definitely Agree", "Rather Agree"))
#'}
stem_plot_battery <- function(data,
                              items,
                              weight = NULL,
                              order_by = NULL,
                              geom_args = list(position = "stack"),
                              label_args = list(position = ggplot2::position_stack(vjust = 0.5), color = label_text_color),
                              label_scale = 100,
                              label_text_color = "black",
                              label_hide = 0.05,
                              item_label = TRUE,
                              group_size = "none",
                              scale_y = ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1, suffix = " %")),
                              coord_flip = TRUE,
                              palette = "div1",
                              legend_rows = 1,
                              legend_byrow = TRUE,
                              item_reverse = TRUE,
                              group_reverse = TRUE,
                              ...) {

  selected <- dplyr::select(.data = data, {{ items }}, {{ weight }})

  selected_long <- tidyr::pivot_longer(selected, cols = -{{ weight }}) |>
    dplyr::mutate(name = as.factor(name))

  # Check if all items have label
  if(item_label) {
    label_check <-  unlist(lapply(data, function(x) attr(x, "label")))
    label_check <- any(is.null(label_check))
  }

  if(label_check) {warning('At least one item does not have attribute "label"')}

  if(item_label & !label_check) {
    selected_name <- names(selected)
    selected_labs <- unlist(lapply(selected, function(x) attr(x, which = "label")))
    selected_cats <- as.list(selected_name)
    names(selected_cats) <- selected_labs

    selected_long$name <- do.call(forcats::fct_recode, c(list(selected_long$name), selected_cats))
  }

  if(!is.null(order_by)) {
    ordered_levels <- selected_long |>
      dplyr::count(name, value, wt = {{ weight }}) |>
      dplyr::mutate(freq = n / sum(n),
                    ordering = sum(freq[value %in% order_by]),
                    .by = name) |>
      dplyr::mutate(name = forcats::fct_reorder(name, ordering))

    ordered_levels <- rev(levels(ordered_levels$name))

    selected_long$name <- factor(selected_long$name, levels = ordered_levels)
  }

  p <- stem_plot(data = selected_long,
                 item = value,
                 group = name,
                 weight = {{ weight }},
                 geom_args = geom_args,
                 label_args = label_args,
                 label_scale = label_scale,
                 label_hide = label_hide,
                 group_size = group_size,
                 scale_y = scale_y,
                 caption = FALSE,
                 title = FALSE,
                 coord_flip = coord_flip,
                 scale_color = scale_color_stem(direction = -1, palette = palette),
                 scale_fill = scale_fill_stem(direction = -1, palette = palette),
                 item_reverse = item_reverse,
                 group_reverse = group_reverse,
                 ...) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE, nrow = legend_rows, byrow = legend_byrow),
                    color = ggplot2::guide_legend(reverse = TRUE, nrow = legend_rows, byrow = legend_byrow))

  attr(p, which = "stem_plot") <- "stem_plot_battery"

  suppressWarnings(print(p))
}

#' Plot a set of multiselection choice items
#'
#' @param data Dataframe including item (and group) variables
#' @param items Plotted items. Can be selected using Tidyselect's selection helpers.
#' @param group Optional; plotted grouping variable
#' @param weight Optional; survey weights
#' @param item_reverse Optional; should item order be reversed?
#' @param infreq_order Optional; should items be ordered in order of their frequency?
#' @param ... Other arguments passed to [stemtools::stem_plot()]
#' @param backround_fill Color of backround bars. Default is `grey`.
#' @param backround_alpha Alpha of backround bars. Default is `1`.
#'
#' @return A ggplot2 object with custom attribute "stem_plot"
#' @export
#'
#' @examples \dontrun{
#' stem_plot_multiselect(trust, items = dplyr::starts_with("biggest"))
#'}
stem_plot_multiselect <- function(data,
                                  items,
                                  group = NULL,
                                  weight = NULL,
                                  item_reverse = FALSE,
                                  infreq_order = TRUE,
                                  backround_fill = "grey",
                                  backround_alpha = 1,
                                  ...) {
  n_items <- dplyr::select(.data = trust, {{ items }}) |> ncol()

  data <- data |>
    dplyr::select({{ items }}, {{ weight }}, {{ group }}) |>
    tidyr::pivot_longer(-c({{ weight }}, {{ group }}),
                 names_to = "item",
                 values_to = "response")

  if(infreq_order) {data$response <- forcats::fct_infreq(data$response)}

  p <- data |>
    stem_plot(item = response,
              group = {{ group }},
              weight = {{ weight }},
              label_scale = n_items * 100,
              caption = FALSE,
              title = FALSE,
              scale_y = ggplot2::scale_y_continuous(labels = scales::percent_format(scale = n_items*100, suffix = " %"), limits = c(0, 1 / n_items)),
              item_reverse = FALSE,
              ...)

  n_cats <- nrow(p$data)
  p <- p + ggplot2::annotate(geom = "col", x = 1:n_cats, y = 1/(n_items), fill = backround_fill, alpha = backround_alpha)

  p$layers <- list(p$layers[[3]], p$layers[[1]], p$layers[[2]])

  attr(p, which = "stem_plot") <- "stem_plot_multiselect"

  return(p)
}




#' Plot a simple line chart
#'
#' Wrapper around [stemtools::stem_plot()] to make creating line plots easier.
#'
#' @param data Dataframe including item (and group) variables
#' @param item Plotted item on x-axis
#' @param group Optional; plotted grouping variable on y-axis
#' @param weight Optional; survey weights
#' @param scale_y Format y axis using [ggplot2::scale_y_continuous()]
#' @param label Should geom labels be printed? Yes by default
#' @param label_scale Multiplicative scale of geom labels. `100` multiplies the values by 100
#' @param label_hide Hides geom labels with values smaller than this threshold. Defaults to 0.
#' @param ... Other arguments passed to [stemtools::stem_plot()]
#'
#' @return A ggplot2 object with custom attribute "stem_plot"
#' @export
#'
#' @examples
#' stem_plot_line(trust, government, weight = W)
stem_plot_line <- function(data,
                           item,
                           group = NULL,
                           weight = NULL,
                           scale_y = ggplot2::scale_y_continuous(limits = c(0, NA)),
                           label = TRUE,
                           label_scale = 100,
                           label_hide = 0,
                           ...) {
  p <- stem_plot(data = data,
                 item = {{ item }},
                 group = {{ group }},
                 weight = {{weight}},
                 geom = ggplot2::geom_line,
                 scale_y = scale_y,
                 label = label,
                 label_scale = label_scale,
                 label_hide = label_hide,
                 ...)

  attr(p, which = "stem_plot") <- "stem_plot_line"

  return(p)
}

#' Plot a simple scatter chart
#'
#' This function creates a scatter plot directly using ggplot2. It allows for customization of plot aesthetics and includes options for weighting and grouping data.
#'
#' @param data Dataframe including items for x and y axes
#' @param x Plotted item on the x-axis
#' @param y Plotted item on the y-axis
#' @param group Optional; plotted grouping variable
#' @param weight Optional; survey weights
#' @param scale_x Optional; scaling for the x-axis
#' @param scale_y Optional; scaling for the y-axis
#' @param label Should geom labels be printed? Yes by default
#' @param label_scale Multiplicative scale of geom labels
#' @param label_hide Threshold below which labels are hidden
#' @return A ggplot2 object
#' @export
#' @import ggplot2
#' @import scales
#'
#' @examples
#' stem_plot_scatter(data = trust,x = age,y = W)

stem_plot_scatter <- function(data,
                              x,
                              y,
                              group = NULL,
                              weight = NULL,
                              scale_x = ggplot2::scale_x_continuous(),
                              scale_y = ggplot2::scale_y_continuous(),
                              label = TRUE,
                              label_scale = 1,
                              label_hide = 0.05) {

  # Check if group is specified
  group_check <- rlang::enquo(group)

  # Construct the base ggplot object
  p <- ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = {{ x }}, y = {{ y }}))

  # Add points with optional grouping
  if (!rlang::quo_is_null(group_check)) {
    p <- p + ggplot2::geom_point(ggplot2::aes(color = {{ group }}))
  } else {
    p <- p + ggplot2::geom_point()
  }

  # Add scales if specified
  p <- p + scale_x + scale_y

  # Add labels if specified
  if (label) {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = ifelse(is.numeric({{ y }}), scales::number({{ y }}, scale = label_scale), "")),
                                hjust = -0.2,
                                vjust = 1,
                                check_overlap = TRUE)
  }

  # Optionally hide labels below threshold
  if (label && label_hide > 0) {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = ifelse({{ y }} < label_hide, "", scales::number({{ y }}, scale = label_scale))),
                                hjust = -0.2,
                                vjust = 1,
                                check_overlap = TRUE)
  }

  return(p)
}
