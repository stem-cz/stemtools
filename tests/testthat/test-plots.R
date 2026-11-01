# These are smoke tests: they check that each plotting function returns a
# ggplot object and that the plot can be built (i.e. the underlying data and
# aesthetics are valid) without error.

build_ok <- function(p) {
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
}

test_that("stem_barplot builds, with and without a group", {
  build_ok(stem_barplot(trust, police))
  build_ok(stem_barplot(trust, police, group = eu_index))
  build_ok(stem_barplot(trust, police, group = eu_index, weight = W))
})

test_that("stem_barplot honours weight, error bars and label toggles", {
  build_ok(stem_barplot(trust, police, weight = W, errorbar = TRUE))
  build_ok(stem_barplot(trust, police, labels = FALSE))
})

test_that("stem_inline builds a single stacked bar", {
  build_ok(stem_inline(trust, police))
  build_ok(stem_inline(trust, police, weight = W))
})

test_that("title_show adds the variable label as the plot title", {
  labelled <- trust
  attr(labelled$police, "label") <- "Trust in the police"

  p_bar <- stem_barplot(labelled, police, title_show = TRUE)
  expect_identical(p_bar$labels$title, "Trust in the police")

  p_inline <- stem_inline(labelled, police, title_show = TRUE)
  expect_identical(p_inline$labels$title, "Trust in the police")

  # Grouped barplot draws its title too.
  p_grouped <- stem_barplot(labelled, police, group = eu_index, title_show = TRUE)
  expect_identical(p_grouped$labels$title, "Trust in the police")

  # No title unless requested.
  expect_null(stem_barplot(labelled, police)$labels$title)
  expect_null(stem_inline(labelled, police)$labels$title)
})

test_that("title_quote wraps the title in low/high double quotes", {
  labelled <- trust
  attr(labelled$police, "label") <- "Trust in the police"

  p_bar <- stem_barplot(labelled, police, title_show = TRUE, title_quote = TRUE)
  expect_identical(p_bar$labels$title, "\u201eTrust in the police\u201c")

  p_inline <- stem_inline(labelled, police, title_show = TRUE, title_quote = TRUE)
  expect_identical(p_inline$labels$title, "\u201eTrust in the police\u201c")
})

test_that("title_wrap breaks long titles onto several lines", {
  labelled <- trust
  long <- paste(
    "How much do you trust the police to act in the best interest of",
    "ordinary citizens living in this country nowadays"
  )
  attr(labelled$police, "label") <- long

  # Default wrap (80) inserts newlines for a long title.
  wrapped <- stem_barplot(labelled, police, title_show = TRUE)$labels$title
  expect_match(wrapped, "\n")
  expect_true(all(nchar(strsplit(wrapped, "\n")[[1]]) <= 80))

  # Narrow wrap produces more lines than a wide one.
  narrow <- stem_barplot(labelled, police, title_show = TRUE, title_wrap = 20)
  wide <- stem_barplot(labelled, police, title_show = TRUE, title_wrap = 200)
  expect_gt(
    length(strsplit(narrow$labels$title, "\n")[[1]]),
    length(strsplit(wide$labels$title, "\n")[[1]])
  )

  # Inf disables wrapping entirely.
  no_wrap <- stem_inline(labelled, police, title_show = TRUE, title_wrap = Inf)
  expect_identical(no_wrap$labels$title, long)

  # Wrapping and quoting combine: the quotes enclose the wrapped block.
  quoted <- stem_barplot(
    labelled, police, title_show = TRUE, title_quote = TRUE, title_wrap = 20
  )$labels$title
  expect_match(quoted, "^\u201e")
  expect_match(quoted, "\u201c$")
  expect_match(quoted, "\n")
})

test_that("title_show falls back to the variable name when no label is present", {
  no_label <- trust
  attr(no_label$police, "label") <- NULL

  p_bar <- stem_barplot(no_label, police, title_show = TRUE)
  expect_identical(p_bar$labels$title, "police")

  p_inline <- stem_inline(no_label, police, title_show = TRUE)
  expect_identical(p_inline$labels$title, "police")

  # Fallback name is quoted too when requested.
  p_quoted <- stem_barplot(no_label, police, title_show = TRUE, title_quote = TRUE)
  expect_identical(p_quoted$labels$title, "\u201epolice\u201c")
})

test_that("stem_battery builds for a set of like items", {
  build_ok(stem_battery(trust, items = c(police, eu, government, army)))
  build_ok(stem_battery(
    trust,
    items = c(police, eu, government, army),
    order_by = c("Definitely Agree", "Rather Agree")
  ))
})

test_that("stem_battery orders items by the plotted shares, ignoring missing answers", {
  lv <- c("Agree", "Disagree")
  df <- data.frame(
    # 50 Agree + 50 missing: drawn as 100 % Agree.
    a = factor(c(rep("Agree", 50), rep(NA, 50)), levels = lv),
    # 60 Agree + 40 Disagree: drawn as 60 % Agree.
    b = factor(c(rep("Agree", 60), rep("Disagree", 40)), levels = lv),
    # 20 Agree + 30 Disagree + 50 missing: drawn as 40 % Agree.
    c = factor(c(rep("Agree", 20), rep("Disagree", 30), rep(NA, 50)), levels = lv)
  )
  p <- stem_battery(df, items = c(a, b, c), item_label = FALSE, order_by = "Agree")
  expect_equal(levels(p$data$.battery_item), c("a", "b", "c"))
  agree <- p$data[p$data$.response == "Agree", ]
  expect_equal(agree$freq[match(c("a", "b", "c"), agree$.battery_item)], c(1, 0.6, 0.4))

  # The bar order survives the native conversion.
  skip_if_not_installed("mschart")
  chart <- stem_as_mschart(p)
  expect_equal(levels(chart$data[[chart$x]]), c("a", "b", "c"))
})

test_that("stem_battery shows a given title, quoted and wrapped like the others", {
  expect_null(stem_battery(trust, items = c(police, eu))$labels$title)

  p <- stem_battery(trust, items = c(police, eu), title = "Trust in institutions")
  expect_identical(p$labels$title, "Trust in institutions")

  p <- stem_battery(trust, items = c(police, eu), title = "Trust in institutions",
                    title_quote = TRUE, title_wrap = 10)
  expect_identical(p$labels$title, "„Trust in\ninstitutions“")

  skip_if_not_installed("mschart")
  chart <- stem_as_mschart(p)
  expect_s3_class(chart, "ms_barchart")
  expect_identical(chart$labels$title, "„Trust in institutions“")
})

test_that("stem_battery stops when items have different categories", {
  lv <- c("Yes", "No")
  df <- data.frame(
    a = factor(c("Yes", "No"), levels = lv),
    b = factor(c("Yes", "No"), levels = rev(lv)),
    c = factor(c("Yes", "Maybe"), levels = c("Yes", "Maybe")),
    d = c("Yes", "Perhaps"),
    e = c("Yes", NA),
    stringsAsFactors = FALSE
  )
  expect_error(stem_battery(df, items = c(a, b), item_label = FALSE),
               "`b` has the same categories in a different order")
  expect_error(stem_battery(df, items = c(a, c), item_label = FALSE),
               "`c` lacks \"No\" and has \"Maybe\"")
  expect_error(stem_battery(df, items = c(a, d), item_label = FALSE),
               "`d` has \"Perhaps\"")
  # Character items within the shared levels, and all-character batteries, are fine.
  expect_no_error(stem_battery(df, items = c(a, e), item_label = FALSE))
  expect_no_error(stem_battery(df, items = c(d, e), item_label = FALSE))

  # Trust's battery items all share one set of levels.
  skip_if_not_installed("mschart")
  expect_s3_class(
    stem_as_mschart(stem_battery(trust, items = c(police, eu, government, army))),
    "ms_barchart"
  )
})

test_that("stem_battery warns and falls back to names when a label is missing", {
  no_label <- trust
  attr(no_label$police, "label") <- NULL
  expect_warning(
    stem_battery(no_label, items = c(police, eu)),
    "no `label` attribute"
  )
})

test_that("stem_battery uses haven value labels as response categories", {
  skip_if_not_installed("haven")
  df <- data.frame(
    a = haven::labelled(c(1, 2, 2, 1), c(Yes = 1, No = 2), label = "Item A"),
    b = haven::labelled(c(2, 2, 1, 1), c(Yes = 1, No = 2), label = "Item B")
  )
  p <- stem_battery(df, items = c(a, b))
  build_ok(p)
  expect_equal(levels(p$data$.response), c("Yes", "No"))
})

test_that("every plotting function accepts SPSS-labelled input", {
  skip_if_not_installed("haven")
  skip_if_not_installed("mschart")
  scale <- c(Agree = 1, Neutral = 2, Disagree = 3)
  df <- data.frame(
    q  = haven::labelled(rep(1:3, 20), scale, label = "Trust in the police"),
    q2 = haven::labelled(rep(3:1, 20), scale, label = "Trust in the army"),
    m1 = haven::labelled(rep(c(1, 2), 30), c(Police = 1, Army = 2)),
    m2 = haven::labelled(rep(c(2, NA), 30), c(Police = 1, Army = 2)),
    g  = haven::labelled(rep(1:2, each = 30), c(Men = 1, Women = 2)),
    w  = rep(c(0.5, 1.5), 30)
  )
  native <- function(p) {
    build_ok(p)
    expect_s3_class(stem_as_mschart(p), "ms_barchart")
    p
  }

  # Ungrouped bar plot: labels become categories, the title the variable label.
  p <- native(stem_barplot(df, q, weight = w, title_show = TRUE))
  expect_equal(levels(p$data$q), names(scale))
  expect_identical(p$labels$title, "Trust in the police")
  # collapse_item refers to the value labels.
  p <- native(stem_barplot(df, q, collapse_item = list(Other = c("Neutral", "Disagree"))))
  expect_equal(levels(p$data$q), c("Agree", "Other"))
  # Grouped bar plot: the group's labels too.
  p <- native(stem_barplot(df, q, group = g, collapse_group = list(All = c("Men", "Women"))))
  expect_equal(levels(p$data$g), "All")
  p <- native(stem_inline(df, q, title_show = TRUE))
  expect_equal(levels(p$data$q), names(scale))
  p <- native(stem_battery(df, items = c(q, q2), weight = w))
  expect_equal(levels(p$data$.battery_item), c("Trust in the police", "Trust in the army"))
  p <- native(stem_multiselect(df, items = c(m1, m2), group = g))
  expect_setequal(levels(p$data$.response), c("Police", "Army"))
})

# don't-know levels --------------------------------------------------------

# trust with "Nevím" (and optionally a second DK level) added to `police`.
trust_dk <- function(second = FALSE) {
  df <- trust
  extra <- c("Nevím", if (second) "Neodpověděl")
  df$police <- forcats::fct_expand(df$police, extra)
  df$police[1:80] <- "Nevím"
  if (second) df$police[81:100] <- "Neodpověděl"
  df
}

fill_of <- function(p) {
  built <- ggplot2::ggplot_build(p)$data[[1]]
  fill_name <- stem_aes_name(p$mapping$fill, p$data)
  levels_ <- levels(droplevels(as.factor(p$data[[fill_name]])))
  stats::setNames(built$fill[match(seq_along(levels_), built$group)], levels_)
}

test_that("don't-know levels go last, grey, outside the palette (stacked plots)", {
  skip_if_not_installed("mschart")
  df <- trust_dk()
  div1 <- as.character(stem_palette("div1"))

  plots <- list(
    inline  = stem_inline(df, police, dk_levels = "Nevím"),
    grouped = stem_barplot(df, police, group = eu_index, dk_levels = "Nevím"),
    battery = {
      df$army <- forcats::fct_expand(df$army, "Nevím")
      df$army[200:240] <- "Nevím"
      stem_battery(df, items = c(police, army), dk_levels = "Nevím")
    }
  )

  for (p in plots) {
    fills <- fill_of(p)
    # A 5-point scale plus "Nevím" uses the 5-colour palette plus grey.
    expect_equal(names(fills)[6], "Nevím")
    expect_equal(unname(fills), c(div1, "#B3B3B3"))
    # The native chart gets the same series, order and colours.
    chart <- stem_as_mschart(p)
    expect_equal(names(chart$series_settings$fill), names(fills))
    expect_equal(unname(unlist(chart$series_settings$fill)), unname(fills))
  }

  # Without dk_levels "Nevím" is a sixth substantive category of the palette.
  fills <- fill_of(stem_inline(df, police))
  expect_equal(unname(fills), stem_palette_gen("div1")(6))
})

test_that("don't-know levels move to the end even when listed first", {
  df <- trust_dk()
  df$police <- forcats::fct_relevel(df$police, "Nevím")
  p <- stem_inline(df, police, dk_levels = c("Nevím", "Not present"))
  expect_equal(levels(p$data$police)[6], "Nevím")
  # Single-colour bar plots put them at the bottom (last level).
  p <- stem_barplot(df, police, dk_levels = "Nevím")
  expect_equal(levels(p$data$police)[6], "Nevím")
})

test_that("ungrouped stem_barplot draws don't-know bars grey, as separate series", {
  skip_if_not_installed("mschart")
  df <- trust_dk(second = TRUE)
  p <- stem_barplot(df, police, dk_levels = c("Nevím", "Neodpověděl"),
                    dk_color = c("grey70", "grey50"), errorbar = TRUE)
  build_ok(p)

  # One bar per category; substantive bars keep the palette's first colour.
  built <- ggplot2::ggplot_build(p)$data[[1]]
  expect_equal(nrow(built), 7)
  bar_fill <- stats::setNames(built$fill, levels(p$data$police)[8 - built$y])
  expect_equal(unname(bar_fill[c("Nevím", "Neodpověděl")]), as_hex(c("grey70", "grey50")))
  expect_true(all(bar_fill[1:5] == "#82143C"))
  expect_null(ggplot2::ggplot_build(p)$plot$guides$fill)

  # Native chart: the main series plus one grey series per DK level, fully
  # overlapping (one bar per category), no legend, same colours.
  chart <- suppressMessages(stem_as_mschart(p))
  expect_equal(names(chart$series_settings$fill), c("%", "Nevím", "Neodpověděl"))
  expect_equal(unname(unlist(chart$series_settings$fill)),
               c("#82143C", as_hex(c("grey70", "grey50"))))
  expect_equal(chart$options$grouping, "clustered")
  expect_equal(chart$options$overlap, 100)
  expect_equal(chart$theme$legend_position, "n")
  expect_equal(levels(chart$data[[chart$x]]), levels(p$data$police))
  # Every category has exactly one value across the series.
  expect_equal(nrow(chart$data), 7)

  # Without DK levels in the data the chart is unchanged: one series.
  chart <- stem_as_mschart(stem_barplot(trust, police, dk_levels = "Nevím"))
  expect_null(chart$group)
  expect_equal(unname(unlist(chart$series_settings$fill)), "#82143C")
})

test_that("several don't-know levels get distinguishable greys", {
  df <- trust_dk(second = TRUE)
  fills <- fill_of(stem_inline(df, police, dk_levels = c("Nevím", "Neodpověděl")))
  expect_equal(unname(fills[6]), "#B3B3B3")
  expect_false(fills[6] == fills[7])

  fills <- fill_of(stem_inline(df, police, dk_levels = c("Nevím", "Neodpověděl"),
                               dk_color = c("grey80", "grey60")))
  expect_equal(unname(fills[6:7]), as_hex(c("grey80", "grey60")))
})

test_that("without dk_levels the stacked colours are unchanged", {
  fills <- fill_of(stem_barplot(trust, police, group = eu_index, direction = -1))
  expect_equal(unname(fills), rev(as.character(stem_palette("div1"))))
})

test_that("stem_multiselect builds, with and without a group", {
  build_ok(stem_multiselect(trust, items = dplyr::starts_with("biggest_concern")))
  build_ok(stem_multiselect(
    trust,
    items = dplyr::starts_with("biggest_concern"),
    group = eu_index
  ))
})

# Yes/no ("Ano"/"Ne") columns with labels, missing answers and weights.
yes_no_data <- function() {
  set.seed(1)
  n <- 200
  draw <- function(p) {
    x <- ifelse(stats::runif(n) < p, "Ano", "Ne")
    x[sample(n, 20)] <- NA
    factor(x, levels = c("Ano", "Ne"))
  }
  df <- data.frame(
    q5_1 = draw(0.2), q5_2 = draw(0.7), q5_3 = draw(0.5), q5_4 = draw(0.9),
    w = stats::runif(n, 0.5, 1.5),
    sex = factor(rep(c("Muž", "Žena"), n / 2))
  )
  attr(df$q5_1, "label") <- "Ceny"
  attr(df$q5_2, "label") <- "Zdraví"
  attr(df$q5_3, "label") <- "Bydlení"
  attr(df$q5_4, "label") <- "Jiné"
  df
}

test_that("stem_multiselect(selected =) draws one bar per yes/no item", {
  skip_if_not_installed("mschart")
  df <- yes_no_data()
  p <- stem_multiselect(df, items = q5_1:q5_4, weight = w, selected = "Ano")

  # One bar per item, named by its label; freq in 0-1.
  expect_setequal(as.character(p$data$.response), c("Ceny", "Zdraví", "Bydlení", "Jiné"))
  expect_true(all(p$data$freq >= 0 & p$data$freq <= 1))
  # The share among valid answers, weighted.
  ok <- !is.na(df$q5_2)
  expect_equal(
    p$data$freq[p$data$.response == "Zdraví"],
    sum(df$w[ok & df$q5_2 == "Ano"]) / sum(df$w[ok])
  )
  # Sorted by share: the largest bar is the last level (drawn on top).
  expect_equal(
    levels(p$data$.response),
    as.character(p$data$.response[order(p$data$freq)])
  )

  # keep_last pins items to the bottom (the first levels), by label or name.
  p2 <- stem_multiselect(df, items = q5_1:q5_4, selected = "Ano", keep_last = "Jiné")
  expect_equal(levels(p2$data$.response)[1], "Jiné")
  p3 <- stem_multiselect(df, items = q5_1:q5_4, selected = "Ano",
                         keep_last = c("q5_4", "Bydlení"))
  expect_equal(levels(p3$data$.response)[1:2], c("Bydlení", "Jiné"))

  chart <- stem_as_mschart(p2)
  expect_s3_class(chart, "ms_barchart")
  expect_equal(levels(chart$data[[chart$x]])[1], "Jiné")
  expect_true(all(chart$data[["%"]] <= 100))
})

test_that("stem_multiselect(selected =) works with a group", {
  skip_if_not_installed("mschart")
  df <- yes_no_data()
  # Nobody among men chose item 1: their share is 0, not missing.
  df$q5_1[df$sex == "Muž"] <- "Ne"
  p <- stem_multiselect(df, items = q5_1:q5_4, group = sex, selected = "Ano")
  expect_equal(nrow(p$data), 8)
  expect_equal(p$data$freq[p$data$sex == "Muž" & p$data$.response == "Ceny"], 0)
  ok <- !is.na(df$q5_3) & df$sex == "Žena"
  expect_equal(
    p$data$freq[p$data$sex == "Žena" & p$data$.response == "Bydlení"],
    mean(df$q5_3[ok] == "Ano")
  )
  chart <- stem_as_mschart(p)
  expect_equal(chart$options$grouping, "clustered")
})

test_that("stem_multiselect(selected =) checks its input", {
  df <- yes_no_data()
  expect_error(stem_multiselect(df, items = q5_1:q5_4, selected = "Yes"),
               "None of the items has the answer \"Yes\"")
  attr(df$q5_4, "label") <- NULL
  expect_warning(
    p <- stem_multiselect(df, items = q5_1:q5_4, selected = "Ano"),
    "no `label` attribute"
  )
  expect_true("q5_4" %in% levels(p$data$.response))
})

test_that("stem_multiselect mention mode is unchanged and accepts keep_last", {
  p <- stem_multiselect(trust, items = dplyr::starts_with("biggest_concern"))
  lv <- levels(p$data$.response)
  p2 <- stem_multiselect(trust, items = dplyr::starts_with("biggest_concern"),
                         keep_last = lv[length(lv)])
  expect_equal(levels(p2$data$.response), c(lv[length(lv)], lv[-length(lv)]))
  expect_equal(sort(p2$data$freq), sort(p$data$freq))
})

test_that("stem_multiselect can drop the background bars", {
  build_ok(stem_multiselect(
    trust,
    items = dplyr::starts_with("biggest_concern"),
    background = FALSE
  ))
})

# internal helpers -------------------------------------------------------

test_that("format_pct hides small proportions and formats the rest", {
  out <- format_pct(c(0.02, 0.5), hide = 0.05)
  expect_identical(out[1], "")
  expect_match(out[2], "%")
})

test_that("column_labels returns labels and NA where missing", {
  df <- data.frame(a = 1, b = 2)
  attr(df$a, "label") <- "Alpha"
  labs <- column_labels(df)
  expect_identical(unname(labs[["a"]]), "Alpha")
  expect_true(is.na(labs[["b"]]))
})

# stem_boxplot -------------------------------------------------------------

test_that("stem_boxplot draws horizontal boxes with a mean mark", {
  p <- stem_boxplot(trust, age)
  build_ok(p)
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomBoxplot" %in% geoms)
  expect_equal(sum(geoms == "GeomPoint"), 1 + (nrow(stem_box_stats(trust, age)$outliers) > 0))

  p <- stem_boxplot(trust, age, group = eu_index, weight = W, title_show = TRUE)
  build_ok(p)
  expect_equal(nrow(p$data), nlevels(trust$eu_index))
  expect_identical(p$labels$title, attr(trust$age, "label") %||% "age")
  built <- ggplot2::ggplot_build(p)$data[[1]]
  expect_true(all(built$flipped_aes))
})

test_that("stem_boxplot keeps the mean visible for a zero-width box", {
  df <- data.frame(x = c(rep(5, 20), 9))
  p <- stem_boxplot(df, x)
  build_ok(p)
  expect_equal(p$data$q1, p$data$q3)
  mean_layer <- ggplot2::ggplot_build(p)$data[[2]]
  expect_equal(mean_layer$x, mean(df$x))
  expect_equal(mean_layer$fill, "white")
})

test_that("stem_as_mschart refuses boxplots with a clear message", {
  expect_error(stem_as_mschart(stem_boxplot(trust, age)),
               "Boxplots can't be converted to native charts; save them as images")
})

# brand colour --------------------------------------------------------------

test_that("single-series bars are burgundy whatever the palette, in ggplot and native charts", {
  skip_if_not_installed("mschart")

  for (pal in c("div1", "seq2")) {
    p <- stem_barplot(trust, government, palette = pal)
    expect_equal(unique(ggplot2::ggplot_build(p)$data[[1]]$fill), "#82143C")
    expect_equal(unname(unlist(stem_as_mschart(p)$series_settings$fill)), "#82143C")
  }
  p <- stem_barplot(trust, government, bar_color = "navy")
  expect_equal(unname(unlist(stem_as_mschart(p)$series_settings$fill)), "navy")

  p <- stem_multiselect(trust, items = dplyr::starts_with("biggest_concern"))
  bars <- ggplot2::ggplot_build(p)$data[[2]] # layer 1 holds the background bars
  expect_equal(unique(bars$fill), "#82143C")
  expect_equal(unname(unlist(stem_as_mschart(p)$series_settings$fill)), "#82143C")

  expect_equal(theme_stem()$geom$accent, "#82143C")
})

# full scale ------------------------------------------------------------------

test_that("an empty scale point does not shift the other colours", {
  skip_if_not_installed("mschart")
  div1 <- as.character(stem_palette("div1"))
  df <- trust[trust$police != "Neither Agree nor Disagree", ]

  plots <- list(
    stem_inline(df, police),
    stem_barplot(df, police, group = eu_index),
    stem_battery(df, items = c(police, army), item_label = FALSE)
  )
  for (p in plots) {
    fills <- fill_of(p)
    expect_equal(unname(fills), div1)
    expect_equal(fills[["Neither Agree nor Disagree"]], div1[3])
    # Label colours come from the same full scale: white on the extremes only.
    by_level <- vapply(split(p$data$.label_color, p$data[[stem_aes_name(p$mapping$fill, p$data)]]),
                       `[`, "", 1)
    expect_equal(unname(by_level), c("white", "black", "black", "black", "white"))
    chart <- stem_as_mschart(p)
    expect_equal(unname(unlist(chart$series_settings$fill)), div1)
  }
})

test_that("titles wrap at 73 characters by default", {
  long <- paste(rep("Jak byste celkově hodnotil(a) práci vlády", 4), collapse = " ")
  df <- trust
  attr(df$police, "label") <- long
  for (p in list(stem_barplot(df, police, title_show = TRUE),
                 stem_inline(df, police, title_show = TRUE),
                 stem_battery(df, items = c(police, army), title = long))) {
    lines <- strsplit(p$labels$title, "\n")[[1]]
    expect_gt(length(lines), 1)
    expect_true(all(nchar(lines) <= 73))
    expect_identical(lines, strwrap(long, 73))
  }
  df$age <- structure(df$age, label = long)
  lines <- strsplit(stem_boxplot(df, age, title_show = TRUE)$labels$title, "\n")[[1]]
  expect_identical(lines, strwrap(long, 73))
})

test_that("items with the same label stop with an error naming them", {
  df <- trust
  attr(df$army, "label") <- attr(df$police, "label")
  attr(df$eu, "label") <- attr(df$police, "label")
  expect_error(
    stem_battery(df, items = c(police, army, eu, government)),
    paste0("\"", attr(trust$police, "label"), "\" (`police`, `army`, `eu`)"),
    fixed = TRUE
  )
  # Variable names are unique, so item_label = FALSE still works.
  expect_no_error(stem_battery(df, items = c(police, army), item_label = FALSE))

  yn <- yes_no_data()
  attr(yn$q5_2, "label") <- "Ceny"
  expect_error(stem_multiselect(yn, items = q5_1:q5_4, selected = "Ano"),
               "\"Ceny\" (`q5_1`, `q5_2`)", fixed = TRUE)
})
