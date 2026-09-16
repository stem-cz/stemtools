# Native Microsoft Office charts ------------------------------------------

skip_if_no_office <- function() {
  testthat::skip_if_not_installed("mschart")
  testthat::skip_if_not_installed("officer")
}

# Calibri is not installed on every machine; use the device default so the
# tests never depend on the font.
local_stem_theme <- function(env = parent.frame()) {
  withr_theme <- ggplot2::theme_set(theme_stem(family = ""))
  withr::defer(ggplot2::theme_set(withr_theme), envir = env)
}

test_that("stem_num_fmt() reads accuracy and suffix off the labels", {
  expect_equal(stem_num_fmt(c("1", "20", "39")), "0")
  expect_equal(stem_num_fmt(c("1.5", "20.0")), "0.0")
  expect_equal(stem_num_fmt(c("1 %", "20 %")), "0\" %\"")
  expect_equal(stem_num_fmt(c("1.25 %")), "0.00\" %\"")
  expect_equal(stem_num_fmt(c("", NA)), "0")
})

test_that("stem_levels() keeps factor levels and drops unused ones", {
  f <- factor(c("b", "a"), levels = c("a", "b", "c"))
  expect_equal(stem_levels(f), c("a", "b"))
  expect_equal(stem_levels(c("b", "a")), c("a", "b"))
})

test_that("stem_as_mschart() converts an ungrouped bar plot", {
  skip_if_no_office()
  local_stem_theme()

  chart <- stem_as_mschart(stem_barplot(trust, government))

  expect_s3_class(chart, "ms_barchart")
  expect_equal(chart$options$grouping, "clustered")
  expect_equal(chart$options$dir, "horizontal")
  # The Stem plots reverse the discrete y axis, so the first category is on top,
  # and the value axis has to cross at the far end to stay below the bars.
  expect_equal(chart$x_axis$orientation, "maxMin")
  expect_equal(chart$y_axis$crosses, "max")
  # Values reach the worksheet as percentages, in the ggplot's category order.
  expect_equal(
    levels(chart$data[[chart$x]]),
    levels(trust$government)
  )
  expect_equal(sum(chart$data[["%"]]), 100)
  expect_equal(unname(unlist(chart$series_settings$fill)), stem_palette()[1])
})

test_that("stem_as_mschart() converts stacked plots with the right palette", {
  skip_if_no_office()
  local_stem_theme()

  chart <- stem_as_mschart(stem_barplot(trust, police, group = eu_index))

  expect_equal(chart$options$grouping, "stacked")
  expect_equal(
    unname(unlist(chart$series_settings$fill)),
    as.character(stem_palette("div1"))
  )
  expect_equal(names(chart$series_settings$fill), levels(trust$police))
  # Every stacked bar sums to 100%.
  totals <- tapply(chart$data[["%"]], chart$data[[chart$x]], sum)
  expect_equal(as.vector(round(totals)), rep(100, nlevels(trust$eu_index)))
  # Labels on the two side categories are white, as in stem_stack().
  label_colors <- vapply(
    chart$series_settings$labels_fp,
    function(x) x$color,
    character(1)
  )
  expect_equal(unname(label_colors[c(1, 5)]), c("white", "white"))
  expect_equal(unname(label_colors[3]), "black")
})

test_that("stem_as_mschart() handles inline, battery and multiselect plots", {
  skip_if_no_office()
  local_stem_theme()

  inline <- stem_as_mschart(stem_inline(trust, police))
  expect_equal(inline$options$grouping, "stacked")
  expect_equal(nlevels(inline$data[[inline$x]]), 1L)

  battery <- stem_as_mschart(
    stem_battery(trust, items = c(police, eu, government, army))
  )
  expect_equal(battery$options$grouping, "stacked")
  expect_equal(nlevels(battery$data[[battery$x]]), 4L)

  multi <- stem_as_mschart(
    stem_multiselect(trust, items = dplyr::starts_with("biggest_concern"))
  )
  expect_equal(multi$options$grouping, "clustered")
  # stem_multiselect() does not reverse the y axis, so the category order stands
  # and the value axis crosses at the default end.
  expect_equal(multi$x_axis$orientation, "minMax")
  expect_equal(multi$y_axis$crosses, "autoZero")
})

test_that("stem_as_mschart() carries over the plot title and drops error bars", {
  skip_if_no_office()
  local_stem_theme()

  chart <- stem_as_mschart(stem_barplot(trust, government, title_show = TRUE))
  expect_equal(chart$labels$title, "Vláda ČR")

  expect_message(
    stem_as_mschart(stem_barplot(trust, government, errorbar = TRUE)),
    "error bars"
  )

  untitled <- stem_as_mschart(stem_barplot(trust, government), title = NA)
  expect_null(untitled$labels$title)
})

test_that("stem_as_mschart() rejects plots it cannot read", {
  skip_if_no_office()

  expect_error(stem_as_mschart("not a plot"), "ggplot2 object")
  expect_error(
    stem_as_mschart(ggplot2::ggplot(mtcars, ggplot2::aes(mpg, cyl))),
    "freq"
  )
})

test_that("export helpers write native Office files", {
  skip_if_no_office()
  local_stem_theme()

  plots <- list(
    stem_barplot(trust, government),
    stem_battery(trust, items = c(police, eu, government, army))
  )

  pptx <- withr::local_tempfile(fileext = ".pptx")
  stem_export_pptx(plots, path = pptx)
  expect_true(file.exists(pptx))
  # Native charts ship an embedded worksheet, one per chart.
  contents <- utils::unzip(pptx, list = TRUE)$Name
  expect_length(grep("^ppt/charts/chart", contents), 2L)
  expect_length(grep("^ppt/embeddings/.*\\.xlsx$", contents), 2L)

  docx <- withr::local_tempfile(fileext = ".docx")
  stem_export_docx(plots[[1]], path = docx)
  expect_true(file.exists(docx))
  expect_length(
    grep("\\.xlsx$", utils::unzip(docx, list = TRUE)$Name),
    1L
  )

  expect_error(stem_export_pptx(path = pptx), "No plots")
})

test_that("chart sizes are given in centimetres", {
  skip_if_no_office()
  local_stem_theme()

  expect_equal(stem_to_inches(2.54), 1)
  expect_equal(stem_to_inches(25.4, "mm"), 1)
  expect_equal(stem_to_inches(3, "in"), 3)
  expect_null(stem_to_inches(NULL))

  # No size given: fall back to the layout's body placeholder.
  expect_s3_class(stem_ph_location(), "location_type")

  loc <- stem_ph_location(width = 24, height = 12, left = 2, top = 4)
  expect_equal(loc$width, 24 / 2.54)
  expect_equal(loc$height, 12 / 2.54)
  expect_equal(loc$left, 2 / 2.54)
  expect_equal(loc$top, 4 / 2.54)

  expect_error(stem_ph_location(width = 24), "both")

  # Sizes reach the file in EMU: 360000 EMU to the centimetre.
  pptx <- withr::local_tempfile(fileext = ".pptx")
  stem_export_pptx(
    stem_barplot(trust, government),
    path = pptx,
    width = 24,
    height = 12,
    left = 2,
    top = 4
  )
  slide <- readLines(unzip(pptx, "ppt/slides/slide1.xml", exdir = tempdir()),
    warn = FALSE
  )
  expect_match(paste(slide, collapse = ""), "<a:off x=\"720000\" y=\"1440000\"/>")
  expect_match(paste(slide, collapse = ""), "<a:ext cx=\"8640000\" cy=\"4320000\"/>")
})

test_that("stem_add_chart() adds to both document types", {
  skip_if_no_office()
  local_stem_theme()

  chart <- stem_as_mschart(stem_barplot(trust, government))

  pptx <- officer::read_pptx()
  pptx <- officer::add_slide(pptx, "Title and Content", "Office Theme")
  expect_s3_class(stem_add_chart(pptx, chart), "rpptx")

  expect_s3_class(stem_add_chart(officer::read_docx(), chart), "rdocx")

  expect_error(stem_add_chart(list(), chart), "officer")
})

test_that("stem_label_size() reads the resolved label size in points", {
  local_stem_theme()

  p <- stem_barplot(trust, government)
  expect_equal(stem_label_size(p, ggplot2::ggplot_build(p)), 14)

  p18 <- stem_barplot(trust, government) + theme_stem(family = "", label_size = 18)
  expect_equal(stem_label_size(p18, ggplot2::ggplot_build(p18)), 18)

  # No text layer at all: fall back rather than error.
  p0 <- stem_barplot(trust, government, labels = FALSE)
  expect_equal(stem_label_size(p0, ggplot2::ggplot_build(p0)), 11)
})

test_that("data labels are exported at 11/14 of their ggplot size", {
  spec <- stem_chart_spec(stem_barplot(trust, government) + theme_stem(family = ""))
  expect_equal(spec$label_size, 11)

  # The scale follows a custom `label_size`, rounded to half points.
  spec18 <- stem_chart_spec(
    stem_barplot(trust, government) + theme_stem(family = "", label_size = 18)
  )
  expect_equal(spec18$label_size, 14)

  expect_equal(spec$font_size, 18)
})

test_that("stem_label_fp() sizes the labels with the export size", {
  spec <- stem_chart_spec(stem_barplot(trust, government) + theme_stem(family = ""))
  fp <- stem_label_fp(spec, "x")
  expect_equal(fp[["x"]]$font.size, 11)
})

test_that("the mschart theme uses the pinned export type sizes", {
  skip_if_no_office()

  spec <- stem_chart_spec(stem_barplot(trust, government) + theme_stem(family = ""))
  theme <- stem_mschart_theme(spec)

  expect_equal(theme$axis_text_x$font.size, 11)
  expect_equal(theme$axis_text_y$font.size, 11)
  expect_equal(theme$legend_text$font.size, 11)
  expect_equal(theme$axis_title_x$font.size, 12)
  expect_equal(theme$main_title$font.size, 14)

  # The family and the colours still follow the plot's theme.
  expect_equal(theme$axis_text_x$font.family, spec$font_family)
  expect_equal(theme$axis_text_x$color, spec$ink)

  # A larger base text size no longer leaks into the exported chart.
  big <- stem_chart_spec(
    stem_barplot(trust, government) +
      theme_stem(family = "", text = ggplot2::element_text(size = 24))
  )
  expect_equal(big$font_size, 24)
  expect_equal(stem_mschart_theme(big)$axis_text_x$font.size, 11)
})

test_that("charts follow the app's bar geometry and axis steps", {
  skip_if_no_office()
  local_stem_theme()

  stacked <- stem_as_mschart(stem_battery(trust, c(government, army)))
  expect_equal(stacked$options$gap_width, 30)
  expect_equal(stacked$y_axis$major_unit, 25)

  simple <- stem_as_mschart(stem_barplot(trust, government))
  expect_equal(simple$options$gap_width, 30)
  expect_equal(simple$y_axis$major_unit, 10)

  # Overridable.
  expect_equal(
    stem_as_mschart(stem_barplot(trust, government), axis_major_unit = 20)$y_axis$major_unit,
    20
  )
})

test_that("data labels print the plot's own text, blanks included", {
  skip_if_no_office()
  local_stem_theme()

  plot <- stem_battery(trust, c(government, army), label_hide = 0.2)
  chart <- stem_as_mschart(plot)
  expect_equal(chart$label_cols, ".stem_label")

  # The exported text is the plot's own, blanks from `label_hide` included.
  labels <- chart$data[[".stem_label"]]
  expect_setequal(labels, plot$data$stem_label)
  expect_true(any(labels == ""))

  # An explicit `num_fmt` labels the worksheet values instead.
  values <- stem_as_mschart(stem_barplot(trust, government), num_fmt = "0.0")
  expect_null(values$label_cols)
  expect_false(".stem_label" %in% names(values$data))
})

test_that("bar separators keep the ggplot line width", {
  skip_if_no_office()
  local_stem_theme()

  spec <- stem_chart_spec(stem_barplot(trust, government))
  expect_equal(spec$border_width, 0.5 * ggplot2::.pt)
})
