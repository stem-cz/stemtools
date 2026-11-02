test_that("theme_stem returns a ggplot2 theme", {
  th <- theme_stem()
  expect_s3_class(th, "theme")
})

test_that("theme_stem exposes the font family through the family argument", {
  th <- theme_stem(family = "Helvetica")
  expect_identical(th$text$family, "Helvetica")
})

test_that("theme_stem draws the plot title in bold", {
  th <- theme_stem()
  expect_identical(ggplot2::calc_element("plot.title", th)$face, "bold")
})

test_that("theme_stem passes ink through to the text colour", {
  th <- theme_stem(ink = "navy")
  expect_identical(th$text$colour, "navy")
})

test_that("theme_stem forwards extra arguments to ggplot2::theme", {
  th <- theme_stem(legend.position = "right")
  expect_identical(th$legend.position, "right")
})

test_that("theme_stem can be added to a plot", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) +
    ggplot2::geom_point() +
    theme_stem(family = "")
  expect_s3_class(p, "ggplot")
})

test_that("theme_stem uses a 13 pt base, 15.6 pt titles and 12 pt labels", {
  th <- theme_stem()
  expect_equal(th$text$size, 13)
  expect_equal(ggplot2::calc_element("plot.title", th)$size, 15.6)
  expect_equal(ggplot2::calc_element("axis.text.y", th)$size, 13 * 0.8)
  expect_equal(th$geom$fontsize * ggplot2::.pt, 12)

  th <- theme_stem(base_size = 18)
  expect_equal(ggplot2::calc_element("plot.title", th)$size, 18 * 1.2)
})

test_that("native charts keep their fixed type sizes with the new theme", {
  spec <- stem_chart_spec(stem_barplot(trust, government) + theme_stem(family = ""))
  expect_equal(spec$label_size, 11)
})

test_that("theme_stem left-aligns the legend and leaves room for \"100 %\"", {
  th <- theme_stem()
  expect_identical(th$legend.location, "plot")
  expect_identical(th$legend.justification.top, "left")
  expect_gte(as.numeric(th$plot.margin[2]), 18)
})

test_that("plotting functions attach theme_stem() unless theme = NULL", {
  plots <- list(
    stem_barplot(trust, police),
    stem_barplot(trust, police, group = eu_index),
    stem_inline(trust, police),
    stem_battery(trust, items = c(police, army)),
    stem_battery(trust, items = c(police, army, eu), items_per_chart = 2)[[1]],
    stem_multiselect(trust, items = dplyr::starts_with("biggest_concern")),
    stem_boxplot(trust, age)
  )
  for (p in plots) {
    expect_equal(p$theme$text$size, 13)
    expect_identical(p$theme$legend.location, "plot")
  }

  # theme = NULL leaves the session theme in charge.
  bare <- stem_barplot(trust, police, theme = NULL)
  expect_length(bare$theme, 0)

  # A variant passed in, or added afterwards, wins.
  expect_equal(stem_inline(trust, police, theme = theme_stem(base_size = 18))$theme$text$size, 18)
  expect_equal((stem_inline(trust, police) + theme_stem(base_size = 10))$theme$text$size, 10)
})

test_that("the stemtools.family option sets the default font", {
  withr::local_options(stemtools.family = "")
  expect_identical(theme_stem()$text$family, "")
  expect_identical(stem_barplot(trust, police)$theme$text$family, "")
  withr::local_options(stemtools.family = NULL)
  expect_identical(theme_stem()$text$family, "Calibri")
})
