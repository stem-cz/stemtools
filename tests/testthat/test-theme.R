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
