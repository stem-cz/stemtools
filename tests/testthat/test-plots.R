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
})

test_that("stem_barplot honours weight, error bars and label toggles", {
  build_ok(stem_barplot(trust, police, weight = W, errorbar = TRUE))
  build_ok(stem_barplot(trust, police, labels = FALSE))
})

test_that("stem_barstack builds", {
  build_ok(stem_barstack(trust, police, group = eu_index))
  build_ok(stem_barstack(trust, police, group = eu_index, weight = W))
})

test_that("stem_inline builds a single stacked bar", {
  build_ok(stem_inline(trust, police))
  build_ok(stem_inline(trust, police, weight = W))
})

test_that("stem_battery builds for a set of like items", {
  build_ok(stem_battery(trust, items = c(police, eu, government, army)))
  build_ok(stem_battery(
    trust,
    items = c(police, eu, government, army),
    order_by = c("Definitely Agree", "Rather Agree")
  ))
})

test_that("stem_battery warns and falls back to names when a label is missing", {
  no_label <- trust
  attr(no_label$police, "label") <- NULL
  expect_warning(
    stem_battery(no_label, items = c(police, eu)),
    "no `label` attribute"
  )
})

test_that("stem_multiselect builds, with and without a group", {
  build_ok(stem_multiselect(trust, items = dplyr::starts_with("biggest_concern")))
  build_ok(stem_multiselect(
    trust,
    items = dplyr::starts_with("biggest_concern"),
    group = eu_index
  ))
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
