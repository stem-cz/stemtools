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
