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

test_that("stem_battery stops when answers fall outside the categories", {
  lv <- c("Yes", "No")
  df <- data.frame(
    a = factor(c("Yes", "No"), levels = lv),
    b = factor(c("Yes", "No"), levels = rev(lv)),
    c = factor(c("Yes", "Maybe"), levels = c("Yes", "Maybe")),
    d = c("Yes", "Perhaps"),
    e = c("Yes", NA),
    f = factor(c("Yes", "No"), levels = c(lv, "Filtered")),
    stringsAsFactors = FALSE
  )
  # Answers that would be dropped: error naming them.
  expect_error(stem_battery(df, items = c(a, c), item_label = FALSE),
               "* `c` has \"Maybe\"", fixed = TRUE)
  expect_error(stem_battery(df, items = c(a, d), item_label = FALSE),
               "* `d` has \"Perhaps\"", fixed = TRUE)
  # Nothing lost: another level order, an unused extra level (also in the
  # first item), character items within the levels, all-character batteries.
  expect_no_error(stem_battery(df, items = c(a, b), item_label = FALSE))
  expect_no_error(stem_battery(df, items = c(f, a), item_label = FALSE))
  expect_no_error(stem_battery(df, items = c(a, f), item_label = FALSE))
  # (These two only warn that `e` has fewer valid answers.)
  expect_warning(stem_battery(df, items = c(a, e), item_label = FALSE), "valid answers")
  expect_warning(stem_battery(df, items = c(d, e), item_label = FALSE), "valid answers")
})

test_that("stem_battery matches answers by text when level orders differ", {
  lv <- c("Yes", "No")
  df <- data.frame(
    a = factor(c("Yes", "Yes", "Yes", "No"), levels = lv),
    b = factor(c("Yes", "No", "No", "No"), levels = rev(lv))
  )
  p <- stem_battery(df, items = c(a, b), item_label = FALSE)
  b_rows <- p$data[p$data$.battery_item == "b", ]
  expect_equal(b_rows$freq[b_rows$.response == "Yes"], 0.25)
  expect_equal(levels(p$data$.response), lv)
})

test_that("stem_battery keeps every answer when items share their categories", {
  lv <- c("Ano", "Ne", "Nevím")
  df <- data.frame(
    a = factor(c("Ano", "Ne", "Ano", "Ne"), levels = lv),
    b = factor(c("Ano", "Nevím", "Nevím", "Ne"), levels = lv)
  )
  p <- stem_battery(df, items = c(a, b), item_label = FALSE)
  b_rows <- p$data[p$data$.battery_item == "b", ]
  expect_equal(sum(b_rows$freq), 1)
  expect_equal(b_rows$freq[b_rows$.response == "Nevím"], 0.5)
})

test_that("stem_battery stops when items share a label", {
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
})

test_that("stem_battery checks that order_by values are categories", {
  expect_error(
    stem_battery(trust, items = c(police, army), order_by = "Agree"),
    "not found: \"Agree\""
  )
  # One typo among correct values is an error too.
  expect_error(
    stem_battery(trust, items = c(police, army),
                 order_by = c("Definitely Agree", "Rather Agre")),
    "not found: \"Rather Agre\""
  )
  expect_no_warning(
    stem_battery(trust, items = c(police, army),
                 order_by = c("Definitely Agree", "Rather Agree"))
  )
})

test_that("stem_battery orders items by shares without missing answers", {
  lv <- c("Yes", "No")
  # a: 3 of 4 valid answers "Yes" (75 %), plus 4 NA; b: 5 of 8 "Yes" (62.5 %).
  df <- data.frame(
    a = factor(c("Yes", "Yes", "Yes", "No", NA, NA, NA, NA), levels = lv),
    b = factor(c(rep("Yes", 5), rep("No", 3)), levels = lv)
  )
  p <- suppressWarnings(
    stem_battery(df, items = c(a, b), item_label = FALSE, order_by = "Yes")
  )
  expect_equal(levels(p$data$.battery_item), c("a", "b"))
})

test_that("stem_battery warns when items have different numbers of valid answers", {
  lv <- c("Yes", "No")
  df <- data.frame(
    a = factor(c("Yes", "No", NA, NA), levels = lv),
    b = factor(c("Yes", "No", "Yes", NA), levels = lv),
    c = factor(c("Yes", "No", "No", "Yes"), levels = lv)
  )
  expect_warning(
    stem_battery(df, items = c(a, b, c), item_label = FALSE),
    "* `a`: 2\n* `b`: 3\n* `c`: 4",
    fixed = TRUE
  )
  expect_no_warning(stem_battery(df, items = c(c), item_label = FALSE))
  # Missing answers spread evenly: no warning.
  df$c[4] <- NA
  expect_no_warning(stem_battery(df, items = c(b, c), item_label = FALSE))
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
