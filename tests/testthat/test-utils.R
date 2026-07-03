# Standard errors --------------------------------------------------------

test_that("se_prop matches the closed-form formula", {
  expect_equal(se_prop(0.5, 100), sqrt(0.25 / 100))
  expect_equal(se_prop(0.3, 200), sqrt((0.3 * 0.7) / 200))
})

test_that("se_prop is vectorised over p", {
  p <- c(0.1, 0.5, 0.9)
  expect_equal(se_prop(p, 100), sqrt((p * (1 - p)) / 100))
})

test_that("se_mean matches the closed-form formula and ignores NA", {
  x <- c(1, 2, 3, 4, 5)
  expect_equal(se_mean(x), sqrt(stats::var(x) / length(x)))
  expect_equal(se_mean(c(1, 2, NA)), sqrt(stats::var(c(1, 2), na.rm = TRUE) / 2))
})

test_that("se_mean rejects non-numeric input", {
  expect_error(se_mean(letters))
})

test_that("se_median is the normal-theory inflation of se_mean", {
  x <- trust$age
  expect_equal(se_median(x), 1.2533 * se_mean(x))
})

test_that("se_median rejects non-numeric input", {
  expect_error(se_median(factor(letters)))
})

# collapse_cats ----------------------------------------------------------

test_that("collapse_cats merges the requested levels", {
  out <- collapse_cats(trust, police, list(Agree = c("Definitely Agree", "Rather Agree")))
  expect_s3_class(out$police, "factor")
  expect_true("Agree" %in% levels(out$police))
  expect_false(any(c("Definitely Agree", "Rather Agree") %in% levels(out$police)))
})

test_that("collapse_cats preserves the number of rows", {
  out <- collapse_cats(trust, police, list(Agree = c("Definitely Agree", "Rather Agree")))
  expect_identical(nrow(out), nrow(trust))
})

test_that("collapse_cats can rename a single level", {
  out <- collapse_cats(trust, police, list(Yes = "Definitely Agree"))
  expect_true("Yes" %in% levels(out$police))
  expect_false("Definitely Agree" %in% levels(out$police))
})
