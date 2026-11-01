# stem_summarise_cat -----------------------------------------------------

test_that("stem_summarise_cat returns the documented columns", {
  out <- stem_summarise_cat(trust, police)
  expect_named(out, c("police", "freq", "freq_low", "freq_upp"))
  expect_s3_class(out, "tbl_df")
})

test_that("stem_summarise_cat proportions sum to 1", {
  out <- stem_summarise_cat(trust, police)
  expect_equal(sum(out$freq), 1)
})

test_that("stem_summarise_cat proportions sum to 1 within each group", {
  out <- stem_summarise_cat(trust, police, group = eu_index, min_cell_n = NULL)
  sums <- tapply(out$freq, out$eu_index, sum)
  expect_equal(as.numeric(sums), rep(1, length(sums)))
})

test_that("stem_summarise_cat confidence bounds bracket the estimate", {
  out <- stem_summarise_cat(trust, police)
  expect_true(all(out$freq_low <= out$freq))
  expect_true(all(out$freq_upp >= out$freq))
})

test_that("stem_summarise_cat return_n adds the absolute-size columns", {
  out <- stem_summarise_cat(trust, police, group = eu_index, return_n = TRUE,
                            min_cell_n = NULL)
  expect_true(all(c("n", "group_n", "item_n") %in% names(out)))
  expect_equal(sum(out$n), nrow(trust))
})

test_that("stem_summarise_cat collapses item categories", {
  out <- stem_summarise_cat(
    trust, police,
    collapse_item = list(Agree = c("Definitely Agree", "Rather Agree"))
  )
  expect_true("Agree" %in% as.character(out$police))
  expect_false("Definitely Agree" %in% as.character(out$police))
})

test_that("stem_summarise_cat supports survey weights", {
  out <- stem_summarise_cat(trust, police, weight = W)
  expect_named(out, c("police", "freq", "freq_low", "freq_upp"))
  expect_equal(sum(out$freq), 1)
})

test_that("stem_summarise_cat long format adds item/item_cat columns", {
  out <- stem_summarise_cat(trust, police, long = TRUE)
  expect_true(all(c("item", "item_cat") %in% names(out)))
  expect_true(all(out$item == "police"))
})

# stem_summarise_num -----------------------------------------------------

test_that("stem_summarise_num returns the documented columns and correct mean", {
  out <- stem_summarise_num(trust, age)
  expect_named(out, c("mean", "mean_low", "mean_upp"))
  expect_equal(out$mean, mean(trust$age))
})

test_that("stem_summarise_num computes a mean per group", {
  out <- stem_summarise_num(trust, age, group = eu_index)
  expect_equal(nrow(out), nlevels(trust$eu_index))
  expect_true(all(out$mean_low <= out$mean & out$mean <= out$mean_upp))
})

test_that("stem_summarise_num supports survey weights", {
  out <- stem_summarise_num(trust, age, weight = W)
  expect_true("mean" %in% names(out))
  expect_true(is.finite(out$mean))
})

# stem_summarise dispatch ------------------------------------------------

test_that("stem_summarise dispatches on the item type", {
  expect_true("freq" %in% names(stem_summarise(trust, police)))
  expect_true("mean" %in% names(stem_summarise(trust, age)))
})

test_that("stem_summarise errors on an unsupported item type", {
  df <- data.frame(flag = c(TRUE, FALSE, TRUE))
  expect_error(stem_summarise(df, flag), "must be a factor")
})

# uncertainty measures ---------------------------------------------------

test_that("variance returns the requested measures, consistent with each other", {
  out <- stem_summarise_cat(trust, police, weight = W,
                            variance = c("se", "var", "cv", "ci", "moe", "deff"))
  expect_named(out, c("police", "freq", "freq_se", "freq_var", "freq_cv",
                      "freq_low", "freq_upp", "freq_moe", "freq_deff"))
  expect_equal(out$freq_var, out$freq_se^2)
  expect_equal(out$freq_cv, out$freq_se / out$freq)
  expect_equal(out$freq_moe, stats::qnorm(0.975) * out$freq_se)
  expect_equal(out$freq_upp - out$freq_low, 2 * out$freq_moe)
  expect_true(all(out$freq_deff > 1))

  out <- stem_summarise_num(trust, age, variance = c("se", "deff"))
  expect_named(out, c("mean", "mean_se", "mean_deff"))
  expect_equal(out$mean_se, se_mean(trust$age))
  expect_equal(out$mean_deff, 1)
})

test_that("variance = NULL returns point estimates only", {
  expect_named(stem_summarise_cat(trust, police, variance = NULL), c("police", "freq"))
  expect_named(stem_summarise_num(trust, age, weight = W, variance = NULL), "mean")
})

test_that("conf_level widens or narrows the interval", {
  ci90 <- stem_summarise_num(trust, age, weight = W, conf_level = 0.90)
  ci99 <- stem_summarise_num(trust, age, weight = W, conf_level = 0.99)
  expect_lt(ci90$mean_upp - ci90$mean_low, ci99$mean_upp - ci99$mean_low)
})

test_that("invalid variance or conf_level errors", {
  expect_error(stem_summarise_cat(trust, police, variance = "sd"), "must be one or more")
  expect_error(stem_summarise_num(trust, age, conf_level = 95), "between 0 and 1")
})

test_that("weighted estimates match the survey package", {
  design <- survey::svydesign(ids = ~1, weights = ~W, data = trust)
  ref <- survey::svymean(~age, design)
  out <- stem_summarise_num(trust, age, weight = W, variance = "se")
  expect_equal(out$mean, unname(stats::coef(ref)))
  expect_equal(out$mean_se, as.numeric(survey::SE(ref)))

  ref <- survey::svymean(~police, design)
  out <- stem_summarise_cat(trust, police, weight = W, variance = "se")
  expect_equal(out$freq, unname(stats::coef(ref)))
  expect_equal(out$freq_se, as.numeric(survey::SE(ref)))
})

# weighted N --------------------------------------------------------------

test_that("n_weighted is the sum of weights per cell", {
  out <- stem_summarise_cat(trust, police, group = eu_index, weight = W,
                            n_weighted = TRUE, min_cell_n = NULL)
  expect_equal(sum(out$n_weighted), sum(trust$W))
  expect_equal(out$n_weighted[1],
               sum(trust$W[trust$eu_index == out$eu_index[1] & trust$police == out$police[1]]))

  out <- stem_summarise_num(trust, age, n_weighted = TRUE, return_n = TRUE)
  expect_equal(out$n_weighted, out$n)
})

# small-cell warning ------------------------------------------------------

test_that("cells below min_cell_n raise a classed warning", {
  expect_warning(stem_summarise_cat(trust, police, group = eu_index),
                 class = "stemtools_warning_small_cell")
  expect_warning(stem_summarise_cat(trust, police, weight = W, min_cell_n = 100),
                 "2 cells have fewer than 100")
  expect_no_warning(stem_summarise_cat(trust, police, group = eu_index, min_cell_n = NULL))
  expect_no_warning(stem_summarise_num(trust, age, group = eu_index))
})

# value labels ------------------------------------------------------------

test_that("haven value labels are applied to item and group", {
  skip_if_not_installed("haven")
  df <- data.frame(
    q = haven::labelled(c(1, 2, 2, 1, NA, 2), c(Yes = 1, No = 2)),
    g = haven::labelled(c(1, 1, 2, 2, 1, 2), c(Men = 1, Women = 2))
  )

  out <- stem_summarise(df, q, group = g, min_cell_n = NULL)
  expect_true("freq" %in% names(out))
  expect_equal(levels(out$q), c("Yes", "No"))
  expect_equal(levels(out$g), c("Men", "Women"))

  out <- stem_summarise_cat(df, q, collapse_item = list(Any = c("Yes", "No")),
                            min_cell_n = NULL)
  expect_equal(as.character(out$q), "Any")

  out <- stem_summarise_num(df, q, group = g, min_cell_n = NULL)
  expect_equal(out$mean, c(1.5, 5 / 3))
})
