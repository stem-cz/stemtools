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
  out <- stem_summarise_cat(trust, police, group = eu_index)
  sums <- tapply(out$freq, out$eu_index, sum)
  expect_equal(as.numeric(sums), rep(1, length(sums)))
})

test_that("stem_summarise_cat confidence bounds bracket the estimate", {
  out <- stem_summarise_cat(trust, police)
  expect_true(all(out$freq_low <= out$freq))
  expect_true(all(out$freq_upp >= out$freq))
})

test_that("stem_summarise_cat return_n adds the absolute-size columns", {
  out <- stem_summarise_cat(trust, police, group = eu_index, return_n = TRUE)
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
