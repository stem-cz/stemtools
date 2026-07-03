# stem_palette -----------------------------------------------------------

test_that("stem_palette returns a character vector carrying a type attribute", {
  pal <- stem_palette("modern")
  expect_type(pal, "character")
  expect_identical(attr(pal, "type"), "diverging")
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", pal)))
})

test_that("stem_palette knows every registered palette", {
  for (name in names(.stem_palettes)) {
    pal <- stem_palette(name)
    expect_gt(length(pal), 0)
    expect_true(attr(pal, "type") %in% c("nominal", "diverging", "sequential"))
  }
})

test_that("stem_palette errors on an unknown palette", {
  expect_error(stem_palette("not-a-palette"), "Unknown palette")
})

# select_diverging -------------------------------------------------------

test_that("select_diverging keeps the palette midpoint for odd n", {
  pal <- stem_palette("modern") # 5 colours
  expect_identical(select_diverging(pal, 3)[2], pal[3])
})

test_that("select_diverging draws from both ends for even n", {
  pal <- stem_palette("modern")
  out <- select_diverging(pal, 2)
  expect_identical(out, pal[c(1, 5)])
})

test_that("select_diverging returns the full palette when n equals its length", {
  pal <- stem_palette("modern")
  expect_identical(select_diverging(pal, length(pal)), pal)
})

test_that("select_diverging preserves original palette order", {
  pal <- stem_palette("div1")
  out <- select_diverging(pal, 3)
  expect_identical(out, out[order(match(out, pal))])
})

test_that("select_diverging rejects out-of-range n", {
  pal <- stem_palette("modern")
  expect_error(select_diverging(pal, 0))
  expect_error(select_diverging(pal, length(pal) + 1))
})

# stem_palette_gen -------------------------------------------------------

test_that("stem_palette_gen returns a generator that yields n colours", {
  gen <- stem_palette_gen("modern")
  expect_type(gen, "closure")
  expect_length(gen(3), 3)
})

test_that("stem_palette_gen direction = -1 reverses the colours", {
  gen_fwd <- stem_palette_gen("nom1", direction = 1)
  gen_rev <- stem_palette_gen("nom1", direction = -1)
  expect_identical(gen_rev(3), rev(gen_fwd(3)))
})

test_that("stem_palette_gen errors when asked for too many colours", {
  gen <- stem_palette_gen("modern") # 5 colours
  expect_error(gen(99), "Not enough colours")
})

# ggplot2 scales ---------------------------------------------------------

test_that("scale_*_stem build ggplot2 Scale objects", {
  expect_s3_class(scale_colour_stem(), "Scale")
  expect_s3_class(scale_fill_stem(), "Scale")
  expect_s3_class(scale_color_stem(), "Scale")
})

test_that("scale_color_stem is an alias for scale_colour_stem", {
  expect_identical(scale_color_stem, scale_colour_stem)
})

test_that("scale_colour_stem maps to the colour aesthetic and fill to fill", {
  expect_identical(scale_colour_stem()$aesthetics, "colour")
  expect_identical(scale_fill_stem()$aesthetics, "fill")
})

# overview ---------------------------------------------------------------

test_that("stem_palettes_all returns a ggplot", {
  expect_s3_class(stem_palettes_all(), "ggplot")
})
