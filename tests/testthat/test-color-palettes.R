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
  gen <- stem_palette_gen("modern") # 5 colours, extended to 7
  expect_error(gen(99), "Not enough colours")
  expect_error(gen(8), "needs 8, has 7).*collapse_item")
  expect_error(stem_palette_gen("seq1")(7), "needs 7, has 6).*collapse_item")
})

test_that("diverging selections for 2-5 categories are unchanged", {
  expect_identical(stem_palette_gen("div1")(2), c("#4DA8A0", "#FC684D"))
  expect_identical(stem_palette_gen("div1")(3), c("#4DA8A0", "#D1C9BC", "#FC684D"))
  expect_identical(stem_palette_gen("div1")(4), c("#4DA8A0", "#8CD2CC", "#FDA592", "#FC684D"))
  for (pal in c("modern", "div1", "div2", "div3")) {
    for (n in 2:5) {
      expect_identical(
        stem_palette_gen(pal)(n),
        select_diverging(as.character(stem_palette(pal)), n)
      )
    }
  }
})

test_that("every diverging palette has 6- and 7-colour versions", {
  for (pal in c("modern", "div1", "div2", "div3")) {
    base <- as.character(stem_palette(pal))
    seven <- stem_palette_gen(pal)(7)
    six <- stem_palette_gen(pal)(6)
    expect_length(unique(seven), 7)
    expect_length(unique(six), 6)
    # The 5-colour palette sits inside the 7-colour one; 6 drops the midpoint.
    expect_identical(seven[2:6], base)
    expect_identical(six, seven[-4])
    expect_true(all(grepl("^#[0-9A-F]{6}$", seven)))
    expect_identical(stem_palette_gen(pal, direction = -1)(7), rev(seven))
  }
  # stem_palette() itself still returns the 5 base colours.
  expect_length(stem_palette("div1"), 5)
})

test_that("6- and 7-point scales can be stacked and exported", {
  skip_if_not_installed("mschart")
  lv <- paste("Level", 1:7)
  df <- data.frame(q = factor(rep(lv, 10), levels = lv), g = rep(c("a", "b"), 35))
  for (n in 6:7) {
    d <- droplevels(df[df$q %in% lv[seq_len(n)], ])
    p <- stem_barplot(d, q, group = g)
    expect_no_error(ggplot2::ggplot_build(p))
    chart <- stem_as_mschart(p)
    expect_equal(unname(unlist(chart$series_settings$fill)), stem_palette_gen("div1")(n))
  }
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
