# stem_save -----------------------------------------------------------------

test_that("stem_save writes PNG, SVG and PDF at 19 cm", {
  p <- stem_barplot(trust, police, group = eu_index) + theme_stem(family = "")
  dir <- withr::local_tempdir()

  png_path <- file.path(dir, "chart.png")
  expect_identical(stem_save(p, png_path, dpi = 100), png_path)
  expect_gt(file.size(png_path), 5000)
  # 19 cm at 100 dpi is 748 px wide: read the width from the PNG header.
  header <- readBin(png_path, "raw", 24)
  expect_equal(sum(as.integer(header[17:20]) * 256^(3:0)), round(19 / 2.54 * 100))

  for (ext in c("svg", "pdf")) {
    path <- file.path(dir, paste0("chart.", ext))
    stem_save(p, path)
    expect_gt(file.size(path), 1000)
  }
  expect_error(stem_save(p, file.path(dir, "chart.jpg")), "must end in .png, .svg or .pdf")
  expect_error(stem_save("not a plot", file.path(dir, "x.png")), "ggplot2 object")
})

test_that("stem_save numbers a list of plots", {
  dir <- withr::local_tempdir()
  plots <- stem_battery(trust, items = c(police, eu, government, army),
                        items_per_chart = 2)
  paths <- stem_save(lapply(plots, `+`, theme_stem(family = "")),
                     file.path(dir, "battery.png"), dpi = 72)
  expect_equal(basename(paths), c("battery_1.png", "battery_2.png"))
  expect_true(all(file.exists(paths)))
})

test_that("stem_save renders Calibri with the Cairo device", {
  skip_on_cran()
  skip_if_not(isTRUE(capabilities("cairo")))
  skip_on_os(c("mac", "linux", "solaris")) # Calibri ships with Windows
  p <- stem_barplot(trust, police, group = eu_index, title_show = TRUE) + theme_stem()
  path <- withr::local_tempfile(fileext = ".png")
  stem_save(p, path, dpi = 100)
  # A blank 19 x 10 cm image compresses to about 1 kB; this chart to over 10 kB.
  expect_gt(file.size(path), 8000)
})
