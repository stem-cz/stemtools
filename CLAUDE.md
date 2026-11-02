# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

`stemtools` is an R package of color palettes, plotting functions, and survey aggregation helpers for analysts at Stem (https://www.stem.cz).

## Commands

```r
pkgload::load_all()                 # load package for interactive dev
roxygen2::roxygenise()              # regenerate NAMESPACE + man/ after changing roxygen or reexports
testthat::test_local()              # run the full test suite
testthat::test_file("tests/testthat/test-aggregation.R")   # run one test file
```

```sh
R CMD build . && R CMD check *.tar.gz   # full package check (mirrors CI in .github/workflows/R-CMD-check.yaml)
```

Roxygen version is pinned (`Config/roxygen2/version` in DESCRIPTION); NAMESPACE and `man/*.Rd` are generated — never edit them by hand.

## Architecture

**Aggregation is the engine; plotting sits on top of it.** `R/aggregation.R` defines `stem_summarise_cat()` / `stem_summarise_num()` (dispatched by `stem_summarise()`), the single source of survey estimates. Each has two code paths:
- **weighted** — builds a `srvyr::as_survey_design()` design, drops NAs with `filter()` on the design (domain estimation), and computes `survey_prop()`/`survey_mean()` with Taylor-series standard errors.
- **unweighted** — falls back to `dplyr::count()` with the parametric SEs from `R/utils.R` (design effect 1).

Both paths produce only the estimate plus internal `.se`/`.deff` columns; `add_uncertainty()` turns those into whatever the `variance` argument requests (`<est>_se`, `_var`, `_cv`, `_low`/`_upp`, `_moe`, `_deff`). Shared helpers in the same file: `label_values()` converts `haven_labelled` columns to factors (haven is only in Suggests; `labelled_cols()` detects labelled columns by class and calls `rlang::check_installed("haven")` only when one exists, so keep every `haven::` call behind it), and `warn_small_cells()` raises the `stemtools_warning_small_cell` warning (plots pass `min_cell_n = NULL` to suppress it).

The plotting functions in `R/plots.R` (`stem_barplot`, `stem_battery`, `stem_multiselect`, `stem_inline`) never compute statistics themselves — they call the internal `stem_plot_data()`, which wraps `stem_summarise_cat()` and adds a preformatted `stem_label` column. So estimation logic lives in exactly one place; changes to how frequencies/CIs are computed belong in `R/aggregation.R`, not the plot functions. Likewise `stem_boxplot()` gets its weighted quartiles, whiskers and mean from `stem_box_stats()` / `weighted_quantile()` in `R/aggregation.R`.

Stacked plots go through `stem_stack()`, which builds an explicit level → colour map (`stem_fill_values()`: palette over the factor's *full* set of levels, greys for `dk_levels`) and per-row `.label_color` chosen by fill contrast (`contrast_label_color()`). Plots are sized for 19 cm exports at the 13 pt `theme_stem()`: titles wrap at 73 characters, y-axis and legend labels are wrapped by the scales' `labels` functions (`wrap_labels()`), never in the data, so native charts get no line breaks.

**Native Office charts.** `R/officer.R` turns the bar plots into mschart charts by reading the ggplot back (`stem_chart_spec()`): `plot$data` with `freq`, `y`/`fill` in the top-level `aes()`, a `GeomCol` layer, colours from the built fill scale, `stem_label`, `.label_color` and the title. The fill layer's position decides the chart type: none → `simple`, stack → `stacked`, dodge → `dodged`, identity → `overlap` (the grey "don't know" series of an ungrouped `stem_barplot()`). Any change to a bar plot must keep that contract; tests convert every bar plot with `stem_as_mschart()`. Boxplots are refused (no Office equivalent).

**Column-name contract.** The whole pipeline communicates through fixed column names (`freq`, `freq_low`, `freq_upp`, `n`, `group_n`, `item_n`, `n_weighted`, `stem_label`, plus long-format `item`/`item_cat`/`group`/`group_cat`). These flow from aggregation → `stem_plot_data` → ggplot aesthetics. Renaming any of them requires updating all three layers.

**Palettes.** `R/color-palettes.R` defines an S7 class `stem_colors` and a single registry list `.stem_palettes` (the source of truth, tagged nominal/diverging/sequential). `stem_palette()`, `stem_palette_gen()`, `scale_*_stem()`, and `stem_palettes_all()` all derive from it — add a palette by appending one `stem_colors()` entry to that list. Diverging palettes carry 6-/7-colour versions in `extended`; renamed palettes go in `.stem_palettes_deprecated` (resolved with a warning by `resolve_palette()`).

**Non-standard evaluation.** Functions take bare column names via `{{ }}` / rlang quosures. Every data-variable symbol referenced through NSE must also be registered in the `utils::globalVariables()` call in `R/global.R`, or `R CMD check` flags it as an undefined global. Keep that list in sync when adding NSE columns.

**Re-exports.** `R/reexports.R` re-exports selected functions from other packages (e.g. `spreadview::get_categorical_vars`, `spreadview::compose_spreadsheet`) using the `#' @importFrom` + `#' @export pkg::fun` pattern, so `stemtools` users get them without loading the source package. `spreadview` is GitHub-only, hence the `Remotes:` field in DESCRIPTION.

**Test data.** `data/trust.rda` (documented in `R/data.R`) is a simulated 1000×12 survey dataset used throughout examples and tests.
