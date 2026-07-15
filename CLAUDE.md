# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

`stemtools` is an R package of color palettes, plotting functions, and
survey aggregation helpers for analysts at Stem (<https://www.stem.cz>).

## Commands

``` r

pkgload::load_all()                 # load package for interactive dev
roxygen2::roxygenise()              # regenerate NAMESPACE + man/ after changing roxygen or reexports
testthat::test_local()              # run the full test suite
testthat::test_file("tests/testthat/test-aggregation.R")   # run one test file
```

``` sh
R CMD build . && R CMD check *.tar.gz   # full package check (mirrors CI in .github/workflows/R-CMD-check.yaml)
```

Roxygen version is pinned (`Config/roxygen2/version` in DESCRIPTION);
NAMESPACE and `man/*.Rd` are generated — never edit them by hand.

## Architecture

**Aggregation is the engine; plotting sits on top of it.**
`R/aggregation.R` defines
[`stem_summarise_cat()`](https://stem-cz.github.io/stemtools/reference/stem_summarise_cat.md)
/
[`stem_summarise_num()`](https://stem-cz.github.io/stemtools/reference/stem_summarise_num.md)
(dispatched by
[`stem_summarise()`](https://stem-cz.github.io/stemtools/reference/stem_summarise.md)),
the single source of survey estimates. Each has two code paths: -
**weighted** — builds a
[`surveycore::as_survey()`](https://jdenn0514.github.io/surveycore/reference/as_survey.html)
design (optionally
[`surveytidy::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)),
computes estimates with Taylor-series confidence intervals, then renames
surveycore’s `pct`/`ci_low`/`ci_high` to the package-wide
`freq`/`freq_low`/`freq_upp`. - **unweighted** — falls back to
[`dplyr::count()`](https://dplyr.tidyverse.org/reference/count.html)
with normal-approximation CIs.

The plotting functions in `R/plots.R` (`stem_barplot`, `stem_battery`,
`stem_multiselect`, `stem_inline`) never compute statistics themselves —
they call the internal
[`stem_plot_data()`](https://stem-cz.github.io/stemtools/reference/stem_plot_data.md),
which wraps
[`stem_summarise_cat()`](https://stem-cz.github.io/stemtools/reference/stem_summarise_cat.md)
and adds a preformatted `stem_label` column. So estimation logic lives
in exactly one place; changes to how frequencies/CIs are computed belong
in `R/aggregation.R`, not the plot functions.

**Column-name contract.** The whole pipeline communicates through fixed
column names (`freq`, `freq_low`, `freq_upp`, `n`, `group_n`, `item_n`,
`stem_label`, plus long-format `item`/`item_cat`/`group`/`group_cat`).
These flow from aggregation → `stem_plot_data` → ggplot aesthetics.
Renaming any of them requires updating all three layers.

**Palettes.** `R/color-palettes.R` defines an S7 class `stem_colors` and
a single registry list `.stem_palettes` (the source of truth, tagged
nominal/diverging/sequential).
[`stem_palette()`](https://stem-cz.github.io/stemtools/reference/stem_palette.md),
[`stem_palette_gen()`](https://stem-cz.github.io/stemtools/reference/stem_palette_gen.md),
`scale_*_stem()`, and
[`stem_palettes_all()`](https://stem-cz.github.io/stemtools/reference/stem_palettes_all.md)
all derive from it — add a palette by appending one
[`stem_colors()`](https://stem-cz.github.io/stemtools/reference/stem_colors.md)
entry to that list.

**Non-standard evaluation.** Functions take bare column names via
`{{ }}` / rlang quosures. Every data-variable symbol referenced through
NSE must also be registered in the
[`utils::globalVariables()`](https://rdrr.io/r/utils/globalVariables.html)
call in `R/global.R`, or `R CMD check` flags it as an undefined global.
Keep that list in sync when adding NSE columns.

**Re-exports.** `R/reexports.R` re-exports selected functions from other
packages
(e.g. [`spreadview::get_categorical_vars`](https://rdrr.io/pkg/spreadview/man/get_categorical_vars.html),
[`spreadview::compose_spreadsheet`](https://rdrr.io/pkg/spreadview/man/compose_spreadsheet.html))
using the `#' @importFrom` + `#' @export pkg::fun` pattern, so
`stemtools` users get them without loading the source package.
`spreadview` is GitHub-only, hence the `Remotes:` field in DESCRIPTION.

**Test data.** `data/trust.rda` (documented in `R/data.R`) is a
simulated 1000×12 survey dataset used throughout examples and tests.
