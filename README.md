# pxfetch

<!-- badges: start -->
[![R-CMD-check](https://github.com/emilmalta/pxfetch/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/emilmalta/pxfetch/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**pxfetch** is a general-purpose R client for [PXWeb](https://www.scb.se/en/services/statistical-programs-for-px-files/px-web/) statistical APIs. It supports both API v1 (POST/JSON) and v2 (GET/query params), with version detection automatic from the URL. Not specific to any single statistics office.

Companion packages:

- [**pxmake**](https://github.com/StatisticsGreenland/pxmake) — Creates and modifies PX files. On CRAN.
- [**statgl**](https://github.com/StatisticsGreenland/statgl) — Greenland-specific defaults, ggplot2 themes, report tooling. Wraps pxfetch.

## Installation

```r
# install.packages("pak")
pak::pak("emilmalta/pxfetch")
```

## Quick start

```r
library(pxfetch)

# Point at your statistics office's API once, e.g. in .Rprofile
options(px.api_url = "https://example.stat.org/api/v1/en/")

# Fetch a table — returns labels by default, matching the web interface
px_fetch("POP001")

# Select specific values for one or more variables
px_fetch("POP001",
  gender = px_all(),
  time   = px_top(5)
)

# Fetch codes instead of labels (useful for joining, pivoting, automated plots)
px_fetch("POP001", .column_labels = FALSE, .value_labels = FALSE)

# Inspect what variables and values a table contains
px_meta("POP001")

# Add labels in multiple languages (e.g. for multilingual output)
px_fetch("POP001", .column_labels = FALSE, .value_labels = FALSE) |>
  px_label(.lang = "en", .codes = "keep") |>
  px_label(.lang = "da", .codes = "keep") |>
  px_label(.lang = "kl", .codes = "keep")

# Works with any tibble — attach table identity first with px_tag()
readr::read_csv("POP001.csv") |>
  px_tag("POP001") |>
  px_label()
```

## Design notes

- `httr2` throughout — no `httr`
- API version inferred from URL; never a separate argument
- `px_fetch()` dispatches to POST (v1) or GET (v2) automatically
- `px_fetch()` stamps `px_table_id` and `px_api_url` attributes; `px_label()` uses them
- `rlang::abort()` with classed conditions for all errors
- `cli::cli_inform()` for all user-facing messages
- Full test suite via `httptest2` with committed fixtures (CI runs offline)

## Status

Early development — not yet on CRAN. See [NEWS.md](NEWS.md) for the changelog.
