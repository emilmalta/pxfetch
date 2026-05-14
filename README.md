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

# Point at your statistics office's API (do this once, e.g. in .Rprofile)
options(px.api_url = "https://bank.stat.gl/api/v1/en/Greenland/")

# Fetch a table — variables not named get their eliminable default value
px_fetch("BEESTA")

# Select specific values
px_fetch("BEESTA",
  gender = px_all(),
  time   = px_top(5)
)

# Browse what variables and values a table has
px_meta("BEESTA")

# Add human-readable labels alongside codes
px_fetch("BEESTA") |>
  px_label("BEESTA", gender, time)
```

## Design notes

- `httr2` throughout — no `httr`
- API version inferred from URL; never a separate argument
- `px_fetch()` dispatches to POST (v1) or GET (v2) automatically
- `rlang::abort()` with classed conditions for all errors
- `cli::cli_inform()` for all user-facing messages
- Full test suite via `httptest2` with committed fixtures (CI runs offline)

## Status

Early development — not yet on CRAN. See [NEWS.md](NEWS.md) for the changelog and the [versioning plan](https://github.com/emilmalta/pxfetch#versioning) for the roadmap.
