# pxfetch

<!-- badges: start -->
[![R-CMD-check](https://github.com/emilmalta/pxfetch/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/emilmalta/pxfetch/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**pxfetch** is a general-purpose R client for [PXWeb](https://www.scb.se/en/services/statistical-programs-for-px-files/px-web/) statistical APIs. It supports both v1 (POST/JSON) and v2 (GET/query parameters), with API version detected automatically from the URL. It works with any statistics office running PXWeb — not tied to any single country or organisation.

Companion packages:

- [**pxmake**](https://github.com/StatisticsGreenland/pxmake) — creates and modifies PX files (on CRAN)
- [**statgl**](https://github.com/StatisticsGreenland/statgl) — Greenland-specific defaults, ggplot2 themes, and report tooling; wraps pxfetch

## Installation

```r
# install.packages("pak")
pak::pak("emilmalta/pxfetch")
```

## Usage

Set your API URL once, for example in `.Rprofile`:

```r
options(px.api_url = "https://bank.stat.gl/api/v1/en/Greenland/")
```

### Fetch a table

`px_fetch()` returns a tibble with human-readable labels by default, matching what you see in the browser.

```r
library(pxfetch)

px_fetch("BEXSTA")
```

### Select values

Pass named arguments using the variable codes from `px_meta()`. DSL helpers `px_top()`, `px_all()`, and `px_agg()` are available for common selection patterns.

```r
# Five most recent periods, all residence types
px_fetch("BEXSTA",
  `residence type` = px_all(),
  time             = px_top(5)
)

# Specific values
px_fetch("BEXSTA",
  `residence type` = c("A", "B"),
  time             = c("2020", "2021", "2022")
)
```

### Keep codes instead of labels

Useful for joining, pivoting, or automated downstream processing.

```r
px_fetch("BEXSTA", .codes = "both")
```

Or selectively per variable:

```r
px_fetch("BEXSTA", .codes = c(time = "both"))
```

### Explore table metadata

```r
px_meta("BEXSTA")
#> # A tibble: 42 × 6
#>    variable        label           eliminable is_time value value_label
#>    <chr>           <chr>           <lgl>      <lgl>   <chr> <chr>
#>  1 residence type  Residence type  TRUE       FALSE   A     Capital city
#>  ...
```

### Debug a query before sending

```r
px_fetch("BEXSTA", time = px_top(3), .dry_run = TRUE)
```

### Tag a tibble from any source

If your data wasn't fetched via `px_fetch()` — for example, read from a CSV — you can attach the metadata needed for label joining (coming in 0.3.0) with `px_tag()`.

```r
readr::read_csv("BEXSTA.csv") |>
  px_tag("BEXSTA")
```

## v1 and v2 APIs

pxfetch detects the API version from the URL and dispatches accordingly — no extra argument needed.

```r
# v1: POST with JSON body
options(px.api_url = "https://bank.stat.gl/api/v1/en/Greenland/")
px_fetch("BEXSTA")

# v2: GET with query parameters
options(px.api_url = "https://data.ssb.no/api/pxwebapi/v2/")
px_fetch("05279")
```

## Design

- `httr2` throughout, never `httr`
- API version inferred from URL, never a separate argument
- `rlang::abort()` with classed conditions for all errors
- Full test suite via `httptest2` with committed fixtures — CI runs offline

## Roadmap

- **0.3.0** — `px_label()` for joining human-readable labels onto any tagged tibble, with tidyselect column targeting; time-format helpers (`px_year()`, `px_year_month()`, etc.)
- **0.4.0** — `px_search()` for browsing table catalogues
- **CRAN** — targeted at 0.5.0

## Related work

- [**pxweb**](https://github.com/ropengov/pxweb) — the established PXWeb client from rOpenGov. `httr`-based, v1 only, returns a more complex list structure. A good choice for v1-only APIs with complex query building needs.
- [**PxWebApiData**](https://github.com/statisticsnorway/PxWebApiData) — Statistics Norway's client, also general-purpose and supports v2. `httr`-based. 

pxfetch uses `httr2`, returns a flat tidy tibble directly, and provides a small DSL (`px_top()`, `px_all()`, `px_agg()`) for common selection patterns. API version is detected from the URL with no extra argument needed. Label and code display can be controlled per-variable via `.codes`, and `px_label()` (coming in 0.3.0) will allow joining labels onto any tagged tibble in one step — including data not originally fetched through pxfetch.
