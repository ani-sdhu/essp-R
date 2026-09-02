# ESSP

Energy data pipeline for the Energy Security Studies Program.

`ESSP` wraps the U.S. Energy Information Administration (EIA) API behind a small
set of plain-English commands for **gathering**, **analyzing**, and **charting**
energy data — a student can go from a question to a house-styled figure without
touching a URL, a facet id, or a raw data file.

## Install

```r
# install.packages("pak")
pak::pak("ani-sdhu/essp-R")
```

This is a **private** repository, so `pak` needs a GitHub token that can read it:

```r
Sys.setenv(GITHUB_PAT = "ghp_xxx")   # a PAT with access to ani-sdhu/essp-R
pak::pak("ani-sdhu/essp-R")
```

You also need an EIA API key (free from <https://www.eia.gov/opendata/>) in
`EIA_KEY` (or `EIA_API_KEY`):

```r
Sys.setenv(EIA_KEY = "your-eia-key")   # best set once in ~/.Renviron
```

## Use

```r
library(ESSP)

essp.catalog()                              # everything essp.get() understands
essp.get("mix", "GA", "2024")               # analysis-ready table
essp.get("generation", "CA,MD,DE", "2024")  # several states, stacked

chart.generation("Solar", "US", "2024")             # capacity vs generation
chart.demandcurve("Solar,NGCT", "CISO", "2024")     # daily demand curve
```

Every metric takes the same plain strings: a place (`"CA"`, `"CA,MD,DE"`,
`"ALL"`, `"US"`, or a grid operator like `"CISO"`) and a time (`"2024"`,
`"2023,2024"`, `"2020-2024"`, `"July 2024"`, `"summer 2024"`).

## Notes

- Charts render in **Merriweather** when the optional `showtext` and `sysfonts`
  packages are installed; otherwise they fall back to the default font.
- See `ESSP_DEMO.qmd` for a full worked walkthrough.
