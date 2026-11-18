# Weekly hospitalisations and temperature in Austria

Interactive visualisations for a master's thesis on the association between
weekly temperature and hospitalisations at the municipality level in Austria,
2009-2018.

## Live links

- **Interactive map (standalone HTML):** `Output/map.html`
- **Interactive 3D plot (standalone HTML):** `Output/3D_n_pop_age_week.html`
- **Live links:** https://USERNAME.github.io/REPO/

## What is in this repository

| Path | Purpose |
| --- | --- |
| `ui.R`, `server.R` | The two-file Shiny app (municipality map) |
| `Prepare.Map.Data.R` | Builds the compact `Data/map_data.rds` used by the app |
| `Build.Static.Map.R` | Builds the self-contained `Output/map.html` |
| `Shiny.Apps.R` | Builds the standalone 3D surface plot |
| `Data/map_data.rds` | Precomputed map table (municipality x year x week) |
| `Data/gemeinden_999_geo.json` | Municipality boundaries (2,117 units) |
| `Output/map.html` | Self-contained interactive map, no R required |
| `Output/3D_n_pop_age_week.html` | Self-contained 3D plot, no R required |

## The map

- Year and ISO-week selectors, 2009-2018.
- Indicator switch between the age-standardised hospitalisation rate and the
  mean weekly temperature (`week_t2m_mean`).
- Colour scale: blue = low, white = middle, red = high, fixed across weeks.
- Rates are age-standardised by direct standardisation to the Austrian age
  distribution of the same year:

  `rate_std(i, y, w) = sum_a share(y, a) * n(i, y, w, a) / pop(i, y, a)`

  The weights are population shares, so they sum to 1 and the result stays on
  the same per-capita scale as the crude rate. A district with a young age
  structure is no longer penalised, which makes districts and years comparable.
- The rate is shown as a raw proportion (about 0.0000-0.0123), not a percentage.
- The rate colour scale is capped at the 99th percentile so that very small
  municipalities with extreme rates do not dominate the map.

## The 3D plot

- z = `n / pop`, x = age, y = ISO week, pooled over 2009-2018 with a
  population-weighted `sum(n) / sum(pop)` per age-week.
- Age 1 is excluded because it has no population denominator.

## Run locally

From the project root:

```r
shiny::runApp(".")
```

Required packages: `shiny`, `plotly`, `data.table`, `jsonlite`.

## Rebuild the data

Only needed if the source data change. `Data/map_data.rds` is built from
`Data/merge.RData` and `Data/popdat_aut.RData`, which are not committed:

```r
source("Prepare.Map.Data.R")
```

Rebuild the static map with:

```r
source("Build.Static.Map.R")
```

Rebuild the 3D plot with:

```r
source("Shiny.Apps.R")
```

## Deployment

The app is deployed with `rsconnect`. Only four files are uploaded:

```r
rsconnect::deployApp(
  appDir  = ".",
  appName = "austria-hospitalisations",
  appFiles = c(
    "ui.R",
    "server.R",
    "Data/map_data.rds",
    "Data/gemeinden_999_geo.json"
  )
)
```

## Data notes

- Population denominators come from `popdat_aut` (complete counts), not from
  the `pop` column in `merge`, which is only attached to rows with discharges.
- Vienna is represented by its 23 districts; the aggregate `Wien` polygon is
  excluded from the map.
- Municipality boundaries are stored as RFC 7946 GeoJSON; the ring winding is
  normalised at load time because the d3-geo renderer used by plotly expects
  the opposite orientation.
