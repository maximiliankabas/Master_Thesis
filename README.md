# Weekly hospitalisations and temperature in Austria

Interactive visualisations for a master's thesis on the association between
weekly temperature and hospitalisations at the municipality level in Austria,
2009-2018.

## Live links

- **Interactive map (Shiny app):** <!-- SHINY_URL --> _coming soon_
- **Interactive 3D plot (standalone HTML):**
  https://USERNAME.github.io/REPO/Output/3D_n_pop_age_week.html

## What is in this repository

| Path | Purpose |
| --- | --- |
| `ui.R`, `server.R` | The two-file Shiny app (municipality map) |
| `Prepare.Map.Data.R` | Builds the compact `Data/map_data.rds` used by the app |
| `Shiny.Apps.R` | Builds the standalone 3D surface plot |
| `Data/map_data.rds` | Precomputed map table (municipality x year x week) |
| `Data/gemeinden_999_geo.json` | Municipality boundaries (2,117 units) |
| `Output/3D_n_pop_age_week.html` | Self-contained 3D plot, no R required |

## The map

- Year and ISO-week selectors, 2009-2018.
- Indicator switch between the crude hospitalisation rate and the mean weekly
  temperature (`week_t2m_mean`).
- Colour scale: blue = low, white = middle, red = high, fixed across weeks.
- The rate is shown as a raw proportion (`n / pop`, about 0.0000-0.0123), not
  as a percentage.
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
