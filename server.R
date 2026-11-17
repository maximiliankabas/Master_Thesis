# =====================================================================
# Master thesis: weekly temperature and hospitalisations in Austria
# Server for the interactive municipality map
# =====================================================================
# Run from the project root with:
#   shiny::runApp(".")
#
# The app loads Data/map_data.rds, which is built once by
# Prepare.Map.Data.R. This keeps startup fast and memory small; the large
# Data/merge.RData is not needed at runtime.
# =====================================================================

library(shiny)
library(plotly)
library(data.table)

# ---------------------------------------------------------------------
# 1. Load the precomputed map data
# ---------------------------------------------------------------------
map_file <- file.path("Data", "map_data.rds")
if (!file.exists(map_file)) {
  stop(
    "Data/map_data.rds not found. Run source('Prepare.Map.Data.R') first.",
    call. = FALSE
  )
}
map_dt <- as.data.table(readRDS(map_file))

# ---------------------------------------------------------------------
# 2. Geojson
# ---------------------------------------------------------------------
geojson <- jsonlite::fromJSON(
  file.path("Data", "gemeinden_999_geo.json"),
  simplifyVector = FALSE
)
geo_props <- rbindlist(lapply(geojson$features, function(f) {
  as.data.table(f$properties)
}))
geo_props <- geo_props[, .(name, iso)]

# The file follows RFC 7946 (counter-clockwise exterior rings), but the map
# engine used by plotly (d3-geo) expects clockwise exterior rings and
# counter-clockwise holes. With the RFC winding, some municipalities fill the
# whole map frame instead of their own polygon. Normalise the ring direction.
ring_signed_area <- function(ring) {
  m <- matrix(unlist(ring), ncol = length(ring[[1]]), byrow = TRUE)
  x <- m[, 1]
  y <- m[, 2]
  sum(x * c(y[-1], y[1]) - c(x[-1], x[1]) * y) / 2
}

rewind_ring <- function(ring, exterior) {
  area <- ring_signed_area(ring)
  wrong <- if (exterior) area > 0 else area < 0
  if (isTRUE(wrong)) rev(ring) else ring
}

rewind_polygon <- function(poly) {
  lapply(seq_along(poly), function(i) {
    rewind_ring(poly[[i]], exterior = i == 1L)
  })
}

geojson$features <- lapply(geojson$features, function(f) {
  co <- f$geometry$coordinates
  f$geometry$coordinates <- if (f$geometry$type == "Polygon") {
    rewind_polygon(co)
  } else {
    lapply(co, rewind_polygon)
  }
  f
})

# Explicit map bounds: fitbounds is not applied reliably for custom geojson in
# this plotly version, so the map would otherwise start at world scale.
geo_bbox <- function(f) {
  co <- f$geometry$coordinates
  rings <- if (f$geometry$type == "Polygon") co else unlist(co, recursive = FALSE)
  m <- do.call(rbind, lapply(rings, function(ring) {
    matrix(unlist(ring), ncol = length(ring[[1]]), byrow = TRUE)[, 1:2, drop = FALSE]
  }))
  c(min(m[, 1]), max(m[, 1]), min(m[, 2]), max(m[, 2]))
}
geo_bb <- do.call(rbind, lapply(geojson$features, geo_bbox))
lon_range <- range(geo_bb[, c(1, 2)])
lat_range <- range(geo_bb[, c(3, 4)])
lon_pad <- diff(lon_range) * 0.01
lat_pad <- diff(lat_range) * 0.01
geo_lon_range <- lon_range + c(-lon_pad, lon_pad)
geo_lat_range <- lat_range + c(-lat_pad, lat_pad)

# All 2117 municipality IDs in merge have an exact polygon in the new geojson.
# The file also contains one extra aggregate Wien polygon (90001); it is not
# used because merge has separate data for the 23 Vienna districts.
bg_ids <- intersect(unique(map_dt$id), geo_props$iso)

# ---------------------------------------------------------------------
# 3. Colour limits and year/week choices
# ---------------------------------------------------------------------
rate_zmax  <- as.numeric(quantile(map_dt$rate, 0.99, na.rm = TRUE))
temp_range <- range(-5, 30)

year_week <- unique(map_dt[, .(year, week)])
weeks_by_year <- split(year_week$week, year_week$year)
weeks_by_year <- lapply(weeks_by_year, sort)

# ---------------------------------------------------------------------
# 4. Server
# ---------------------------------------------------------------------
server <- function(input, output, session) {

  # Only offer weeks that exist in the selected year: week 53 is not present
  # in every year and some years start in week 2.
  observeEvent(input$year, {
    w <- weeks_by_year[[as.character(input$year)]]
    selected <- suppressWarnings(as.integer(input$week))
    if (!length(selected) || is.na(selected) || !selected %in% w) {
      selected <- w[1]
    }
    updateSelectInput(session, "week", choices = w, selected = selected)
  })

  output$map <- renderPlotly({
    req(input$year, input$week, input$metric)

    d <- map_dt[
      year == as.integer(input$year) & week == as.integer(input$week)
    ]

    if (input$metric == "rate") {
      z         <- d$rate
      zmin      <- 0
      zmax      <- rate_zmax
      cb_title  <- "Rate"
      cb_format <- ".4f"
      hover <- paste0(
        "<b>%{customdata}</b><br>",
        "Year: ", input$year, "<br>",
        "Week: ", input$week, "<br>",
        "Rate: %{z:.4f}<extra></extra>"
      )
      subtitle <- "Weekly Crude Hospitalisation Rate"
    } else {
      z         <- d$t2m_mean
      zmin      <- temp_range[1]
      zmax      <- temp_range[2]
      cb_title  <- "degrees C"
      cb_format <- ".3f"
      hover <- paste0(
        "<b>%{customdata}</b><br>",
        "Year: ", input$year, "<br>",
        "Week: ", input$week, "<br>",
        "t2m_mean: %{z:.1f} &deg;C<extra></extra>"
      )
      subtitle <- "Mean weekly temperature (week_t2m_mean)"
    }

    validate(need(any(!is.na(z)),
                  "No data available for this year and week."))

    plot_ly() |>
      # Light-grey background for municipalities without data.
      add_trace(
        type = "choropleth",
        geojson = geojson,
        locations = bg_ids,
        z = rep(0, length(bg_ids)),
        featureidkey = "properties.iso",
        colorscale = list(c(0, "#e9e9e9"), c(1, "#e9e9e9")),
        showscale = FALSE,
        hoverinfo = "skip",
        marker = list(line = list(color = "#ffffff", width = 0.3))
      ) |>
      add_trace(
        type = "choropleth",
        geojson = geojson,
        locations = d$id,
        z = z,
        customdata = d$name,
        featureidkey = "properties.iso",
        colorscale = list(
          c(0.0, "#2166ac"),
          c(0.5, "#f7f7f7"),
          c(1.0, "#b2182b")
        ),
        zmin = zmin,
        zmax = zmax,
        colorbar = list(
          title = cb_title,
          tickformat = cb_format,
          len = 0.65,
          thickness = 16
        ),
        marker = list(line = list(color = "#ffffff", width = 0.3)),
        hovertemplate = hover
      ) |>
      layout(
        title = list(
          text = paste0(
            "<b>Austria, ", input$year, ", week ", input$week, "</b><br>",
            "<sup>", subtitle, "</sup>"
          ),
          x = 0.02,
          xanchor = "left"
        ),
        geo = list(
          visible = FALSE,
          resolution = 50,
          projection = list(type = "mercator"),
          lonaxis = list(range = geo_lon_range),
          lataxis = list(range = geo_lat_range),
          bgcolor = "#ffffff"
        ),
        margin = list(l = 0, r = 0, t = 55, b = 0),
        paper_bgcolor = "#ffffff",
        showlegend = FALSE
      )
  })
}

