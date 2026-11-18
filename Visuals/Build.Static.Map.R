# =====================================================================
# Master thesis: weekly temperature and hospitalisations in Austria
# Build the self-contained static map (Output/map.html)
# =====================================================================
# Run once from the project root:
#   Rscript Build.Static.Map.R
#
# The resulting HTML contains everything it needs (plotly.js, the
# municipality boundaries and all precomputed values), so the year, week and
# indicator controls work without an R server. Data/map_data.rds is only read.
# =====================================================================

library(data.table)
library(jsonlite)
library(here)

root <- here()
map_file  <- file.path(root, "Data", "map_data.rds")
geo_file  <- file.path(root, "Data", "gemeinden_999_geo.json")
template_file <- file.path(root, "Visuals", "Static", "template.html")
app_file  <- file.path(root, "Visuals", "Static", "app.js")
out_file  <- file.path(root, "Output", "map.html")

stopifnot(file.exists(map_file), file.exists(geo_file),
          file.exists(template_file), file.exists(app_file))

read_text <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

# ---------------------------------------------------------------------
# 1. Timeline and the complete municipality x week table
# ---------------------------------------------------------------------
map_dt <- as.data.table(readRDS(map_file))
setorder(map_dt, id, year, week)

ids <- sort(unique(map_dt$id))
timeline <- unique(map_dt[, .(year, week)])
setorder(timeline, year, week)

n_muni <- length(ids)
n_time <- nrow(timeline)

stopifnot(
  nrow(map_dt) == n_muni * n_time,
  !anyDuplicated(map_dt[, .(id, year, week)])
)

ri <- match(map_dt$id, ids)
ti <- match(
  paste(map_dt$year, map_dt$week),
  paste(timeline$year, timeline$week)
)
stopifnot(!anyNA(ri), !anyNA(ti))

# ---------------------------------------------------------------------
# 2. Encode the values as integers
# ---------------------------------------------------------------------
# Rate as rate * 1e5, temperature as degrees C * 10; -999 marks a gap.
rate <- matrix(-999L, nrow = n_muni, ncol = n_time)
temp <- matrix(-999L, nrow = n_muni, ncol = n_time)

rate[cbind(ri, ti)] <- ifelse(
  is.na(map_dt$rate_std), -999L, as.integer(round(map_dt$rate_std * 1e5))
)
temp[cbind(ri, ti)] <- ifelse(
  is.na(map_dt$t2m_mean), -999L, as.integer(round(map_dt$t2m_mean * 10))
)

# Flat arrays ordered by municipality, then week: value(i, t) = i * n_time + t
rate_flat <- as.integer(t(rate))
temp_flat <- as.integer(t(temp))

stopifnot(
  length(rate_flat) == n_muni * n_time,
  length(temp_flat) == n_muni * n_time
)

# Spot check: Wien-Innere Stadt, 2018, week 1.
i <- match("90101", ids)
t <- match("2018 1", paste(timeline$year, timeline$week))
row <- map_dt[id == "90101" & year == 2018 & week == 1]
stopifnot(
  rate_flat[(i - 1L) * n_time + t] == as.integer(round(row$rate_std * 1e5)),
  temp_flat[(i - 1L) * n_time + t] == as.integer(round(row$t2m_mean * 10))
)

# ---------------------------------------------------------------------
# 3. Colour limits and map bounds (same rules as the Shiny app)
# ---------------------------------------------------------------------
rate_zmax <- as.numeric(quantile(map_dt$rate_std, 0.99, na.rm = TRUE))

geojson_text <- read_text(geo_file)
geojson <- fromJSON(geojson_text, simplifyVector = FALSE)

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

cfg <- toJSON(
  list(
    rate_zmax = round(rate_zmax, 8),
    temp_min  = -5,
    temp_max  = 30,
    lon_range = round(lon_range + c(-lon_pad, lon_pad), 4),
    lat_range = round(lat_range + c(-lat_pad, lat_pad), 4)
  ),
  auto_unbox = TRUE,
  digits = 8
)

# ---------------------------------------------------------------------
# 4. Assemble the HTML
# ---------------------------------------------------------------------
names_by_id <- unique(map_dt[, .(id, name)])
names_by_id <- names_by_id[match(ids, id)]

data_js <- paste0(
  "const IDS = ", toJSON(ids), ";\n",
  "const NAMES = ", toJSON(names_by_id$name), ";\n",
  "const TIMELINE = [",
  paste0("[", timeline$year, ",", timeline$week, "]", collapse = ","),
  "];\n",
  "const RATE = [", paste(rate_flat, collapse = ","), "];\n",
  "const TEMP = [", paste(temp_flat, collapse = ","), "];\n",
  "const CFG = ", cfg, ";"
)

plotly_js <- read_text(
  system.file("htmlwidgets/lib/plotlyjs/plotly-latest.min.js", package = "plotly")
)
app_js <- read_text(app_file)

# Embedded text must not be able to close the surrounding script element.
for (part in list(plotly_js, geojson_text, data_js, app_js)) {
  if (grepl("</script", part, fixed = TRUE)) {
    stop("Embedded content contains '</script' and cannot be inlined.",
         call. = FALSE)
  }
}

html <- read_text(template_file)
inject <- function(x, marker, content) {
  parts <- strsplit(x, marker, fixed = TRUE)[[1]]
  if (length(parts) != 2L) {
    stop("Marker not found exactly once: ", marker, call. = FALSE)
  }
  paste0(parts[1], content, parts[2])
}

html <- inject(html, "/*__PLOTLY_JS__*/", plotly_js)
html <- inject(html, "/*__GEO_JSON__*/", paste0("const GEO = ", geojson_text, ";"))
html <- inject(html, "/*__DATA_JSON__*/", data_js)
html <- inject(html, "/*__APP_JS__*/", app_js)

dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
con <- file(out_file, open = "wb")
writeBin(charToRaw(enc2utf8(html)), con)
close(con)

# ---------------------------------------------------------------------
# 5. Report
# ---------------------------------------------------------------------
cat("Municipalities:", n_muni, "\n")
cat("Timeline points:", n_time, "(",
    min(timeline$year), "-", max(timeline$year), ")\n")
cat("Rate z-max (99th pct, standardised):", sprintf("%.4f", rate_zmax), "\n")
cat("Gaps: rate", sum(rate_flat == -999L),
    "/ temp", sum(temp_flat == -999L), "\n")
cat("Saved:", out_file,
    sprintf("(%.1f MB)\n", file.info(out_file)$size / 1024^2))
