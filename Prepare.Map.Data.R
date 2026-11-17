# =====================================================================
# Master thesis: weekly temperature and hospitalisations in Austria
# Build the compact map data used by the Shiny app
# =====================================================================
# Run once from the project root:
#   Rscript Prepare.Map.Data.R
#
# Creates Data/map_data.rds. Data/merge.RData and Data/popdat_aut.RData
# are only read, never modified.
# =====================================================================

library(data.table)
library(here)

# ---------------------------------------------------------------------
# 1. Load the source data (read-only)
# ---------------------------------------------------------------------
data_env <- new.env(parent = emptyenv())
load(here("Data", "merge.RData"), envir = data_env)
dt <- as.data.table(data_env$merge)
rm(data_env)

pop_env <- new.env(parent = emptyenv())
load(here("Data", "popdat_aut.RData"), envir = pop_env)
pop <- as.data.table(pop_env$popdat_aut)
rm(pop_env)

# ---------------------------------------------------------------------
# 2. Aggregate to the municipality x year x week level
# ---------------------------------------------------------------------
# Complete population denominator per municipality-year. The `pop` column in
# merge is attached only to rows with discharges, so it would understate the
# population; popdat_aut contains the complete counts. Age 1 has no population
# count there and is therefore absent from the denominator.
pop[, `:=`(
  year = as.integer(as.character(Year)),
  id   = as.character(Kennziffer)
)]
pop <- pop[year %in% unique(dt$year) & age <= max(dt$age)]
pop_muni_year <- pop[, .(pop = sum(pop)), by = .(id, year)]

# Hospitalisations per municipality-week, age 1 excluded.
n_muni_week <- dt[
  age != 1,
  .(n = sum(n)),
  by = .(id, year, week)
]

# One temperature value per municipality-week (repeated across age and sex).
temp_muni_week <- dt[
  , .(t2m_mean = mean(week_t2m_mean, na.rm = TRUE)),
  by = .(id, year, week)
]

# Complete municipality x year-week grid. Municipality-weeks with no discharge
# row then become n = 0 instead of disappearing from the map.
map_ids   <- unique(dt$id)
map_names <- unique(dt[, .(id, name)])
year_week <- unique(dt[, .(year, week)])

map_dt <- data.table(id = map_ids)[
  , .(year = year_week$year, week = year_week$week),
  by = .(id)
]
map_dt <- merge(map_dt, map_names, by = "id", all.x = TRUE)

map_dt <- merge(
  map_dt, n_muni_week,
  by = c("id", "year", "week"), all.x = TRUE
)
map_dt <- merge(
  map_dt, pop_muni_year,
  by = c("id", "year"), all.x = TRUE
)
map_dt <- merge(
  map_dt, temp_muni_week,
  by = c("id", "year", "week"), all.x = TRUE
)

map_dt[is.na(n), n := 0]
map_dt[, rate := n / pop]

# Fill temperature gaps with linear interpolation inside each municipality-year.
# Only missing municipality-weeks are filled; observed values stay unchanged.
# Municipality-years without any temperature remain NA and are drawn in grey.
setorder(map_dt, id, year, week)
map_dt[, t2m_mean := {
  ok <- !is.na(t2m_mean)
  if (sum(ok) >= 2L) {
    approx(x = week[ok], y = t2m_mean[ok], xout = week, rule = 2)$y
  } else {
    t2m_mean
  }
}, by = .(id, year)]

# Only keep what the app needs. The population denominator is no longer needed
# after the rate has been computed.
map_dt <- map_dt[, .(id, name, year, week, n, rate, t2m_mean)]

stopifnot(
  nrow(map_dt) > 0L,
  !anyDuplicated(map_dt[, .(id, year, week)])
)

# ---------------------------------------------------------------------
# 3. Save the compact file
# ---------------------------------------------------------------------
out_file <- here("Data", "map_data.rds")
saveRDS(map_dt, out_file, compress = "xz")

cat("Rows:", nrow(map_dt), "\n")
cat("Municipalities:", uniqueN(map_dt$id), "\n")
cat("Years:", paste(range(map_dt$year), collapse = "-"), "\n")
cat("Saved:", out_file,
    sprintf("(%.1f MB)\n", file.info(out_file)$size / 1024^2))
