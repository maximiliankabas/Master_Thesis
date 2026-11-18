# =====================================================================
# Master thesis: weekly temperature and hospitalisations in Austria
# Interactive 3D plot of the weekly hospitalisation rate by age and week
# =====================================================================
# Inputs
#   Data/merge.RData        hospital discharges
#                           (municipality x ISO week x age x sex)
#   Data/popdat_aut.RData   municipality x year x age x sex population
#
# Output
#   Output/3D_n_pop_age_week.html
#
# Notes
#   * z = n / pop, x = age, y = week (week_f).
#   * Age 1 has no population denominator in the source data and is excluded.
#   * merge stores `pop` only on rows where discharges occur. Summing that
#     column would therefore understate the population. The complete counts
#     in popdat_aut are used as the denominator instead.
#   * Data/merge.RData is only read, never modified.
#   * The interactive municipality map is in ui.R and server.R. Run it with
#     shiny::runApp(".") from the project root.
# =====================================================================

rm(list = ls())

library(data.table)
library(plotly)
library(here)
library(htmlwidgets)

# ---------------------------------------------------------------------
# 1. Load the data (read-only)
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
# 2. Build the age x week rate surface
# ---------------------------------------------------------------------
# Population is a yearly figure. Aggregate the complete municipality x age x
# sex counts to age x year (both sexes together).
pop[, year := as.integer(as.character(Year))]
pop <- pop[year %in% unique(dt$year) & age <= max(dt$age)]
pop_age_year <- pop[, .(pop = sum(pop)), by = .(age, year)]

# Hospitalisations by age x week x year. Age 1 has no denominator.
# week_f is the factor version of week; the surface uses its integer levels
# (1-53) so that plotly can draw a continuous y axis.
n_age_week_year <- dt[age != 1, .(n = sum(n)), by = .(age, week, year)]

# Pool the years: z = sum(n) / sum(pop) for every age-week cell.
rate_age_week <- merge(
  n_age_week_year, pop_age_year,
  by = c("age", "year"), all.x = TRUE
)[, .(n = sum(n), pop = sum(pop)), by = .(age, week)]
rate_age_week[, rate := n / pop]

stopifnot(!anyNA(rate_age_week$rate))

ages  <- sort(unique(rate_age_week$age))
weeks <- sort(unique(rate_age_week$week))

# Plotly surface expects a matrix with one row per y value (week) and one
# column per x value (age).
z <- matrix(
  NA_real_,
  nrow = length(weeks), ncol = length(ages),
  dimnames = list(week = weeks, age = ages)
)
z[cbind(match(rate_age_week$week, weeks),
        match(rate_age_week$age, ages))] <- rate_age_week$rate

stopifnot(!anyNA(z))

# ---------------------------------------------------------------------
# 3. Build and save the interactive 3D plot
# ---------------------------------------------------------------------
p <- plot_ly(
  x = ages,
  y = weeks,
  z = z,
  type = "surface",
  colorscale = list(
    c(0.0, "#2166ac"),
    c(0.5, "#f7f7f7"),
    c(1.0, "#b2182b")
  ),
  cmin = 0,
  cmax = max(z, na.rm = TRUE),
  colorbar = list(
    title = "n / pop",
    tickformat = ".4f",
    len = 0.65,
    thickness = 16
  ),
  hovertemplate = paste0(
    "Age: %{x} years<br>",
    "Week: %{y}<br>",
    "n / pop: %{z:.4f}<extra></extra>"
  ),
  contours = list(
    z = list(
      show = TRUE,
      usecolormap = TRUE,
      highlightcolor = "#ffffff"
    )
  ),
  lighting = list(
    ambient = 0.65,
    diffuse = 0.80,
    specular = 0.15,
    roughness = 0.90
  ),
  lightposition = list(x = 100, y = 200, z = 100)
) |>
  layout(
    title = list(
      text = paste0(
        "<b>Weekly hospitalisation rate by age and week</b><br>",
        "<sup>Austria, 2009-2018. z = n / pop; age 1 excluded ",
        "(no population denominator).</sup>"
      ),
      x = 0.02,
      xanchor = "left"
    ),
    scene = list(
      xaxis = list(
        title = "Age",
        tickmode = "array",
        tickvals = ages[ages %% 10 == 0],
        ticktext = ages[ages %% 10 == 0]
      ),
      yaxis = list(
        title = "Week of year",
        dtick = 4
      ),
      zaxis = list(
        title = "n / pop",
        tickformat = ".4f"
      ),
      camera = list(
        eye = list(x = 1.75, y = -1.85, z = 1.08)
      ),
      aspectratio = list(x = 1.15, y = 1.25, z = 0.80)
    ),
    margin = list(l = 0, r = 0, t = 80, b = 0),
    paper_bgcolor = "#ffffff"
  )

dir.create(here("Output"), recursive = TRUE, showWarnings = FALSE)
out_file <- here("Output", "3D_n_pop_age_week.html")
saveWidget(p, out_file, selfcontained = TRUE)

# saveWidget can leave a redundant dependency folder next to a self-contained
# HTML. Remove it so that Output only contains the single portable HTML file.
lib_dir <- sub("\\.html$", "_files", out_file)
if (dir.exists(lib_dir)) unlink(lib_dir, recursive = TRUE)

message("3D plot written to: ", out_file)
message("Ages: ", paste(ages, collapse = ", "))
message("Weeks: ", min(weeks), "-", max(weeks))
