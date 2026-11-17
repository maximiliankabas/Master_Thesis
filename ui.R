# =====================================================================
# Master thesis: weekly temperature and hospitalisations in Austria
# UI for the interactive municipality map
# =====================================================================
# Run from the project root with:
#   shiny::runApp(".")
# =====================================================================

library(shiny)
library(plotly)

fluidPage(
  title = "Austria: hospitalisations and temperature",

  tags$head(
    tags$style(HTML("
      .app-subtitle {
        color: #555;
        margin-top: -6px;
        margin-bottom: 14px;
      }
      .small-note {
        color: #666;
        font-size: 12px;
        margin-top: 8px;
        line-height: 1.35;
      }
      .sidebar-note {
        color: #666;
        font-size: 12px;
        line-height: 1.35;
      }
    "))
  ),

  titlePanel(
    div(
      "Weekly hospitalisations and temperature in Austria",
      div(class = "app-subtitle",
          "Municipality-level interactive map, 2009-2018")
    ),
    windowTitle = "Austria: hospitalisations and temperature"
  ),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      selectInput(
        "year", "Year",
        choices = 2009:2018,
        selected = 2018
      ),

      selectInput(
        "week", "ISO week",
        choices = 1:53,
        selected = 1
      ),

      radioButtons(
        "metric", "Indicator",
        choices = c(
          "Crude Hospitalisation Rate" = "rate",
          "Weekly Mean Temperature"          = "temp"
        ),
        selected = "rate"
      ),

      tags$p(
        class = "sidebar-note",
        "Blue = low values, red = high values. For the rate the colour ",
        "scale is capped at the 99th percentile so that very small ",
        "municipalities with extreme rates do not dominate the map."
      )
    ),

    mainPanel(
      width = 9,
      plotlyOutput("map", height = "78vh"),
      tags$p(
        class = "small-note",
        "Grey areas have no data for the selected indicator."
      )
    )
  )
)

