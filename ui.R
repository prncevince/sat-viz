library(DT)
library(lazyeval)
library(leaflet)
library(plotly)
library(shinycssloaders)
library(shinydashboard)

# TODO: suspendwhilehidden = FALSE will ensure that everything is loaded at once - but this may sacrifice performance

# header ----
header <- dashboardHeader(
  title = "SAVi"
)
# sidebar comb 1 ----
sb_comb1 <- sidebarMenu(
  id = "comb_menu1",
  fluidRow(
    column(
      12,
      tags$style("#daterange {font-size: 16px;min-height: 30px;}"),
      dateRangeInput(
        inputId = "daterange", label = "Select Date Range",
        start = "2020-03-20", end = "2020-03-29"
      ),
      tags$head(
        tags$style(
          ".form-control {font-size: 14px;}"
        )
      )
    )
  ),
  fluidRow(
    column(
      12, align = "left", offset = 0, 
      actionButton(
        "btn_sb_daterange_comb_plot", "Update", 
        style = "color: #fff; background-color: #aa7add; border-color: #2e6da4; height:30px; font-size:16px")
    )
  )
)
# sidebar comb 2 ----
sb_comb2 <- sidebarMenu(
  id = "comb_menu2",
  fluidRow(
    column(
      12, align = "left", offset = 0,
      checkboxGroupInput(
        inputId = "selector", label = "Select Traces:",
        choices = c("PriSys", "PriSysOut", "SecSys", "LightPolys"),
        selected = c("PriSys", "PriSysOut", "SecSys", "LightPolys"),
        inline=TRUE
      ),
      tags$style("#selector {
                    font-size:16px;
      }")
    )
  ),
  fluidRow(
    column(
      12, align = "left", offset = 0, 
      actionButton(
        "btn_sb_traces_comb_plot", "Filter", 
        style = "color: #fff; background-color: #99aa37; border-color: #2e6da4; height:30px; font-size:16px")
    )
  )
)
# sidebar target ----
sb_tgt <- sidebarMenu(
  id = "tgt_menu",
  fluidRow(
    column(12,
      align = "left", offset = 1, 
      div(style = "display: inline-block;width: 25%; font-size:16px;font-weight: bold", "Target ID:"),
      div(style = "display: inline-block;width: 60%;",
          numericInput("tgtID", NULL, 1000, width = "80%", step = 100)),
      tags$style("#tgtID { font-size:12px; }")
    )
  ),
  fluidRow(
    column(12,
      align = "left", offset = 0, 
      div(style = "display: inline-block;width: 30%;", 
          actionButton(
            "btn_sb_tgtID_map_zoom", "Zoom", 
            style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; height:30px; width: 60px; font-size:16px; text-align: center"
          )
      ),
      div(style = "display: inline-block;width: 30%;",
          actionButton(
            "btn_sb_tgtID_comb_plot", "Plot", 
            style = "color: #fff; background-color: #aa7ab7; border-color: #2e6da4; height:30px; width: 60px; font-size:16px; text-align: center"
          )
      )
    )
  )
)
# sidebar latlot ----
sb_latlon <- sidebarMenu(
  id = "latlon_menu",
  tags$style(type = "text/css", ".form-control.shiny-bound-input {height: 30px;}"),
  fluidRow(
    column(12, align = "center", "Map Lat/Lon Bounds:",
           style = "font-size:16px; font-weight: bold")
  ),
  fluidRow(
    column(
      12, align = "center", 
      div(
        style = "display: inline-block;width: 45%; font-size:16px; font-weight: bold", 
        numericInput("NLat", "North Lat", 0, min = -90, max = 90, step = 5)
      )),
  ),
  fluidRow(
    column(
      12, align = "center",
      div(
        style = "display: inline-block;width: 50%; font-size:16px; font-weight: bold", 
        numericInput("WLon", "West Lon", 0, min = -180, max = 180, step = 5)
      ),
      div(
        style = "display: inline-block;width: 50%; font-size:16px; font-weight: bold", 
        numericInput("ELon", "East Lon", 0, min = -180, max = 180, step = 5)
      )
    )
  ),
  fluidRow(
    column(
      12, align = "center", 
      div(
        style = "display: inline-block;width: 45%; font-size:16px; font-weight: bold", 
        numericInput("SLat", "South Lat", 0, min = -90, max = 90, step = 5)
      )
    )
  ),
  fluidRow(
    column(
      12, align = "center", 
      actionButton(
        "btn_sb_bound_map_zoom", "Zoom", 
        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; width:70px; height:30px; font-size:16px"
      )
    )
  )
)
# sidebar ----
sidebar <- dashboardSidebar(
  width = 250,
  sb_comb1,
  hr(),
  sb_comb2,
  hr(),
  sb_tgt,
  hr(),
  sb_latlon
)
# body ----
body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css")
  ),
  mainPanel(
    width = 12,
    tabsetPanel(
      id = "navbar",
      tabPanel(
        title = "Map",
        div(
          class = "map",
          leafletOutput("map", height = "88vh"),
          absolutePanel(
            id = "controls", class = "panel panel-default", fixed = TRUE,
            draggable = TRUE, top = 105, left = "auto", right = 10, bottom = "auto",
            width = 400, height = "auto",
            h4("Data Preview"),
            DTOutput("table_tgt_map", height = 350),
            actionButton("btn_table_comb_plot", "Plot")
          ),
          absolutePanel(
            id = "controls", class = "panel panel-default", fixed = TRUE,
            draggable = TRUE, top = "auto", left = "auto", right = 10, bottom = 5,
            width = 700, height = 350,
            h4("Plot Preview"),
            plotlyOutput("comb_map", height = 300) %>% withSpinner(type = 4)
          ),
          absolutePanel(
            id = "mouse_coord", class = "panel panel-default", fixed = TRUE,
            draggable = FALSE, top = "auto", left = 420, right = "auto", bottom = 10,
            width = 200, height = 30,
            verbatimTextOutput("box_scroll")
          )
        )
      ),
      tabPanel(
        title = "Data",
        div(
          class = "data",
          fluidRow(
            plotlyOutput("comb_data", height = 400) %>% withSpinner(type = 4)
          ),
          tabsetPanel(
            id = "data-nav",
            tabPanel(
              "Primary",
              fluidRow(
                height = 300,
                column(width = 6, DTOutput("table_prisys_data", height = 300)) %>% withSpinner(type = 4), 
                column(width = 6, DTOutput("table_prisysout_data", height = 300)) %>% withSpinner(type = 4)
              )
            ),
            tabPanel(
              "Secondary",
              fluidRow(
                height = 300,
                column(width = 6, DTOutput("table_secsys_data", height = 300)) %>% withSpinner(type = 4)
              )
            )
          )
        )
      ),
      tabPanel(
        title = "Chron",
        div(
          class = "data",
          fluidRow(
            plotlyOutput("chron_chron", height = 700, width = 1400) %>% withSpinner(type = 4)
          )
        )
      )
    )
  )
)
# page ----
ui <- dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body
)
