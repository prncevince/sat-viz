source("r/chron_plot.R")
source("r/comb_plot.R")
source("r/input.R")

library(data.table)
library(dplyr)
library(DT)
library(htmlwidgets)
library(leaflet)
library(maps)
library(plotly)
library(rhdf5)
library(sf)

# OBJECTS ----
# target data ----
geo_df <- h5read(h5_file, paste("input/geo_df", sep = ""))
# leaflet map data ----
# target count by country
d_geo <- geo_df %>%
  group_by(country) %>%
  summarise(count = n())
# world map country dataframe
world_map <- map("world", fill = TRUE, plot = FALSE)
# sf dataframe converted from world map in maps package
d_world_sf <- st_as_sf(world_map, fill = TRUE, group = TRUE)
d_world_sf <- left_join(d_world_sf, d_geo, by = c("ID" = "country")) %>% 
  transmute(COUNT = count, COUNTRY = ID, geom = geom)
# choropleth color palette
breaks <- unique(c(quantile(d_world_sf$COUNT, probs = seq(0, 1, by = 0.10), na.rm = TRUE)))
pal <- colorBin("viridis", bins = breaks)
# tile data
url_tiles <- "https://osm-{s}.gs.mil/tiles/{crs}/{z}/{x}/{y}.png"
url_wms <- "http://osm.gs.mil/ows?"
# found by searching for "<ows:Identifier>osm" at
# https://osm.gs.mil/wmts/1.0.0/WMTSCapabilities.xml
layer_wms <- c(
  'osm-default', 'osm-bright', 'osm-humanitarian', 
  'osm-default-pc', 'osm-bright-pc', 'osm-humanitarian-pc'
)
# subdomains in .mil Tile Layer CRS - https://osm.gs.mil/features/base-map
layer_tiles <- c(
  'default', 'bright', 'humanitarian', 
  'default_pc', 'bright_pc', 'humanitarian_pc'
)
# layer base group names
bg <- c(
  "default", "bright", "humanitarian", 
  "default pc", "bright pc", "humanitarian pc"
)
bg_tiles <- paste("tiles", bg)
bg_wms <- paste("wms", bg)
# date data ----
# date dataframe with numeric days/strings & sim parameters (start date, num days, etc.) from HDF5 input
date_df <- h5read(h5_file, "input/simdates")
colnames(date_df) <- c("daynum", "datenum")
date_df$date <- as.Date(date_df$datenum, origin = "1970-01-01")
ms_d <- 60*60*24*10^3
ms_h <- 1*60*60*10^3
#set the number of days threshold to throw a plot generation warning
day_delta_warning_threshold <- 30

# FUNCTIONS ----
# bounding box for leaflet map zoom event
bounding_box <- function(data, bounds) {
  data %>%
    filter(
      lat > bounds$south & lat < bounds$north & lon < bounds$east & lon > bounds$west
    )
}
# datatable
table_out <- function(group, rv) {
  l <- rv$l_event
  if (is.null(l)) {
    return(NULL)
  } else {
    if (group == "PriSys") {
      current_table <- l[[3]]
      filter_vec <- c("vst_key", "acc", "acc_next", "end_date_frac", "end_date_frac_next", "geo", "geo_next")
    } else if (group == "PriSysOut") {
      current_table <- l[[4]]
      filter_vec <- c("vst_key", "acc", "acc_next", "end_date_frac", "end_date_frac_next")
    } else if (group == "SecSys") {
      current_table <- l[[6]]
      filter_vec <- c("vst_key", "acc", "acc_next", "end_date_frac", "end_date_frac_next")
    }
    # only update after update values are set
    xmin <- isolate(rv$xmin)
    xmax <- isolate(rv$xmax)
    if (!is.null(rv$upxmin)){
      xmin <- rv$upxmin
      xmax <- rv$upxmax
    }
    out_table <- current_table %>%
      filter(end_date_frac_next >= xmin & end_date_frac <= xmax) %>%
      select(all_of(filter_vec))
    return(
      datatable(
        out_table,
        caption = tags$caption(paste("Table: ", group, sep = ""), style = "color:black"),
        extensions = "Scroller",
        #style = "bootstrap",
        class = "compact",
        width = "90%",
        selection = "single",
        options = list(
          deferRender = TRUE,
          scrollY = 250,
          scroller = TRUE
          #dom = "tp"
        )
      )
    )
  }
}
# updates reactive values with input$daterange, nothing is technically "returned"
date_range_rv_update <- function(daterange, rv, date_df) {
  date_fil <- date_df[which(date_df$date >= daterange[1] & date_df$date <= daterange[2]), ]
  row.names(date_fil) <- NULL
  xmin <- min(date_fil$daynum)
  xmax <- max(date_fil$daynum)
  start_day <- date_fil$date[1]
  end_day <- date_fil$date[nrow(date_fil)]
  rv$xmin <- xmin
  rv$xmax <- xmax
  rv$day_delta <- xmax-xmin #TODO may not need this
  rv$start_day <- start_day
  rv$end_day <- end_day
  rv$date_vec <- date_fil$date
}
# check out the date range input span
date_range_check <- function(daterange, rv, date_df) {
  #convert date start/stop to a numeric delta
  date_delta <- as.numeric(daterange[2] - daterange[1])
  #if the delta is over a month, don't update rvs, check plot_confirm flag
  if (date_delta>day_delta_warning_threshold){
    if (rv$plot_confirm){
      #update reactive values, etc.
      date_range_rv_update(daterange, rv, date_df)
    }
  } else {
    #update reactive values, etc.
    date_range_rv_update(daterange, rv, date_df)
  }
}
#daterange input rv updates and plotlyproxy code in a function, chron plot confirmation value
update_plot_date_range <- function(daterange,rv,session){
  #do a check on the date range span to see if a user confirm is needed
  date_range_check(daterange, rv, date_df)
  #set the chron plot reactive values for updates
  rv$upxmin <- rv$xmin
  rv$upxmax <- rv$xmax
  #set plot margin
  # pmargin <- list(t = 46, r = 7, b = 80, l = 37)
  pmargin <- list(t = 42.994497, r = 7.305936, b = 84.5, l = 50)
  x <- list(
    type = "date",
    title = list(text = "Day (Date)",
                 font = list(size = 14.61187)),
    range = c(as.numeric(rv$start_day) * ms_d + ms_h, as.numeric(rv$end_day) * ms_d + ms_h),
    showgrid = TRUE,
    showline = FALSE,
    linecolor = "rgba(0,0,0,0)",
    gridcolor = "rgba(255,255,255,1)",
    ticktext = rv$date_vec,
    tickangle = -45,
    tickfont = list(size = 11.6895, color = "rgba(77,77,77,1)"),
    ticks = "outside"
  )
  y <- list(
    title = list(text = "Local Time (Hr)",
                 font = list(size = 14.61187)),
    range = c(0,24),
    showgrid = TRUE,
    gridcolor = "rgba(255,255,255,1)",
    tickvals = seq(0,24,4),
    tickfont = list(size=11.6895, color = "rgba(77,77,77,1)"),
    ticks = "outside",
    fixedrange = TRUE
  )
  plotlyProxy("comb_map", session) %>%
    plotlyProxyInvoke("relayout", list(xaxis = x, yaxis=y, margin = pmargin))
  plotlyProxy("comb_data", session) %>%
    plotlyProxyInvoke("relayout", list(xaxis = x, yaxis=y, margin = pmargin))
}

# event expression - queries hdf5 data
ex_event <- function(h5_file, geoID, traces) {
  obj1 <- h5read(h5_file, paste("output/PriSys/", geoID, sep = ""))
  obj2 <- h5read(h5_file, paste("output/PriSys_Outage/", geoID, sep = ""))
  obj3 <- h5read(h5_file, paste("output/LightPolys/", geoID, sep = ""))
  obj4 <- h5read(h5_file, paste("output/SecSys/", geoID, sep = ""))
  l <- list(geoID, traces, obj1, obj2, obj3, obj4)
  return(l)
}
# server for shiny
server <- function(input, output, session) {
  # reactive values ----
  rv <- reactiveValues()
  rv$plot_confirm <- FALSE  #initially set plot confirm to FALSE - user needs to drive this to TRUE
  # MAP TAB ----
  # output comb_map, comb_data ----
  comb <- quote({
    l <- rv$l_event
    if (is.null(l)) {
      plot_ly(type="scatter",mode="markers") %>%
        layout(title = "Click a target to generate a combplot")
    } else {
      xmin <- isolate(rv$xmin)
      xmax <- isolate(rv$xmax)
      suppressWarnings(comb_plot(
        df_acc = l[[3]], df_acc_sec = l[[6]], df_out = l[[4]], df_tgt = geo_df, id = l[[1]],
        date = start_date, lightpoly = l[[5]], traces = l[[2]], xmin, xmax
      ))
    }
  })
  output$comb_map <- output$comb_data <- renderPlotly(comb, quoted = T)
  outputOptions(output, "comb_map", suspendWhenHidden = TRUE, priority=10)
  outputOptions(output, "comb_data", suspendWhenHidden = TRUE, priority=10)
  # output map ----
  map <- 
    leaflet(
      data = d_world_sf, options = leafletOptions(worldCopyJump = T)
    ) %>%
    addTiles(group = "DEFAULT") %>%
    addCircleMarkers(
      data = geo_df, ~lon, ~lat, layerId = ~geoID, radius = 4, stroke = FALSE, 
      fillOpacity = 0.8, label = ~paste("Target: ", geoID, sep = ""),  
      popupOptions = popupOptions(closeButton = FALSE),
      clusterOptions = markerClusterOptions()
    ) %>%
    addPolygons(
      group="heatmap", fillColor = ~pal(COUNT),
      stroke = F, smoothFactor = 0.2, fillOpacity = 0.3,
      popup = ~paste(
        "Country: ", COUNTRY, "<br>",
        "Value: ", COUNT, "<br>"
      )
    ) %>% 
    addLegend(
      pal = pal, values = ~COUNT[!is.na(COUNT)], title = "Target Count", "bottomleft", opacity = 0.7
    ) %>%
    addTiles(
      urlTemplate = url_tiles, group = bg_tiles[1],
      options = tileOptions(subdomains = "1234", crs = layer_tiles[1])
    ) %>%
    addTiles(
      urlTemplate = url_tiles, group = bg_tiles[2],
      options = tileOptions(subdomains = "1234", crs = layer_tiles[2])
    ) %>%
    addTiles(
      urlTemplate = url_tiles, group = bg_tiles[3],
      options = tileOptions(subdomains = "1234", crs = layer_tiles[3])
    ) %>%
    addLayersControl(baseGroups = c("DEFAULT", bg_tiles[1:3])) %>%
    setView(0, 0, 2) %>%
    onRender(readLines("www/js/sb.js")) %>%
    onRender(readLines("www/js/toggle-zoom.js"))
  output$map <- renderLeaflet(map)
  # output table_tgt_map ----
  table_tgt_map <- renderDT({
    datatable(
      data_map() , 
      extensions = "Scroller", 
      #style = "bootstrap", 
      class = "compact", 
      width = "100%", 
      selection = "single",
      options = list(
        deferRender = TRUE, scrollY = 250, scroller = TRUE #dom = "tp"
      )
    )
  })
  output$table_tgt_map <- table_tgt_map
  # output box_scroll ----
  output$box_scroll <- renderText({
    if (is.null(input$hover_coordinates)) {
      "Mouse outside of map"
    } else {
      paste0(
        "Lat: ", input$hover_coordinates[1],
        "\nLng: ", input$hover_coordinates[2]
      )
    }
  })
  # DATA TAB ----
  # output table_prisys_data ----
  output$table_prisys_data <- renderDT(table_out("PriSys", rv))
  outputOptions(output, "table_prisys_data", suspendWhenHidden = TRUE, priority=10)
  # output table_prisysout_data ----
  output$table_prisysout_data <- renderDT(table_out("PriSysOut", rv))
  outputOptions(output, "table_prisysout_data", suspendWhenHidden = TRUE, priority=10)
  # output table_secsys_data ----
  output$table_secsys_data <- renderDT(table_out("SecSys", rv))
  outputOptions(output, "table_secsys_data", suspendWhenHidden = TRUE, priority=10)
  # CHRON TAB ----
  # output chron_chron ----
  chron <- quote({
    l <- rv$l_event
    if (is.null(l)) {
      plot_ly(type="scatter",mode="markers") %>%
        layout(title = "Click a target to generate a chron-plot")
    } else {
      xmin <- isolate(rv$xmin)  #actual value used in plot, but isolated
      xmax <- isolate(rv$xmax)  #actual value used in plot, but isolated
      upxmin <- rv$upxmin #dummy variable used to drive plot update
      upxmax <- rv$upxmax #dummy variable used to drive plot update
      chron_plot(
        df_acc = l[[3]], df_acc_sec = l[[6]], df_out = l[[4]], df_tgt = geo_df, id = l[[1]],
        date = start_date, lightpoly = l[[5]], traces = l[[2]], xmin, xmax, upxmin, upxmax
      )
    }
  })
  output$chron_chron <- renderPlotly(chron, quoted = T)
  outputOptions(output, "chron_chron", suspendWhenHidden = TRUE, priority=10)
  # EVENTS ----
  # event map_bounds ----
  # change what is in the datatable based on map bounds
  data_map <- reactive({
    if (is.null(input$map_bounds)) {
      geo_df
    } else {
      bounds <- input$map_bounds
      bounding_box(geo_df, bounds)
    }
  })
  # event map_marker_click ----
  # pull leaflet map marker click data
  observeEvent(
    input$map_marker_click,
    {
      eventdata <- input$map_marker_click
      geoID <- geo_df %>%
        filter(lon == eventdata$lng & lat == eventdata$lat) %>%
        pull(geoID) %>% `[`(1)
      rv$l_event <- ex_event(h5_file, geoID, input$selector)
    }
  )
  # event btn_table_comb_plot ----
  # table_tgt_map (DT) row select and plot button event
  observeEvent(
    input$btn_table_comb_plot, 
    {
      if (!is.null(input$table_tgt_map_rows_selected)) {
        s <- input$table_tgt_map_rows_selected
        data <- data_map()[s, ]
        rv$l_event <- ex_event(h5_file, data$geoID, input$selector)
        if (input$tgtID > 0) {
          # notice `s` above is a single row - so $lon & $lat are single values
          leafletProxy("map") %>% setView(data$lon, data$lat, zoom = 7)
        }
      }
    }
  )
  # event btn_sb_tgtID_map_zoom ----
  # target ID button click map zoom 
  observeEvent(
    input$btn_sb_tgtID_map_zoom, 
    {
      if (input$tgtID > 0) {
        d <- geo_df %>% filter(geoID == input$tgtID)
        leafletProxy("map") %>% setView(d$lon, d$lat, zoom = 7)
      }
    }
  )
  # event btn_sb_tgtID_comb_plot ----
  # target ID button click combplot generation
  observeEvent(
    input$btn_sb_tgtID_comb_plot, 
    {
      if (input$tgtID > 0) {
        rv$l_event <- ex_event(h5_file, geoID = input$tgtID, input$selector)
        d <- geo_df %>% filter(geoID == input$tgtID)
        leafletProxy("map") %>% setView(d$lon, d$lat, zoom = 7)
      }
    }
  )
  # event btn_sb_traces_comb_plot ----
  # "filter" selected traces by selecting & replotting
  observeEvent(
    input$btn_sb_traces_comb_plot, 
    {
      # pull currently plotted geoID
      currentID <- rv$l_event[[1]]
      rv$l_event <- ex_event(h5_file, currentID, input$selector)
    }
  )
  # event btn_sb_bound_map_zoom ----
  # leaflet map bounds update button click
  observeEvent(
    input$btn_sb_bound_map_zoom, 
    {
      l <- list(input$NLat, input$SLat, input$WLon, input$ELon)
      if ((input$NLat > input$SLat) & (input$ELon > input$WLon)) {
        leafletProxy("map") %>% fitBounds(
          lng1 = l[[3]], lat1 = l[[2]], lng2 = l[[4]], lat2 = l[[1]]
        )
      }
    }
  )
  # event daterange ----
  observeEvent(
    input$daterange,
    {
      #run the date range input check code, possibly update reactive values that drive plot updates
      date_range_check(input$daterange, rv, date_df)
    }
  )
  # event btn_sb_daterange_comb_plot ----
  observeEvent(
    input$btn_sb_daterange_comb_plot,
    {
      #calculate the date delta span from input
      date_delta <- as.numeric(input$daterange[2] - input$daterange[1])
      if (date_delta>day_delta_warning_threshold){
        showModal(
          modalDialog(
            title="Date Range Validation - Chron Plot",
            "You picked a date range longer than a month  This will take while to process chron plots.  Proceed?",
            footer = tagList(actionButton("confirm_chron", "Confirm"),modalButton("Cancel"))
          )
        )
      } else {
        #run the plot date range update
        update_plot_date_range(input$daterange,rv,session)
      }
    }
  )
  # event confirm_chron ----
  observeEvent(input$confirm_chron, {
    #set plot confirm to TRUE
    rv$plot_confirm <- TRUE
    #run the plot date range update
    update_plot_date_range(input$daterange,rv,session)
    #set plot confirm back to false
    rv$plot_confirm <- FALSE
    #remove modal
    removeModal()
  })
  # event navbar  - placeholder code for checking if user is on "Chron" tab
  #observeEvent(
  #  input$navbar,
  #  {
  #    if (input$navbar=="Chron"){
  #    }
  #  }
  #)
  
  
}
