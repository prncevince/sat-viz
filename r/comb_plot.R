suppressPackageStartUpMessages({
  library(dplyr)
  library(ggplot2)
  library(plotly)
  library(viridis)
})
  
#' Create a SAVi combplot
#'
#' @param df_acc A data frame. Contains multiple variables of access data.
#' \describe{
#'   \item{acc}{A numeric vector. The start y-value access time for the
#'   \code{geom_segment()} function.}
#'   \item{acc_next}{A numeric vector. The end y-value for \code{geom_segment()}
#'   .}
#'   \item{end_date_frac}{A numeric vector. The start x-value simulation day for
#'   the \code{geom_segment()} function.}
#'   \item{end_date_frac_next}{A numeric vector the end x-value for the
#'   \code{geom_segment()} function.}
#'   \item{geo}{A numeric vector. The elevation angle of a point on the
#'   ground for a segment. Used in metadata.}
#'   \item{geo_next}{A numeric vector. The elevation angle of the next segment.
#'   Used in metadata.}
#'   \item{ID}{A numeric vector. The unique ID of the access window.}
#'   \item{vst_key}{A character vector. A vehicle sensor target key.}
#' }
#' @param df_out A data frame. Like \code{df_acc} but contains outage window
#' data.
#' @param df_tgt A data frame. Contains target data.
#' \describe{
#'   \item{geoID}{A numeric vector. Unique target IDs.}
#'   \item{lat}{A numeric vector. Latitude from -90 to 90}
#'   \item{lon}{A numeric vector. Longitude from -180 to 180}
#' }
#' @param id A numeric value. The unique ID of the target.
#' @param date A string. The start date of the plotted time interval in
#' YYYY-MM-DD format
#' @param lightpoly A list of data frames. Each dataframe contains data on
#' displaying the lighting condition polygons.
#' \describ{
#'   \item{lightpoly[[1]]}{A data frame. Contains dusk times.}
#'   \item{lightpoly[[2]]}{A data frame. Contains dawn times.}
#'   \item{lightpoly[[3]]}{A data frame. Contains day times.}
#' }
#' @return An plotly htmlwidget. This is of the unique SAVi plot.
#' @examples
#' comb_plot(
#'   prisys_list[[2]], prisysout_list[[2]], geo_df, 2,
#'   "2020-03-20", lightpolys_list[[2]]
#' )
#' 
comb_plot <- function(df_acc, df_acc_sec, df_out, df_tgt, id, date, lightpoly, traces, xmin, xmax) {
  
  # DATE SUPPORTING DATA FOR PLOT ----------------------------
  # numeric vector of day numbers
  date_nums <- seq(
    min(floor(df_acc$end_date_frac)),
    max(floor(df_acc$end_date_frac)),
    1
  )
  date <- as.Date(date, format = "%Y-%m-%d")
  date_seq <- seq(date, date + (length(date_nums) - 1), 1)
  df_date <- data.frame(day_num = date_nums, date = date_seq)
  
  # SUMMARY DF - JOIN WITH ACCESS DF FOR PLOT ----------------------------
  # duration assumes that a negative diff would be a day rollover
  # dont include them - this will need to be validated on the high side
  df_summary <- df_acc %>%
    group_by(vst_key, ID) %>%
    summarise(
      dur = sum(diff(acc)[which(diff(acc) > 0)]),
      min_geo = min(geo),
      max_geo = max(geo)
    )
  # for plotting, left join the plotting dataframe with summary df_acc
  # this data was originally encoded into the plot data, but the thought was that
  # it would be more data storage efficient and low overhead to calculate the above
  # stats and then rejoin for plotting
  # TODO: add suppressWarnings() - different attributes are due to data.table storage
  df_acc <- left_join(df_acc, df_summary, by = c("vst_key", "ID"))
  
  # PLOTTING ----------------------------
  # plot savi data
  # TODO: suppress warning of "Ignoring unknown aesthetics: text"
  p <- ggplot() +
    scale_color_viridis() +
    scale_y_continuous(name="Local Time (Hr)", limits = c(0, 24), breaks = seq(0, 24, by = 4), expand = c(0,0)) +
    scale_x_date(name="Day (Date)", date_labels = "%F", date_breaks = "1 day", expand = c(0,0)) +
    coord_cartesian(xlim = c(df_date$date[xmin], df_date$date[xmax]), ylim = c(0, 24)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      title = paste(
        "Access Windows for ID:", id, "- Long:", df_tgt %>% filter(geoID == id) %>% pull(lon), 
        "Lat:", df_tgt %>% filter(geoID == id) %>% pull(lat)
      )
    )
  
  if (!is.null(traces)){
    if ("LightPolys" %in% traces){
      p <- p +
        geom_polygon(data = lightpoly[[1]], aes(x = day_num - 1 + date, y = dusk), fill = "blue", alpha = 0.2) +
        geom_polygon(data = lightpoly[[2]], aes(x = day_num - 1 + date, y = dawn), fill = "blue", alpha = 0.2) +
        geom_polygon(data = lightpoly[[3]], aes(x = day_num - 1 + date, y = time), fill = "orange", alpha = 0.2)
    }
    if ("PriSysOut" %in% traces){
      p <- p + geom_segment(
        data = df_out, colour = "red", size = 1.5,
        aes(
          x = end_date_frac - 1 + date, xend = end_date_frac_next - 1 + date,
          y = acc, yend = acc_next
        )
      )
    }
    #bottom layer - secondary system
    if ("SecSys" %in% traces){
      p <- p + geom_segment(
        data = df_acc_sec,
        aes(x = end_date_frac - 1 + date, xend = end_date_frac_next - 1 + date,
            y = acc, yend = acc_next),
        colour = "orange",
        size = 1.75
      )
    }
    if ("PriSys" %in% traces){
      p <- p + 
        geom_segment(
          data = df_acc,
          aes(
            x = end_date_frac - 1 + date, xend = end_date_frac_next - 1 + date,
            y = acc, yend = acc_next,
            colour = round(geo, digits = 0), # rounding generates less color traces = faster map execution
            text = sprintf(
              "Duration: %.2f hr<br>Geo Range: %.2f-%.2f<br>Start Geo: %.2f<br>Stop Geo: %.2f",
              dur, min_geo, max_geo, geo, geo_next, end_date_frac
            )
          ),
          size = 1.0
        )
    }
    
  }
  pl <- ggplotly(p, tooltip = c("text"), dynamicTicks = TRUE)
  
  ms_d <- 60*60*24*10^3
  ms_h <- 1*60*60*10^3
  # format x-axis to be date-numeric
  for (i in 1:length(pl$x$data)) {
    pl$x$data[[i]]$x <- suppressWarnings(
      as.numeric(pl$x$data[[i]]$x) * ms_d
    )
  }
  pl$x$layout$xaxis$autorange <- FALSE
  pl$x$layout$xaxis$range <- as.numeric(pl$x$layout$xaxis$range) * ms_d
  pl$x$layout$yaxis$autorange <- FALSE
  pl$x$layout$yaxis$tickmode <- "array"
  pl$x$layout$yaxis$range <- c(0,24)
  pl$x$layout$yaxis$tickvals <- seq(0,24,4)
  pl$x$layout$yaxis$fixedrange <- TRUE # restrict y axis zoom
  pl$x$layout$dragmode <- 'pan'
  pl$x$layout$title$x <- 0.5
  pl$x$config$modBarButtonsToRemove <- c("select2d", "lasso2d")
  pl$x$config$displaylogo <- FALSE
  pl$x$config$scrollZoom <- TRUE
  len_tr <- length(pl$x$data)
  # Legend & Colorbar
  pl$x$data[[len_tr]]$marker$colorbar$title <- "Geo"
  pl$x$layout$showlegend <- TRUE
  pl$x$data[[4]]$showlegend <- TRUE # show outage windows
  pl$x$data[[4]]$name <- "Outage"
  return(pl)
}
