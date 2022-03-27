library(dplyr)
library(plotly)


################################################################################
# function for building a radial chron plot with slightly modified SAVi data


chron_plot <- function(df_acc, df_acc_sec, df_out, df_tgt, id, date, lightpoly, traces, xmin, xmax, upxmin, upxmax){
  
  build_st <- Sys.time()
  #need to cycle through all days
  #days <- seq(start_day,stop_day,1)
  
  days <- seq(xmin,xmax,1)
  dates <- seq(as.Date(date)+xmin-1,as.Date(date)+xmax-1,1)
  #TODO figure out a better way to have the user enter this data...
  plot_str_df <- data.frame(series = c("prisys","prisysout","secsys","light","dark"),
                            yval = c(2,1.8,1.6,2.5,2.5),
                            color = c("green","red","orange","yellow","blue"),
                            opac = c(0.8,0.8,0.8,0.2,0.2),
                            res = c(10,10,10,10,10))
  
  #initiate plot object
  chron_pl <- plot_ly(type = "scatterpolar",
                   mode = "lines")
  
  for (i in 1:length(days)){
    
    #store off the day number
    daynum <- days[i]
    current_date <- dates[i]
    
    #pull the dawn and dusk values for the current day
    dusk_val <- filter(lightpoly[[1]],day_num==daynum & dusk<24)$dusk
    dawn_val <- filter(lightpoly[[2]],day_num==daynum & dawn>0)$dawn
    
    #print(paste("building day ",daynum,sep=""))
    
    #filter down the data to the current day
    prisys_fil <- filter(df_acc,end_date_frac>=daynum & end_date_frac <= daynum + 1)
    prisysout_fil <- filter(df_out,end_date_frac>=daynum & end_date_frac <= daynum + 1)
    secsys_fil <- filter(df_acc_sec,end_date_frac>=daynum & end_date_frac <= daynum + 1)
    #tersys_fil <- filter(tersys,end_date_frac>=daynum & end_date_frac <= daynum + 1)
    
    #build out each of the plot series sets, pass the user defined plot parameters
    
    if (!is.null(traces)){
      if ("LightPolys" %in% traces){
        
        #build out light data
        lightpoly_ind <- which(plot_str_df$series=="light")
        lightpoly_vec <- c(rep(0,plot_str_df$res[lightpoly_ind]),
                           rep(plot_str_df$yval[lightpoly_ind],plot_str_df$res[lightpoly_ind]))
        light_df <- data.frame(acc = dawn_val,
                               acc_next = dusk_val)
        
        #add light data to plot
        chron_pl <- add_segments(light_df,lightpoly_vec,daynum,
                                 plot_str_df$color[lightpoly_ind],
                                 plot_str_df$opac[lightpoly_ind], chron_pl, "light", current_date)
        
        #build out dark data
        darkpoly_ind <- which(plot_str_df$series=="dark")
        darkpoly_vec <- c(rep(0,plot_str_df$res[darkpoly_ind]),
                          rep(plot_str_df$yval[darkpoly_ind],plot_str_df$res[darkpoly_ind]))
        dark_df <- data.frame(acc = c(0,dusk_val),
                              acc_next = c(dawn_val,24))
        
        #add dark data to plot
        chron_pl <- add_segments(dark_df,darkpoly_vec,daynum,
                                 plot_str_df$color[darkpoly_ind],
                                 plot_str_df$opac[darkpoly_ind], chron_pl, "dark", current_date)
      }
      
      if ("PriSysOut" %in% traces){
        
        #build out prisysout data
        prisysout_ind <- which(plot_str_df$series=="prisysout")
        prisysout_vec <- c(rep(plot_str_df$yval[prisysout_ind],plot_str_df$res[prisysout_ind]),
                           rep(plot_str_df$yval[prisysout_ind]-0.2,plot_str_df$res[prisysout_ind]))
        
        #add prisysout data to plot
        chron_pl <- add_segments(prisysout_fil,prisysout_vec,daynum,
                                 plot_str_df$color[prisysout_ind],
                                 plot_str_df$opac[prisysout_ind], chron_pl, "PriSysOut", current_date)
        
      }
      
      if ("PriSys" %in% traces){
        
        #build out prisys data
        prisys_ind <- which(plot_str_df$series=="prisys")
        prisys_vec <- c(rep(plot_str_df$yval[prisys_ind],plot_str_df$res[prisys_ind]),
                        rep(plot_str_df$yval[prisys_ind]-0.2,plot_str_df$res[prisys_ind]))
        
        #add prisys data to plot
        chron_pl <- add_segments(prisys_fil,prisys_vec,daynum,
                                 plot_str_df$color[prisys_ind],
                                 plot_str_df$opac[prisys_ind], chron_pl, "PriSys", current_date)
        
      }
      
      if ("SecSys" %in% traces){
        
        #build out secsys data
        secsys_ind <- which(plot_str_df$series=="secsys")
        secsys_vec <- c(rep(plot_str_df$yval[secsys_ind],plot_str_df$res[secsys_ind]),
                        rep(plot_str_df$yval[secsys_ind]-0.2,plot_str_df$res[secsys_ind]))
        
        #add secsys data to plot
        chron_pl <- add_segments(secsys_fil,secsys_vec,daynum,
                                 plot_str_df$color[secsys_ind],
                                 plot_str_df$opac[secsys_ind], chron_pl, "SecSys", current_date)
        
      }
      
      
      #### TERTIARY SYSTEM DATA - PROTOTYPED AS PART OF CHRON PLOT, LEAVING IT OUT FOR NOW IN SAVI ####
      #build out tersys data
      #tersys_ind <- which(plot_str_df$series=="tersys")
      #tersys_vec <- c(rep(plot_str_df$yval[tersys_ind],plot_str_df$res[tersys_ind]),
      #                rep(plot_str_df$yval[tersys_ind]-0.2,plot_str_df$res[tersys_ind]))
      
      #add tersys data to plot
      #chron_pl <- add_segments(tersys_fil,tersys_vec,daynum,
      #                         plot_str_df$color[tersys_ind],
      #                         plot_str_df$opac[tersys_ind], chron_pl)
    }
    
    
    
  }
  
  #set some of the formatting for the plot
  chron_title = paste(
    "Access Windows for target located at Long:",
    df_tgt %>% filter(geoID == id) %>% pull(lon),
    "Lat:", df_tgt %>% filter(geoID == id) %>% pull(lat),
    "ID: ", id
  )
  #pmargin <- list(t = 100, r = 10, b = 10, l = 40)
  
  chron_pl <- chron_pl %>%
    layout(title = list(text=chron_title,x=0.0,y=0.99),
           font= list(size = 12),
           polar = list(
             radialaxis = list(
               visible = F,
               range = c(0,2.5)
             ),
             angularaxis = list(
               rotation = 90,
               direction = 'clockwise',
               tickmode = 'array',
               tick0 = 0,
               dtick = "L15", # tick mark every 15 degrees you'll use this to label your hours
               tickvals = seq(0,345,15),
               ticktext = seq(0,23,1)
             )
           )
    )%>%
    #title = list(text="Chron Plot",x=0.4,y=0.7)) %>% 
    animation_slider(
      currentvalue = list(prefix = " ", font = list(color="red"))
    )
  
  build_end <- Sys.time()
  print(build_end - build_st)
  #chron_pl <- chron_pl %>% layout(sliders = list(list(pad = list(t = 20))))
  #browser()
  m <- list(
    b = 40,
    l = 60,
    t = 25,
    r = 40,
    pad = 10
  )
  chron_pl <- chron_pl %>% layout(margin = m)
  
  return(chron_pl)
  
}

################################################################################
# function to add traces to the plotly polar plot

add_segments <- function(interval_df,yvec,daynum,
                         color,opacity,pl, series, current_date){
  
  if (series=="PriSys"|series=="PriSysOut"){
    
    #minimize data so you have consistency across all input DFs
    unique_IDs <- unique(interval_df$ID)
    skinny_list <- list()
    
    for (i in 1:length(unique_IDs)){
      ID_ind <- which(interval_df$ID==unique_IDs[i])
      first_ind <- ID_ind[1]
      last_ind <- ID_ind[length(ID_ind)]
      base_row <- interval_df[first_ind,]
      base_row$acc_next <- interval_df$acc_next[last_ind]
      base_row$end_date_frac_next <- interval_df$end_date_frac_next[last_ind]
      skinny_list[[i]] <- base_row
    }
    skinny_df <- rbindlist(skinny_list)
    rownames(skinny_df) <- NULL
    
  } else {
    
    skinny_df <- interval_df
    
  }
  
  for (i in 1:nrow(skinny_df)){
    
    start <- skinny_df$acc[i]
    stop <- skinny_df$acc_next[i]
    interp_vec <- seq(start,stop,(stop-start)/9)
    rec_vec <- c(interp_vec,rev(interp_vec))
    plot_df <- data.frame(rad = yvec,
                          theta = rec_vec,
                          day = rep(daynum,length(rec_vec)),
                          date = rep(current_date,length(rec_vec)))

    pl <- pl %>%
      add_trace(data = plot_df,
                r = ~rad,
                theta = ~theta*15,
                fill = "toself",
                fillcolor = color,
                line = list(color = color),
                text = ~paste("start:",min(theta)," stop:",max(theta)),
                hoverinfo = "text",
                hoveron="points+fills",
                opacity = opacity,
                showlegend = F,
                #frame = ~day
                frame = ~date
      )
  }
  return(pl)
}
