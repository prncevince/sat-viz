# savi .RData building
suppressPackageStartupMessages({
  library(countrycode)
  library(data.table)
  library(dplyr)
  library(lubridate)
  library(maps)
  library(stringr)
})

# builds the geo_df data
build_geoID_data <- function(volume) {
  
  if (volume=="large"){
    
    #build a finer resolution set of data with high res regions
    base_lon_vec <- seq(-170,170, 5)
    base_lat_vec <- seq(-80, 80, 5)
    base_lon_col <- rep(base_lon_vec, times = length(base_lat_vec))
    base_lat_col <- rep(base_lat_vec, each = length(base_lon_vec))
    base_geo_df <- data.frame(cbind(base_lon_col, base_lat_col), stringsAsFactors = FALSE)
    colnames(base_geo_df) <- c("lon", "lat")
    #high res regions
    us_lon_vec <- seq(-123,-66,1)
    us_lat_vec <- seq(23,50,1)
    us_lon_col <- rep(us_lon_vec, times = length(us_lat_vec))
    us_lat_col <- rep(us_lat_vec, each = length(us_lon_vec))
    reg1_geo_df <- data.frame(cbind(us_lon_vec, us_lat_col), stringsAsFactors = FALSE)
    colnames(reg1_geo_df) <- c("lon", "lat")
    eu_lon_vec <- seq(10,50,1)
    eu_lat_vec <- seq(30,60,1)
    eu_lon_col <- rep(eu_lon_vec, times = length(eu_lat_vec))
    eu_lat_col <- rep(eu_lat_vec, each = length(eu_lon_vec))
    reg2_geo_df <- data.frame(cbind(eu_lon_vec, eu_lat_col), stringsAsFactors = FALSE)
    colnames(reg2_geo_df) <- c("lon", "lat")
    #glue together geo dfs, integrate the high res regions into the base map
    geo_df <- rbind(base_geo_df,reg1_geo_df,reg2_geo_df)
    colnames(geo_df) <- c("lon", "lat")
    geo_df <- geo_df[!duplicated(geo_df),]
    geo_df <- geo_df[order(geo_df[,1], geo_df[,2]), ]
    rownames(geo_df) <- NULL

  } else if (volume=="small"){
    
    #initial dataset of 45
    lon_vec <- seq(-100, 100, 25)
    lat_vec <- seq(-60, 60, 30)
    lon_col <- rep(lon_vec, times = length(lat_vec))
    lat_col <- rep(lat_vec, each = length(lon_vec))
    geo_df <- data.frame(cbind(lon_col, lat_col), stringsAsFactors = FALSE)
    colnames(geo_df) <- c("lon", "lat")
    rownames(geo_df) <- NULL
    
  }
  
  #build an ID column
  geo_df$geoID <- as.numeric(row.names(geo_df))
  geo_df$country <- map.where(database="world", geo_df$lon, geo_df$lat)
  #returned country string sometimes has a : and a subregion, filter out just keep country
  region_ind <- which(geo_df$country %like% ":")
  c_rep_vec <- geo_df$country[region_ind]
  colon_loc <- str_locate(c_rep_vec, ":")[,1]
  geo_df$country[region_ind] <- substr(c_rep_vec,start=1,stop=(colon_loc-1))
  geo_df$country[is.na(geo_df$country)] <- "Unk"
  #attach country codes
  geo_df$CC <- suppressWarnings(
    countrycode(geo_df$country, origin="country.name",destination="iso2c")
  )
  geo_df$CC[is.na(geo_df$CC)] <- "ZZ"
  return(geo_df)
}

# builds secondary system data
build_secsys_data <- function(geoID) {
  #go through 1000 days
  plot_list <- list()
  for (i in 1:1000){
    #build a randomized 10 interval dataframe
    int_list <- list()
    for (j in 1:5){
      rnum <- round(runif(1, 0.5, 3),digits=1)
      if (j==1){
        #pick a start time somewhat random
        s1 <- rnum
        #current_int <- c(s1,s1+round(runif(1, 0.2, 0.5),digits=1))
        current_int <- c(s1,s1+rnum)
      } else {
        #s2 <- current_int[2]+round(runif(1, 1, 2),digits=1)
        s2 <- current_int[2]+rnum
        #current_int <- c(s2,s2+round(runif(1, 0.2, 0.5),digits=1))
        current_int <- c(s2,s2+rnum)
        #check max of interval - if > 23, quit
        if (max(current_int)>23.9){
          break
        }
      }
      int_list[[j]] <- current_int
    }
    #compile interval start/stops, add different fields
    acc_df <- data.frame(t(data.frame(int_list)))
    colnames(acc_df) <- c("acc","acc_next")
    rownames(acc_df) <- NULL
    #build other fields (except geo)
    acc_df$end_date_frac <- acc_df$acc/24+i
    acc_df$end_date_frac_next <- acc_df$acc_next/24+i
    #acc_df$ID <- as.numeric(rownames(acc_df))
    acc_df$vst_key <- paste("Sys2__SenX__",geoID,sep="")
    plot_list[[i]] <- acc_df
  }
  #bind all data together
  secsys_single_case_df <- rbindlist(plot_list)
  secsys_single_case_df$ID <- as.numeric(rownames(secsys_single_case_df)) #each row is an independent interval
  return(secsys_single_case_df)
}

# builds a list of prisys cases with randomization applied
build_secsys_list <- function(geo_df, build_secsys_data) {
  #set a random seed at the start of the interval gen process
  set.seed(5)
  #initialize list for secsys data
  secsys_list <- list()
  #loop through all of the geo_df
  for (i in 1:nrow(geo_df)){
    #pull the current geoID
    current_geoID <- geo_df$geoID[i]
    #run the data gen function
    current_data <- build_secsys_data(current_geoID)
    secsys_list[[i]] <- current_data
  }
  names(secsys_list) <- geo_df$geoID
  return(secsys_list)
}

# builds outage intervals based on an existing case
build_outage_data <- function(single_case_skn_seg_df) {
  # make a dataframe that has the outage intervals based on single case data
  start_ind <- c(3:5, 28:30)
  all_ind <- c()
  for (i in 1:1000) { # hardcoded at 1000 days - may need to update this
    all_ind <- c(all_ind, ((i - 1) * 33) + start_ind)
  }
  single_outage_case_seg_df <- single_case_skn_seg_df[all_ind, ]
  return(single_outage_case_seg_df)
}

# builds a list of outage cases based on example data
build_outage_list <- function(geo_df, single_outage_case_seg_df) {
  prisysout_list <- list()
  # find the vehicle/sensor of the sample dataset
  vst_vec <- str_split(single_outage_case_seg_df$vst_key[1], "_")[[1]]
  for (i in 1:nrow(geo_df)) {
    # pull the current geoID
    current_geoID <- geo_df$geoID[i]
    # set current_data to single case provided
    current_data <- single_outage_case_seg_df
    # rebuild the vst_key - this is partially hardcoded which isnt ideal but whatever
    current_data$vst_key <- paste(vst_vec[1], "Out", current_geoID, sep = "__")
    # modify the vst_key, write out the data to list
    prisysout_list[[i]] <- current_data
  }
  names(prisysout_list) <- geo_df$geoID
  return(prisysout_list)
}

# builds a light polygons list
# This is for a single case
build_light_polys <- function() {
  # build a dusk polygon
  dusk_vec <- c(seq(17, 19, 0.25), seq(19, 18, -0.25), seq(18, 20, 0.25), seq(20, 18.5, -0.5), seq(18, 20, 0.25), seq(20, 17, -0.25), 15.75)
  dusk_vec_all <- rep(dusk_vec, 20)
  day_vec <- seq(1, 1000, 1)
  dusk_poly <- data.frame(day_num = day_vec, dusk = dusk_vec, stringsAsFactors = FALSE)
  back_df <- data.frame(day_num = seq(1000, 1, (-1000 / nrow(dusk_poly))), dusk = rep(24, len = nrow(dusk_poly)), stringsAsFactors = FALSE)
  np1_df <- rbind(dusk_poly, back_df)
  # build a dawn polygon
  dawn_vec <- c(seq(7, 9, 0.25), seq(9, 8, -0.25), seq(8, 10, 0.25), seq(10, 8, -0.5), seq(8, 10, 0.25), seq(10, 7, -0.25))
  dawn_vec_all <- rep(dawn_vec, 20)
  dawn_poly <- data.frame(day_num = day_vec, dawn = dawn_vec, stringsAsFactors = FALSE)
  back_df <- data.frame(day_num = seq(1000, 1, (-1000 / nrow(dawn_poly))), dawn = rep(0, len = nrow(dawn_poly)), stringsAsFactors = FALSE)
  np2_df <- rbind(dawn_poly, back_df)
  # build a sun polygon
  sun_poly1 <- data.frame(day_num = c(day_vec, rev(day_vec)), time = c(dusk_vec_all, rev(dawn_vec_all)), stringsAsFactors = FALSE)
  lightpoly_example <- list(np1_df, np2_df, sun_poly1)
  return(lightpoly_example)
}

# builds a light polygons list
# This is for as many cases in the geo_df
build_light_polys_list <- function(geo_df, lightpoly_example) {
  lightpolys_list <- list()
  for (i in 1:nrow(geo_df)) {
    lightpolys_list[[i]] <- lightpoly_example
  }
  names(lightpolys_list) <- geo_df$geoID
  return(lightpolys_list)
}

# builds a pri sys example dataset, reduced
# This is for a single geolocation
build_prisys_red_data <- function() {
  # build a dataset for a single day's worth of intervals
  # ACC 1
  acc1_vec <- seq(1.0, 4, 3 / 10)
  acc1_Id <- rep(1, length(acc1_vec))
  date_frac1 <- acc1_vec / 24
  geo_vec1 <- c(seq(0, 40, 40 / 4), seq(40, 20, -20 / 5)) # 11
  dur_vec1 <- rep(3, length(acc1_vec))
  min_geo1 <- rep(min(geo_vec1), length(acc1_vec))
  max_geo1 <- rep(max(geo_vec1), length(acc1_vec))
  # ACC 2
  acc2_vec <- seq(7.0, 10.0, 3 / 10)
  acc2_Id <- rep(2, length(acc2_vec))
  date_frac2 <- acc2_vec / 24
  geo_vec2 <- c(seq(0, 80, 80 / 4), seq(80, 10, -70 / 5)) # 11
  dur_vec2 <- rep(3, length(acc2_vec))
  min_geo2 <- rep(min(geo_vec2), length(acc2_vec))
  max_geo2 <- rep(max(geo_vec2), length(acc2_vec))
  # ACC 3
  acc3_vec <- seq(13.0, 16.0, 3 / 10)
  acc3_Id <- rep(3, length(acc3_vec))
  date_frac3 <- acc3_vec / 24
  # geo_vec3 <- acc3_vec*2
  geo_vec3 <- c(seq(40, 60, 20 / 4), seq(60, 15, -45 / 5)) # 11
  dur_vec3 <- rep(3, length(acc3_vec))
  min_geo3 <- rep(min(geo_vec3), length(acc3_vec))
  max_geo3 <- rep(max(geo_vec3), length(acc3_vec))

  total_len <- length(acc1_vec) + length(acc2_vec) + length(acc3_vec)
  plot_list <- list()
  # then go through 1000 days and repeat/modify above daily dataset
  for (i in 1:1000) {
    # glue together above vectors into a dataframe as a function of what day the loop is on
    # day 5, ID would be 13, end date 5,
    acc_df <- data.frame(
      acc = c(acc1_vec, acc2_vec, acc3_vec), # access times in fractional hours of day
      ID = c(acc1_Id, acc2_Id, acc3_Id) + (i - 1) * 3, # access IDs
      end_date = rep(i, total_len),
      end_date_frac = c(date_frac1, date_frac2, date_frac3) + (i),
      geo = c(geo_vec1, geo_vec2, geo_vec3),
      dur = c(dur_vec1, dur_vec2, dur_vec3),
      min_geo = c(min_geo1, min_geo2, min_geo3),
      max_geo = c(max_geo1, max_geo2, max_geo3),
      veh = rep("Sys1", total_len),
      sen = rep("Sen1", total_len),
      tgt = rep(1, total_len), stringsAsFactors = FALSE
    )
    plot_list[[i]] <- acc_df
  }
  # bind all data together
  single_case_red_df <- rbindlist(plot_list)
  return(single_case_red_df)
}

# builds a list of prisys cases with randomization applied
build_prisys_list <- function(geo_df, single_case_skn_seg_df) {
  #set a random seed at the beginning of this process
  set.seed(10)
  prisys_list <- list()
  # create a day integer column
  single_case_skn_seg_df$day <- floor(single_case_skn_seg_df$end_date_frac)
  # build a day integer vector
  day_ids <- unique(single_case_skn_seg_df$day)
  # find the vehicle/sensor of the sample dataset
  vst_vec <- str_split(single_case_skn_seg_df$vst_key[1], "__")[[1]]
  for (i in 1:nrow(geo_df)) {
    # pull the current geoID
    current_geoID <- geo_df$geoID[i]
    # set current_data to single case provided
    current_data <- single_case_skn_seg_df
    # rebuild the vst_key - this is partially hardcoded which isnt ideal but whatever
    current_data$vst_key <- paste(vst_vec[1], vst_vec[2], current_geoID, sep = "__")
    # now go through each day and randomize the intervals
    for (j in 1:length(day_ids)) {
      current_int_index <- which(current_data$day == day_ids[j])
      x1 <- round(runif(1, 0, 7), digits = 1)
      # now assign acc and acc_next based on the random num generated
      current_data$acc[current_int_index] <- current_data$acc[current_int_index] + x1
      current_data$acc_next[current_int_index] <- current_data$acc_next[current_int_index] + x1
      # recalculate end_day_frac and next - based on acc/acc_next and current day
      current_data$end_date_frac[current_int_index] <- day_ids[j] +
        current_data$acc[current_int_index] / 24
      current_data$end_date_frac_next[current_int_index] <- day_ids[j] +
        current_data$acc_next[current_int_index] / 24
      # update geo and geo_next
      x2 <- round(runif(1, 0, 50), digits = 0)
      current_data$geo[current_int_index] <- current_data$geo[current_int_index] + x2
      current_data$geo_next[current_int_index] <- current_data$geo_next[current_int_index] + x2
    }
    # now delete the extra column
    current_data <- current_data[, -c(9)]
    prisys_list[[i]] <- current_data
  }
  names(prisys_list) <- geo_df$geoID
  return(prisys_list)
}

# converts a single reduced dataframe to segments
convert_to_seg <- function(single_case_red_df) {
  # remove unneeded columns ----
  # delete end_date, min_geo, max_geo, dur - to be joined later in plotting code
  single_case_skn_df <- single_case_red_df[, -c("end_date", "dur", "min_geo", "max_geo")]
  # create a single key string instead of 3 columns
  single_case_skn_df$vst_key <- paste(
    single_case_skn_df$veh, single_case_skn_df$sen, single_case_skn_df$tgt,
    sep = "__"
  )
  # now delete the other columns used to build the key
  single_case_skn_df <- single_case_skn_df[, -c("veh", "sen", "tgt")]
  # convert to segments ----
  # translate this geom_segments - which will add more columns but hopefully reduce rows
  single_case_skn_seg_df <- single_case_skn_df %>%
    mutate(
      end_date_frac_next = lead(end_date_frac),
      acc_next = lead(acc),
      geo_next = lead(geo)
    )
  # now you need to find the ends of each segment
  ends_IDs <- which(diff(single_case_skn_seg_df$ID) > 0)
  single_case_skn_seg_df$end_date_frac_next[ends_IDs] <-
    single_case_skn_seg_df$end_date_frac[ends_IDs]
  single_case_skn_seg_df$acc_next[ends_IDs] <- single_case_skn_seg_df$acc[ends_IDs]
  single_case_skn_seg_df$geo_next[ends_IDs] <- single_case_skn_seg_df$geo[ends_IDs]
  # need to handle the last entry which has no diff
  final_ID <- which(is.na(single_case_skn_seg_df$end_date_frac_next))
  single_case_skn_seg_df$end_date_frac_next[final_ID] <-
    single_case_skn_seg_df$end_date_frac[final_ID]
  single_case_skn_seg_df$acc_next[final_ID] <- single_case_skn_seg_df$acc[final_ID]
  single_case_skn_seg_df$geo_next[final_ID] <- single_case_skn_seg_df$geo[final_ID]
  return(single_case_skn_seg_df)
}
