suppressPackageStartupMessages({
  library(data.table)
  library(rhdf5)
})

#' save SAVi list .Rdata plotting data into HDF5 hierarchical format/type 
#' @param geo_df
#' @param outfile
h5_input <- function(geo_df, outfile) {
  # before creating an HDF5 file, create an inputs dataframe
  # this will act as a key to users to understand the structure of the HDF5 file hierarchy
  data_cases_list <- list()
  for (i in 1:nrow(geo_df)) {
    # build a dataframe with data categories
    groups <- c("LightPolys", "PriSys", "PriSys_Outage", "SecSys")
    current_case <- data.frame(
      group = groups,
      geoID = rep(geo_df$geoID[i], times = length(groups)), stringsAsFactors = FALSE
    )
    data_cases_list[[i]] <- current_case
  }
  data_cases <- rbindlist(data_cases_list)
  # this is hardcoded for now - but is helpful to have
  date_df <- data.frame(
    daynum = seq(1,1000,1),
    date = seq(as.Date("2020-03-20"),as.Date("2020-03-20")+999,1)
  )
  # create HDF5 file input by user
  h5createFile(file = outfile)
  # write out input data to HDF5
  # first need to create a group for the inputs
  h5createGroup(file = outfile, group = "input")
  # declare input objects
  in_obj1_name <- "input/geo_df"
  in_obj2_name <- "input/data_cases"
  in_obj3_name <- "input/simdates"
  # write input objects  
  h5write(geo_df, file = outfile, name = in_obj1_name)
  h5write(data_cases, file = outfile, name = in_obj2_name)
  h5write(date_df, file = outfile, name = in_obj3_name)
  #create an output subgroup which stays empty for now
  h5createGroup(file = outfile, group = "output")
}