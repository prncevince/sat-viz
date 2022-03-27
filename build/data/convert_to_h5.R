library(rhdf5)

#' save SAVi list .Rdata plotting data into HDF5 hierarchical format/type 
#' @param geo_df
#' @param input_list
#' @param outfile
convert_to_h5 <- function(geo_df, input_list, series_name, outfile) {
  # create the output groups and subgroups
  h5createGroup(file = outfile, group = paste("output/",series_name,sep=""))
  # loop through all geo locations, write data to HDF5 file
  for (i in 1:nrow(geo_df)) {
    # pull the current geoID - needs to be a string
    current_geo <- as.character(geo_df$geoID[i])
    # print(paste("writing current geoID ", current_geo, " to HDF5 file...", sep = ""))
    # declare output objects``
    out_obj_name <- paste("output/",series_name,"/", current_geo, sep = "")
    # write output objects
    h5write(input_list[[current_geo]], file = outfile, name = out_obj_name)
  }
}