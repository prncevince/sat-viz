source("build/data/helper.R")
source("build/data/savi_output.R")

object <- commandArgs(trailingOnly = T)[1]
size <- commandArgs(trailingOnly = T)[2]
file_geo <- paste0("data/", size, "/", "geo_df.RData")

l <- list(
  geo_df = list(
    message = "geo_df",
    expr = quote(
      build_geoID_data(size)
    )
  ),
  secsys_list = list(
    message = "secondary sys data",
    expr = quote(
      build_secsys_list(geo_df, build_secsys_data)
    )
  ),
  prisys_list = list(
    message = "primary sys data",
    expr = quote(
      build_prisys_list(geo_df, convert_to_seg(build_prisys_red_data()))
    )
  ),
  prisysout_list = list(
    message = "primary sys outage data",
    expr = quote(
      build_outage_list(
        geo_df, build_outage_data(convert_to_seg(build_prisys_red_data()))
      )
    )
  ),
  lightpolys_list = list(
    message = "light polygons data",
    expr = quote(
      build_light_polys_list(geo_df, build_light_polys())
    )
  )
)

print(paste("building", l[[object]]$message, "..."))
proc_start <- Sys.time()
if (file.exists(file_geo) & object != "geo_df") load(file_geo)
assign(object, eval(l[[object]]$expr))
proc_end <- Sys.time()
print_time(proc_start, proc_end, object, "build")

print(paste("saving", l[[object]]$message, "..."))
proc_start <- Sys.time()
dir.create(paste("data/", size, sep = ""), showWarnings = F)
save(list = object, file = paste0("data/", size, "/", object, ".RData"))
proc_end <- Sys.time()
print_time(proc_start, proc_end, object, "save")
