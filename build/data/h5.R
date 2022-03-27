source("build/data/helper.R")

object <- commandArgs(trailingOnly = T)[1]
size <- commandArgs(trailingOnly = T)[2]
file_geo <- paste0("data/", size, "/", "geo_df.RData")
file_h5 <- paste0("data/", size, "/", "savi.h5")

l <- list(
  input = list(expr = quote(h5_input(geo_df, file_h5))),
  lightpolys_list = list(group = "LightPolys"),
  prisys_list = list(group = "PriSys"),
  prisysout_list = list(group = "PriSys_Outage"),
  secsys_list = list(group = "SecSys")
)

if (identical(object, "input")) {
  if (file.exists(file_h5)) file.remove(file_h5)
  source("build/data/h5_input.R")
  load(file_geo)
  print(paste0("building h5 /", object, " group ..."))
  proc_start <- Sys.time()
  eval(l[[object]]$expr)
  proc_end <- Sys.time()
  print_time(proc_start, proc_end, paste0("/", object), "h5 convert")
}

if (! identical(object, "input")) {
  source("build/data/convert_to_h5.R")
  load(file_geo)
  load(paste0("data/", size, "/", object, ".RData"))
  print(paste0("building h5 /output/", l[[object]]$group, " group ..."))
  proc_start <- Sys.time()
  convert_to_h5(geo_df, get(object), l[[object]]$group, file_h5)
  proc_end <- Sys.time()
  print_time(proc_start, proc_end, paste0("/output/", l[[object]]$group), "h5 convert")
}
