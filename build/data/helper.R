print_time <- function(start, end, obj, proc) {
  x <- as.numeric(difftime(end, start, units = "secs"))
  print(paste(obj, proc, "time", sprintf(
    "%02d:%02d:%02d.%02d", 
    x %% 86400 %/% 3600, x %% 3600 %/% 60,  x %% 60 %/% 1, (x %% 1 * 100) %/% 1
  )))
}