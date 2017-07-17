library(tidyverse)
library(R.matlab)
library(purrr)
library(lubridate)

# load data
d <- readMat("raw-data/cesm_rcp85.mat")
latlon <- readMat("raw-data/latlon.mat")

# functions
slice_time <- function(x, start_time, end_time, ref_start = "2005-01", ref_end = "2100-12") {
  start_period <- as.period(ymd(paste0(start_time, "-01")) - ymd(paste0(ref_start, "-01")))
  start_ind <- round(day(start_period)/31) + 1
  end_period <- as.period(ymd(paste0(end_time, "-01")) - ymd(paste0(ref_start, "-01")))
  end_ind <- round(day(end_period)/31) + 1
  return(map(x, ~ .x[ , , start_ind:end_ind]))
}

a <- slice_time(d, start_time = "2005-01", end_time = "2005-01")

h <- a[["Ld"]]



listed_matrix_to_df <- function(x) {
  
}


