library(dplyr)
library(magrittr)
library(data.table)

bind_rows(
  data.frame(a= 4, b= as.POSIXct("2016-01-01 01:00", tz= Sys.timezone()))
  ,data.frame(a= 4, b= as.POSIXct("2016-01-01 01:00"))
) %>% use_series(b)

bind_rows(
  data.frame(a= 4, b= as.POSIXct("2016-01-01 01:00")),
  data.frame(a= 4, b= as.POSIXct("2016-01-01 01:00", tz= Sys.timezone()))
) %>% use_series(b)

bind_rows(
  data.frame(a= 4, b= as.POSIXct("2016-01-01 01:00", tz= Sys.timezone()))
  ,data.frame(a= 4, b= as.POSIXct("2016-01-01 01:00", tz= Sys.timezone()))
) %>% use_series(b)

bind_rows(
  data.frame(a= 4, b= as.POSIXct("2016-01-01 01:00"))
  ,data.frame(a= 4, b= as.POSIXct("2016-01-01 01:00"))
) %>% use_series(b)

rbindlist(list(
  data.frame(a= 4, b= as.POSIXct("2016-01-01 01:00", tz= Sys.timezone()))
  ,data.frame(a= 4, b= as.POSIXct("2016-01-01 01:00"))
)) %>% use_series(b)

rbind(
  data.frame(a= 4, b= as.POSIXct("2016-01-01 01:00", tz= Sys.timezone()))
  ,data.frame(a= 4, b= as.POSIXct("2016-01-01 01:00"))
) %>% use_series(b)
