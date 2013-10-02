# Benchmark relatively simple join between postgre and sqlite
bench <- function(x) {
  force(x)
  replicate(3, system.time(collect(x)))[1:3, ]
}

hflights1 <- tbl(hflights_postgres(), "hflights")
hflights2 <- tbl(hflights_sqlite(), "hflights")

# A simple filter

jan1_united1 <- filter(hflights1, Month == 1 && DayofMonth == 1 && 
  UniqueCarrier == "UA")
jan1_united2 <- filter(hflights2, Month == 1 && DayofMonth == 1 && 
  UniqueCarrier == "UA")

bench(collect(jan1_united1))
bench(collect(jan1_united2))

# An aggregate

by_day1 <- group_by(hflights1, Month, DayofMonth)
by_day2 <- group_by(hflights2, Month, DayofMonth)

daily_flights1 <- summarise(by_day1, n = n(), delay = mean(ArrDelay))
daily_flights2 <- summarise(by_day2, n = n(), delay = mean(ArrDelay))

bench(daily_flights1)
bench(daily_flights2)
