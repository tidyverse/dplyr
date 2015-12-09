# A data frame with all major types
df_all <- data.frame(
  a = c(1, 2.5),
  b = 1:2,
  c = c(T, F),
  d = c("a", "b"),
  e = factor(c("a", "b")),
  f = as.Date("2015-12-09") + 1:2,
  g = as.POSIXct("2015-12-09 10:51:34 UTC") + 1:2,
  stringsAsFactors = FALSE
)
