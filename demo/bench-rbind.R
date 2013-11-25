library(microbenchmark)
options(digits = 3)

one <- mtcars[1:10, ]
two <- mtcars[11:32, ]
l <- list(one, two)

microbenchmark(
  rbind(one, two),
  plyr::rbind.fill(one, two),
  dplyr::rbind_list(one, two),

  do.call("rbind", l),
  plyr::rbind.fill(l),
  dplyr::rbind_all(l),
  data.table::rbindlist(l)
)


