library(dplyr)
d <- data_frame( x = rep(c(1,2), c(2,4)), y = 1:6, names = letters[1:6] )
d
res <- d %>% group_by(x) %>% summarise( y = list( setNames(y, names) ) ) %>% ungroup
res$y[[1]]
res$y[[2]]
names( res$y[[1]])
names( res$y[[2]])
