devtools::load_all()

memdb_frame(a = 1:3) %>% filter(a %in% 1:2)

ok <- 1:2
memdb_frame(a = 1:3) %>% filter(a %in% ok)
