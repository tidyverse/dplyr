devtools::load_all(".")
  src <- src_mysql("test", user = "muelleki")
  src <- src_sqlite(":memory:", create = TRUE)
  name <- dplyr:::random_table_name()
  DBI::dbWriteTable(src$con, name, data_frame(a = 2:5))
  data <- src %>% tbl(name)
  data %>%
    mutate(b = log(a), c = log(exp(1), a)) %>%
    mutate(d = b * c)
