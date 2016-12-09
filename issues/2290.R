devtools::load_all(".")
mtcars2 <- copy_to(src_postgres(), mtcars, dplyr:::random_table_name())
mtcars2 %>%
  group_by(cyl) %>%
  arrange(disp) %>%
  summarize(mpg2 = first(mpg))
