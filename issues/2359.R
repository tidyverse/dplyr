devtools::load_all()

mtcars_sqlite <- copy_to(src_sqlite(path = tempfile(), create = TRUE), mtcars)

# This works:
mtcars_sqlite %>%
  distinct(cyl) %>%
  collect()

# This doesn't:
mtcars_sqlite %>%
  distinct(cyl) %>%
  compute



# But it works if we explicitly select:
mtcars_sqlite %>%
  select(cyl) %>%
  distinct(cyl) %>%
  compute()
