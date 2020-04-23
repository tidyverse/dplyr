scoped_options(lifecycle_verbosity = "quiet")

verify_output("out/rows-d.txt", {
  data <- tibble(a = 1:3, b = letters[c(1:2, NA)], c = 0.5 + 0:2)
  data

  rows_insert(data, tibble(a = 4, b = "z"))
  rows_update(data, tibble(a = 2:3, b = "z"))
  rows_update(data, tibble(b = "z", a = 2:3), by = "a")
  rows_patch(data, tibble(a = 2:3, b = "z"))
  rows_upsert(data, tibble(a = 2:4, b = "z"))
  rows_delete(data, tibble(a = 2:4))
  rows_delete(data, tibble(a = 2:4, b = "b"))
  rows_delete(data, tibble(a = 2:4, b = "b"), by = c("a", "b"))
  rows_truncate(data)
})
