
group_map <- function(.tbl, .f, ..., env = caller_env()) {
  .f <- rlang::as_function(.f, env = env)
  .datas <- group_split(.tbl)
  .keys  <- group_split(rowwise(select(group_data(.tbl), - last_col())))

  bind_rows(map2(.datas, .keys, .f, ...))
}
