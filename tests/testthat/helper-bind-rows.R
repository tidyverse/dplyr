df_subclass <- function(...) {
  vctrs::new_data_frame(x = vctrs::df_list(...), class = "df_subclass")
}

dispatched <- function(...) {
  structure(list2(...), class = "dispatched")
}
