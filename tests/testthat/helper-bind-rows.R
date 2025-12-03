local_df_subclass_bind_rows_method <- function(frame = caller_env()) {
  local_methods(
    .frame = frame,
    bind_rows.df_subclass = function(..., .id = NULL) {
      dispatched(...)
    }
  )
}

df_subclass <- function(...) {
  vctrs::new_data_frame(x = vctrs::df_list(...), class = "df_subclass")
}

dispatched <- function(...) {
  structure(list2(...), class = "dispatched")
}
