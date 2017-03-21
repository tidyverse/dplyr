#' Group input by rows
#'
#' `rowwise()` is used for the results of [do()] when you
#' create list-variables. It is also useful to support arbitrary
#' complex operations that need to be applied to each row.
#'
#' Currently, rowwise grouping only works with data frames. Its
#' main impact is to allow you to work with list-variables in
#' [summarise()] and [mutate()] without having to
#' use `[[1]]`. This makes `summarise()` on a rowwise tbl
#' effectively equivalent to [plyr::ldply()].
#'
#' @param data Input data frame.
#' @export
#' @examples
#' df <- expand.grid(x = 1:3, y = 3:1)
#' df %>% rowwise() %>% do(i = seq(.$x, .$y))
#' .Last.value %>% summarise(n = length(i))
rowwise <- function(data) {
  stopifnot(is.data.frame(data))

  assert_all_white_list(data)
  structure(data, class = c("rowwise_df", "tbl_df", "tbl", "data.frame"))
}

#' @export
print.rowwise_df <- function(x, ..., n = NULL, width = NULL) {
  cat("Source: local data frame ", dim_desc(x), "\n", sep = "")
  cat("Groups: <by row>\n")
  cat("\n")
  print(trunc_mat(x, n = n, width = width))
  invisible(x)
}

#' @export
ungroup.rowwise_df <- function(x, ...) {
  class(x) <- c("tbl_df", "data.frame")
  x
}
#' @export
as.data.frame.rowwise_df <- function(x, row.names, optional, ...) {
  class(x) <- "data.frame"
  x
}

#' @export
group_size.rowwise_df <- function(x) {
  rep.int(1L, nrow(x))
}

#' @export
n_groups.rowwise_df <- function(x) {
  nrow(x)
}

#' @export
group_by.rowwise_df <- function(.data, ..., add = FALSE) {
  warn("Grouping rowwise data frame strips rowwise nature")
  .data <- ungroup(.data)

  groups <- group_by_prepare(.data, ..., add = add)
  grouped_df(groups$data, groups$group_names)
}
#' @export
group_by_.rowwise_df <- function(.data, ..., .dots = list(), add = FALSE) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  group_by(.data, !!! dots, add = add)
}


# Do ---------------------------------------------------------------------------

#' @export
do.rowwise_df <- function(.data, ...) {
  # Create ungroup version of data frame suitable for subsetting
  group_data <- ungroup(.data)
  index <- attr(.data, "indices")

  args <- dots_quosures(...)
  named <- named_args(args)

  # Create new environment, inheriting from parent, with an active binding
  # for . that resolves to the current subset. `_i` is found in environment
  # of this function because of usual scoping rules.
  env <- child_env(NULL)
  current_row <- function() lapply(group_data[`_i`, , drop = FALSE], "[[", 1)
  env_assign_active(env, ".", current_row)
  env_assign_active(env, ".data", current_row)

  overscope <- new_overscope(env)
  on.exit(overscope_clean(overscope))

  n <- nrow(.data)
  m <- length(args)

  out <- replicate(m, vector("list", n), simplify = FALSE)
  names(out) <- names(args)
  p <- progress_estimated(n * m, min_time = 2)

  for (`_i` in seq_len(n)) {
    for (j in seq_len(m)) {
      out[[j]][`_i`] <- list(overscope_eval(overscope, args[[j]]))
      p$tick()$print()
    }
  }

  if (!named) {
    label_output_dataframe(NULL, out, groups(.data))
  } else {
    label_output_list(NULL, out, groups(.data))
  }
}
#' @export
do_.rowwise_df <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  do(.data, !!! dots)
}
