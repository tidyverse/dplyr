#' Group input by rows
#'
#' \Sexpr[results=rd, stage=render]{dplyr:::lifecycle("questioning")}
#'
#' See [this repository](https://github.com/jennybc/row-oriented-workflows)
#' for alternative ways to perform row-wise operations.
#'
#' `rowwise()` is used for the results of [do()] when you
#' create list-variables. It is also useful to support arbitrary
#' complex operations that need to be applied to each row.
#'
#' Currently, rowwise grouping only works with data frames. Its
#' main impact is to allow you to work with list-variables in
#' [summarise()] and [mutate()] without having to
#' use \code{[[1]]}. This makes `summarise()` on a rowwise tbl
#' effectively equivalent to [plyr::ldply()].
#'
#' @param data Input data frame.
#' @export
#' @examples
#' df <- expand.grid(x = 1:3, y = 3:1)
#' df_done <- df %>% rowwise() %>% do(i = seq(.$x, .$y))
#' df_done
#' df_done %>% summarise(n = length(i))
rowwise <- function(data) {
  stopifnot(is.data.frame(data))

  structure(data, class = c("rowwise_df", "tbl_df", "tbl", "data.frame"))
}

setOldClass(c("rowwise_df", "tbl_df", "tbl", "data.frame"))

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
  ellipsis::check_dots_empty()
  class(x) <- c("tbl_df", "tbl", "data.frame")
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
group_by.rowwise_df <- function(.data, ..., add = FALSE, .drop = group_by_drop_default(.data)) {
  warn("Grouping rowwise data frame strips rowwise nature")
  .data <- ungroup(.data)

  groups <- group_by_prepare(.data, ..., add = add)
  grouped_df(groups$data, groups$group_names, .drop)
}

# Do ---------------------------------------------------------------------------

#' @export
do.rowwise_df <- function(.data, ...) {
  # Create ungroup version of data frame suitable for subsetting
  group_data <- ungroup(.data)

  args <- enquos(...)
  named <- named_args(args)

  # Create new environment, inheriting from parent, with an active binding
  # for . that resolves to the current subset. `_i` is found in environment
  # of this function because of usual scoping rules.
  mask <- new_data_mask(new_environment())
  current_row <- function() lapply(group_data[`_i`, , drop = FALSE], "[[", 1)
  env_bind_do_pronouns(mask, current_row)

  n <- nrow(.data)
  m <- length(args)

  out <- replicate(m, vector("list", n), simplify = FALSE)
  names(out) <- names(args)
  p <- progress_estimated(n * m, min_time = 2)

  for (`_i` in seq_len(n)) {
    for (j in seq_len(m)) {
      out[[j]][`_i`] <- list(eval_tidy(args[[j]], mask))
      p$tick()$print()
    }
  }

  if (!named) {
    label_output_dataframe(NULL, out, groups(.data), group_by_drop_default(.data))
  } else {
    label_output_list(NULL, out, groups(.data))
  }
}
