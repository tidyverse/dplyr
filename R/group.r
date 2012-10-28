group <- function(x, env = parent.frame()) UseMethod("group")

#' @S3method group function
group.function <- function(x, env = parent.frame()) x

#' @importFrom plyr as.quoted
#' @S3method group default
group.default <- function(x, env = parent.frame()) as.quoted(x, env = env)

#' @importFrom plyr split_indices id eval.quoted
group_ids <- function(group, df) {
  if (is.function(group)) {
    splits <- group(df)
  } else {
    splits <- eval.quoted(group, df)
  }

  split_id <- id(splits)
  split_indices(as.integer(split_id), attr(split_id, "n"))
}

group_sql <- function(group, df) {
  if (is.function(group)) {
    stop("Arbitrary grouping functions not supported for SQL", call. = FALSE)
  }

  eval(translate_sql(group[[1]], names(df), group$env))
}
