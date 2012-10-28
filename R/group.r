group <- function(x) UseMethod("group")

#' @S3method group function
group.function <- function(x) x

#' @importFrom plyr as.quoted
#' @S3method group default
group.default <- function(x) as.quoted(x)

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
