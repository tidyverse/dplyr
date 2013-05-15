#' A grouped data frame.
#'
#' The easiest way to create a grouped data frame is to call the \code{group_by}
#' method on a data frame or data source: this will take care of capturing
#' the unevalated expressions for you.
#'
#' @param data a data source or data frame.
#' @param vars a list of quoted variables.
#' @param lazy if \code{TRUE}, index will be computed lazily every time it
#'   is needed. If \code{FALSE}, index will be computed up front on object
#'   creation.
#' @param drop if \code{TRUE} preserve all factor levels, even those without
#'   data.
#' @param name data source name.
grouped_df <- function(data, vars, lazy = TRUE, drop = TRUE,
                               name = NULL) {
  if (is.null(name)) {
    if (is.source(data)) {
      name <- data$name
    } else {
      name <- deparse(substitute(data))
    }
  }

  if (is.source(data)) {
    data <- data$obj
  }

  data <- list(obj = data, name = name, vars = vars, drop = drop)
  if (!lazy) {
    data <- build_index(data)
  }

  structure(data, class = c("grouped_df", "source_df", "source"))
}

#' @rdname grouped_df
#' @method is.lazy grouped_df
#' @export
is.lazy.grouped_df <- function(x) {
  is.null(x$index) || is.null(x$labels)
}

#' @rdname grouped_df
#' @export
is.grouped_df <- function(x) inherits(x, "grouped_df")

#' @S3method print grouped_df
print.grouped_df <- function(x, ...) {
  cat("Source:     local object\n", sep = "")
  cat("Data frame: ", dQuote(x$name), dim_desc(x), "\n", sep = "")
  cat("Groups: ", paste0(deparse_all(x$vars), collapse = ", "), "\n", sep = "")
  cat("\n")

  trunc_mat(x$obj)
}

#' @method group_by data.frame
#' @export
#' @rdname grouped_df
group_by.data.frame <- function(x, ..., drop = TRUE, name = NULL) {
  name <- name %||% substitute(x)
  vars <- named_dots(...)

  grouped_df(x, vars, lazy = FALSE, name = name)
}

#' @S3method ungroup grouped_df
ungroup.grouped_df <- function(x) {
  source_df(x$obj, x$name)
}

make_view <- function(x, env = parent.frame()) {
  if (is.lazy(x)) stop("No index present", call. = FALSE)
  view(x$obj, x$index, parent.frame())
}

build_index <- function(x) {
  splits <- lapply(x$vars, eval, x$obj, parent.frame())
  split_id <- id(splits, drop = x$drop)

  x$labels <- split_labels(splits, drop = x$drop, id = split_id)
  x$index <- split_indices(split_id, attr(split_id, "n"))

  x
}

split_labels <- function(splits, drop, id = plyr::id(splits, drop = TRUE)) {
  if (length(splits) == 0) return(data.frame())

  if (drop) {
    # Need levels which occur in data
    representative <- which(!duplicated(id))[order(unique(id))]
    as_df(lapply(splits, function(x) x[representative]))
  } else {
    unique_values <- llply(splits, ulevels)
    names(unique_values) <- names(splits)
    rev(expand.grid(rev(unique_values), stringsAsFactors = FALSE))
  }
}
