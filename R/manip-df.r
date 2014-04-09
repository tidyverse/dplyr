.data_dots <- function(fun, DOTS = dots){
  f <- function(.data, ...){}
  body(f) <- substitute({
    FUN(.data, DOTS(...), environment() )
  }, list( FUN = substitute(fun), DOTS = substitute(DOTS)))
  attr(f, "srcref") <- NULL
  f
}

#' Data manipulation for data frames.
#'
#' @param .data a data frame
#' @param ... variables interpreted in the context of \code{.data}
#' @examples
#' if (require("hflights")) {
#' filter(hflights, Month == 1, DayofMonth == 1, Dest == "DFW")
#' head(select(hflights, Year:DayOfWeek))
#' summarise(hflights, delay = mean(ArrDelay, na.rm = TRUE), n = length(ArrDelay))
#' head(mutate(hflights, gained = ArrDelay - DepDelay))
#' head(arrange(hflights, Dest, desc(ArrDelay)))
#' }
#' @name manip_df
NULL

#' @rdname manip_df
#' @export
arrange.tbl_df    <- .data_dots(arrange_impl)

#' @rdname manip_df
#' @export
filter.tbl_df    <- .data_dots(filter_impl)

integer_filter   <- .data_dots(integer_filter_impl)

#' @rdname manip_df
#' @export
mutate.tbl_df    <- .data_dots(mutate_impl, named_dots)

#' @rdname manip_df
#' @export
summarise.tbl_df <- .data_dots(summarise_impl, named_dots)

#' @rdname manip_df
#' @export
select.tbl_df <- function(.data, ...) {
  vars <- select_vars(names(.data), ..., env = parent.frame())
  select_impl(.data, vars)
}

#' @export
select.grouped_df <- function(.data, ...) {
  vars <- select_vars(names(.data), ..., env = parent.frame(),
    include = as.character(groups(.data)))

  select_impl(.data, vars)
}

# Other methods that currently don't have a better home -----------------------

order_ <- function(..., data){
  parent_frame <- parent.frame()
  if(missing(data)) {
    env <- parent_frame
  } else {
    env <- as.environment(data)
    parent.env(env) <- parent_frame
  }
  order_impl(dots(...) , env)
}

equal_ <- function(x, y){
  equal_data_frame(x, y)
}

all_equal_ <- function(...){
  env <- parent.frame()
  all_equal_data_frame(dots(...), env)
}

sort_ <- function(data){
  sort_impl(data)
}

rename_ <- .data_dots(rename_impl, named_dots)

#' @export
do.grouped_df <- function(.data, ..., env = parent.frame()) {
  # Force computation of indices
  if (is.null(attr(.data, "indices"))) {
    .data <- grouped_df_impl(.data, attr(.data, "vars"),
      attr(.data, "drop") %||% TRUE)
  }

  args <- dots(...)

  # Arguments must either be all named or all unnamed.
  named <- sum(names2(args) != "")
  if (!(named == 0 || named == length(args))) {
    stop("Arguments to do() must either be all named or all unnamed",
      call. = FALSE)
  }
  if (named == 0 && length(args) > 1) {
    stop("Can only supply single unnamed argument to do()", call. = FALSE)
  }

  labels <- attr(.data, "labels")
  index <- attr(.data, "indices")

  # Create new environment, inheriting from parent, with an active binding
  # for . that resolves to the current subset. `_i` is found in environment
  # of this function because of usual scoping rules.
  env <- new.env(parent = parent.frame())
  makeActiveBinding(".", function() {
    .data[index[[`_j`]] + 1L, , drop = FALSE]
  }, env)


  n <- length(index)
  m <- length(args)

  out <- replicate(m, vector("list", n), simplify = FALSE)
  p <- Progress(n * m, min_time = 5)

  for (i in seq_len(m)) {
    for (`_j` in seq_len(n)) {
      out[[i]][`_j`] <- list(eval(args[[i]], env = env))
      p$tick()$show()
    }
  }

  if (named == 0) {
    data_frame <- vapply(out[[1]], is.data.frame, logical(1))
    if (any(!data_frame)) {
      stop("Results are not data frames at positions: ",
        paste(which(!data_frame), collapse = ", "), call. = FALSE)
    }

    rows <- vapply(out[[1]], nrow, numeric(1))
    labels <- labels[rep(1:nrow(labels), rows), , drop = FALSE]
    rownames(labels) <- NULL

    out <- rbind_all(out[[1]])
    grouped_df(cbind(labels, out), groups(.data))
  } else {
    # Each result should be stored in a list
    labels[names(args)] <- out
    grouped_df(labels, groups(.data))
  }
}
