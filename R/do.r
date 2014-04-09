#' Apply a function to a tbl
#'
#' This is a general purpose complement to the specialised manipulation
#' functions \code{\link{filter}}, \code{\link{select}}, \code{\link{mutate}},
#' \code{\link{summarise}} and \code{\link{arrange}}. You can use \code{do}
#' to perform arbitrary computation, returning either a data frame or
#' arbitrary objects which will be stored in a list.
#'
#' @param .data a tbl
#' @param ... Expressions to apply to each group. If named, results will be
#'   stored in a new column. If unnamed, should return a data frame. You can
#'   use \code{.} to refer to the current group. You can not mix named and
#'   unnamed arguments.
#' @return
#' \code{do} always returns a data frame. The first columns in the data frame
#' will be the labels, the others will be computed from \code{...}. Named
#' arguments become list-columns, with one row for each group; unnamed
#' elements will be converted to data frames (if needed) and labels will be
#' duplicated accordingly.
#'
#' Unlike \code{\link{summarise}} groups are preserved in input and output.
#' This is because while \code{summarise} reduces the complexity of the
#' data, \code{do} generally does not.
#' @export
#' @examples
#' by_cyl <- group_by(mtcars, cyl)
#' do(by_cyl, head(., 2))
#'
#' models <- by_cyl %.% do(mod = lm(mpg ~ disp, data = .))
#' models
#'
#' summarise(models, rsq = summary(mod[[1]])$r.squared)
#' models %.% do(data.frame(coef = coef(.$mod[[1]])))
#' models %.% do(data.frame(
#'   var = names(coef(.$mod[[1]])),
#'   coef(summary(.$mod[[1]])))
#' )
#'
#' models <- by_cyl %.% do(
#'   mod_linear = lm(mpg ~ disp, data = .),
#'   mod_quad = lm(mpg ~ poly(disp, 2), data = .)
#' )
#' models
#' compare <- models %.% do(aov = anova(.$mod_linear[[1]], .$mod_quad[[1]]))
#' compare
#' # Want to get to:
#' # compare <- models %.% mutate(aov = anova(mod_linear, .$mod_quad))
#' # compare %.% summarise(p.value = aov$`Pr(>F)`[2])
#'
#' if (require("hflights")) {
#' # You can use it to do any arbitrary computation, like fitting a linear
#' # model. Let's explore how carrier departure delays vary over the time
#' carriers <- group_by(hflights, UniqueCarrier)
#' group_size(carriers)
#'
#' mods <- do(carriers, mod = lm(ArrDelay ~ DepTime, data = .))
#' mods %.% do(as.data.frame(coef(.$mod[[1]])))
#' mods %.% summarise(rsq = summary(mod[[1]])$r.squared)
#'
#' \dontrun{
#' # This longer example shows the progress bar in action
#' by_dest <- hflights %.% group_by(Dest) %.% filter(n() > 100)
#' library(mgcv)
#' by_dest %.% do(smooth = gam(ArrDelay ~ s(DepTime) + Month, data = .))
#' }
#' }
do <- function(.data, ...) UseMethod("do")

#' @export
do.NULL <- function(.data, ...) {
  NULL
}

# Data frames -----------------------------------------------------------------

#' @export
do.data.frame <- function(.data, ...) {
  args <- dots(...)
  named <- named_args(args)

  env <- new.env(parent = parent.frame())
  env$. <- .data

  if (!named) {
    out <- eval(args[[1]], envir = env)
    if (!is.data.frame(out)) {
      stop("Result must be a data frame", call. = FALSE)
    }
  } else {
    out <- lapply(args, function(arg) list(eval(arg, envir = env)))
    names(out) <- names(args)
    attr(out, "row.names") <- .set_row_names(1L)
    # Use tbl_df to ensure safe printing of list columns
    class(out) <- c("tbl_df", "data.frame")
  }

  out
}

#' @export
do.grouped_df <- function(.data, ..., env = parent.frame()) {
  # Force computation of indices
  if (is.null(attr(.data, "indices"))) {
    .data <- grouped_df_impl(.data, attr(.data, "vars"),
      attr(.data, "drop") %||% TRUE)
  }

  args <- dots(...)
  named <- named_args(args)

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
  p <- Progress(n * m, min_time = 2)

  for (i in seq_len(m)) {
    for (`_j` in seq_len(n)) {
      out[[i]][`_j`] <- list(eval(args[[i]], env = env))
      p$tick()$show()
    }
  }

  if (!named) {
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

# Data tables ------------------------------------------------------------------

#' @export
do.grouped_dt <- function(.data, ...) {
  args <- dots(...)
  named <- named_args(args)

  env <- dt_env(.data, parent.frame())

  if (!named) {
    cols <- replace_sd(args[[1]])
  } else {
    args <- lapply(args, function(x) call("list", replace_sd(x)))
    cols <- as.call(c(quote(list), args))
  }
  call <- substitute(dt[, cols, by = vars], list(cols = cols))

  out <- eval(call, env)

  if (!named) {
    grouped_dt(out, groups(.data))
  } else {
    tbl_dt(out)
  }
}

# tbl_sql ----------------------------------------------------------------------

#' @export
#' @rdname do
#' @param .chunk_size The size of each chunk to pull into R. If this number is
#'   too big, the process will be slow because R has to allocate and free a lot
#'   of memory. If it's too small, it will be slow, because of the overhead of
#'   talking to the database.
do.tbl_sql <- function(.data, .f, ..., .chunk_size = 1e4L) {
  group_by <- .data$group_by
  if (is.null(group_by)) stop("No grouping", call. = FALSE)

  gvars <- seq_along(group_by)
  # Create data frame of labels.
  labels_tbl <- update(.data,
    select = group_by,
    order_by = NULL)
  labels <- as.data.frame(labels_tbl)

  # Create ungrouped data frame suitable for chunked retrieval
  names(group_by) <- paste0("GRP_", seq_along(group_by))
  chunky <- update(.data,
    select = c(group_by, .data$select),
    order_by = c(unname(group_by), .data$order_by),
    group_by = NULL
  )

  # When retrieving in pages, there's no guarantee we'll get a complete group.
  # So we always assume the last group in the chunk is incomplete, and leave
  # it for the next. If the group size is large than chunk size, it may
  # take a couple of iterations to get the entire group, but that should
  # be an unusual situation.
  last_group <- NULL
  out <- vector("list", nrow(labels))
  i <- 0

  chunky$query$fetch_paged(.chunk_size, function(chunk) {
    if (!is.null(last_group)) chunk <- rbind(last_group, chunk)

    # Create an id for each group
    group_id <- id(chunk[gvars], drop = TRUE)
    n <- attr(group_id, "n")

    index <- split_indices(group_id, n)
    last_group <<- chunk[index[[length(index)]], , drop = FALSE]

    for (j in seq_len(n - 1)) {
      subs <- chunk[index[[j]], , drop = FALSE]
      out[[i + j]] <<- .f(subs, ...)
    }
    i <<- i + (n - 1)
  })

  # Process last group
  if (!is.null(last_group)) {
    out[[i + 1]] <- .f(last_group, ...)
  }

  labels$DO <- out
  grouped_df(labels, drop_last(groups(.data)))
}


# Utils ------------------------------------------------------------------------

named_args <- function(args) {
  # Arguments must either be all named or all unnamed.
  named <- sum(names2(args) != "")
  if (!(named == 0 || named == length(args))) {
    stop("Arguments to do() must either be all named or all unnamed",
      call. = FALSE)
  }
  if (named == 0 && length(args) > 1) {
    stop("Can only supply single unnamed argument to do()", call. = FALSE)
  }

  named != 0
}

replace_sd <- function(x) {
  if (is.atomic(x)) {
    x
  } else if (is.name(x)) {
    if (identical(x, quote(.))) quote(.SD) else x
  } else if (is.pairlist(x)) {
    as.pairlist(lapply(x, replace_sd))
  } else if (is.call(x)) {
    as.call(lapply(x, replace_sd))
  } else {
    stop("Unknown input")
  }
}
