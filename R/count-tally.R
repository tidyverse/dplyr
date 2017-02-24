#' Counts/tally observations by group.
#'
#' @description
#' `tally()` is a convenient wrapper for summarise that will either call
#' [n()] or \code{\link{sum}(n)} depending on whether you're tallying
#' for the first time, or re-tallying. `count()` is similar, but also
#' does the [group_by()] for you.
#'
#' `add_tally()` adds a column "n" to a table based on the number
#' of items within each existing group, while `add_count()` is a shortcut that
#' does the grouping as well. These functions are to [tally()]
#' and [count()] as [mutate()] is to [summarise()]:
#' they add an additional column rather than collapsing each group.
#'
#' @note
#' The column name in the returned data is usually `n`. If the data already
#' already has a column named `n` a lower-case n will be appended and the
#' column name returned will be `nn`.  Likewise, if the table already has
#' columns named `n` and `nn` then the column returned will be `nnn`, etc.
#'
#' @param x a [tbl()] to tally/count.
#' @param ...,vars Variables to group by.
#' @param wt (Optional) If omitted, will count the number of rows. If specified,
#'   will perform a "weighted" tally by summing the (non-missing) values of
#'   variable `wt`.
#' @param sort if `TRUE` will sort output in descending order of `n`
#' @return A tbl, grouped the same way as `x`.
#' @export
#' @examples
#' # tally() is short-hand for mutate()
#' mtcars %>% tally()
#' # count() is a short-hand for group_by() + tally()
#' mtcars %>% count(cyl)
#'
#' # add_tally() is short-hand for mutate()
#' mtcars %>% add_tally()
#' # add_count() is a short-hand for group_by() + add_tally()
#' mtcars %>% add_count(cyl)
#'
#' if (require("Lahman")) {
#' batting_tbl <- tbl_df(Lahman::Batting)
#'
#' # count and tally are designed so that you can call
#' # them repeatedly, each time rolling up a level of detail
#' plays_by_year <- batting_tbl %>% count(playerID, yearID, sort = TRUE)
#' plays_by_year
#' plays_by_year %>% count(yearID)
#' plays_by_year %>% count()
#'
#' # add_count() is useful for groupwise filtering
#' # e.g.: get only players with at least 1000 ABs
#' batting_tbl %>%
#'   add_count(playerID, wt = AB) %>%
#'   filter(n >= 1000)
#' }
tally <- function(x, wt, sort = FALSE) {
  if (missing(wt)) {
    if ("n" %in% names(x)) {
      message("Using n as weighting variable")
      wt <- quote(n)
    } else {
      wt <- NULL
    }
  } else {
    wt <- substitute(wt)
  }

  tally_(x, wt, sort = sort)
}

#' @export
#' @rdname tally
tally_ <- function(x, wt, sort = FALSE) {
  if (is.null(wt)) {
    n <- quote(n())
  } else {
    n <- lazyeval::interp(quote(sum(wt, na.rm = TRUE)), wt = wt)
  }

  n_name <- n_name(tbl_vars(x))
  out <- summarise_(x, .dots = setNames(list(n), n_name))

  if (!sort) {
    out
  } else {
    desc_n <- lazyeval::interp(quote(desc(n)), n = as.name(n_name))
    arrange_(out, desc_n)
  }
}

n_name <- function(x) {
  name <- "n"
  while (name %in% x) {
    name <- paste0(name, "n")
  }

  name

}

#' @export
#' @rdname tally
count <- function(x, ..., wt = NULL, sort = FALSE) {
  vars <- lazyeval::lazy_dots(...)
  wt <- substitute(wt)

  count_(x, vars, wt, sort = sort)
}

#' @export
#' @rdname tally
count_ <- function(x, vars, wt = NULL, sort = FALSE) {
  groups <- group_vars(x)

  x <- group_by_(x, .dots = vars, add = TRUE)
  x <- tally_(x, wt = wt, sort = sort)
  x <- group_by_(x, .dots = groups, add = FALSE)
  x
}

#' @rdname tally
#' @export
add_tally <- function(x, wt, sort = FALSE) {
  if (missing(wt)) {
    if ("n" %in% names(x)) {
      message("Using n as weighting variable")
      wt <- quote(n)
    } else {
      wt <- NULL
    }
  } else {
    wt <- substitute(wt)
  }
  add_tally_(x, wt, sort = sort)
}


#' @rdname tally
#' @export
add_tally_ <- function(x, wt = NULL, sort = FALSE) {
  g <- group_vars(x)
  if (is.null(wt)) {
    n <- quote(n())
  } else {
    n <- lazyeval::interp(quote(sum(wt, na.rm = TRUE)), wt = wt)
  }
  n_name <- n_name(tbl_vars(x))
  out <- mutate_(x, .dots = setNames(list(n), n_name))

  if (sort) {
    desc_n <- lazyeval::interp(quote(desc(n)), n = as.name(n_name))
    out <- arrange_(out, desc_n)
  }
  grouped_df(out, g)
}


#' @rdname tally
#' @export
add_count <- function(x, ..., wt = NULL, sort = FALSE) {
  vars <- lazyeval::lazy_dots(...)
  wt <- substitute(wt)
  add_count_(x, vars, wt, sort = sort)
}


#' @rdname tally
#' @export
add_count_ <- function(x, vars, wt = NULL, sort = FALSE) {
  g <- group_vars(x)
  grouped <- group_by_(x, .dots = vars, add = TRUE)

  out <- add_tally_(grouped, wt = wt, sort = sort)
  grouped_df(out, g)
}
