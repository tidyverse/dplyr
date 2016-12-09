#' Add a column counting or tallying observations within groups
#'
#' \code{add_tally} adds a column "n" to a table based on the number
#' of items within each existing group, while \code{add_count} is a shortcut that
#' does the grouping as well. These functions are to \code{\link{tally}}
#' and \code{\link{count}} as \code{\link{mutate}} is to \code{\link{summarise}}:
#' they add an additional column rather than collapsing each group.
#'
#' @param x a \code{tbl}.
#' @param wt (Optional) If omitted, will count the number of rows. Otherwise,
#' performs a weighted tally by summing the (non-missing) values of variable wt
#' @param sort Whether to sort the result in descending order of n
#' @param ...,vars Variables to group by.
#'
#' @details \code{add_count} counts within the current groups of the data when present,
#' and preserves those groups (it does not add the new ones).
#'
#' @examples
#'
#' add_tally(mtcars)
#' add_tally(group_by(mtcars, cyl))
#' add_tally(group_by(mtcars, cyl), sort = TRUE)
#'
#' add_count(mtcars, cyl)
#' add_count(mtcars, cyl, am)
#'
#' if (require("Lahman")) {
#' batting_tbl <- tbl_df(Batting)
#'
#' # get records of players who played in multiple stints in the same year
#' batting_tbl %>%
#'   add_count(playerID, yearID) %>%
#'   filter(n > 1)
#'
#' # get only players who played in more than three stints total
#' batting_tbl %>%
#'   add_count(playerID) %>%
#'   filter(n > 3)
#'
#' # get only players with at least 1000 ABs
#' batting_tbl %>%
#'   add_count(playerID, wt = AB) %>%
#'   filter(n >= 1000)
#' }
#'
#' @export
add_tally <- function(x, wt, sort = FALSE) {
  if (missing(wt)) {
    if ("n" %in% names(x)) {
      message("Using n as weighting variable")
      wt <- quote(n)
    }
    else {
      wt <- NULL
    }
  }
  else {
    wt <- substitute(wt)
  }
  add_tally_(x, wt, sort = sort)
}


#' @rdname add_tally
#' @export
add_tally_ <- function(x, wt = NULL, sort = FALSE) {
  g <- groups(x)
  if (is.null(wt)) {
    n <- quote(n())
  }
  else {
    n <- lazyeval::interp(quote(sum(wt, na.rm = TRUE)), wt = wt)
  }
  n_name <- n_name(tbl_vars(x))
  out <- mutate_(x, .dots = setNames(list(n), n_name))

  if (sort) {
    desc_n <- lazyeval::interp(quote(desc(n)), n = as.name(n_name))
    out <- arrange_(out, desc_n)
  }
  group_by_(out, .dots = g)
}


#' @rdname add_tally
#' @export
add_count <- function(x, ..., wt = NULL, sort = FALSE) {
  vars <- lazyeval::lazy_dots(...)
  wt <- substitute(wt)
  add_count_(x, vars, wt, sort = sort)
}


#' @rdname add_tally
#' @export
add_count_ <- function(x, vars, wt = NULL, sort = FALSE) {
  g <- groups(x)
  grouped <- group_by_(x, .dots = vars, add = TRUE)

  ret <- add_tally_(grouped, wt = wt, sort = sort)
  group_by_(ret, .dots = g)
}
