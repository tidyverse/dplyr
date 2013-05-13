#' Generate a standardised ops
standard_ops <- function(source = NULL, select = NULL, filter = NULL,
                         arrange = NULL, transform = NULL, summarise = NULL) {

  if (length(summarise) > 0 && length(transform) > 0) {
    stop("Single op may either summarise or transform, not both")
  }

  structure(
    class = "standard_ops",
    list(
      source = source,
      ops = list(
        select = select,
        filter = filter,
        arrange = arrange,
        transform = transform,
        summarise = summarise
      )
    )
  )
}
is.standard_ops <- function(x) inherits(x, "standard_ops")

# Collapse ops into a single object.
# Currently assumes that no splitting and combing occurs.
standardise <- function(x) {
  assert_that(is.ops(x))

  collapse <- function(x) {
    if (length(x) == 0) return(NULL)
    unlist(lapply(x, "[[", "calls"), recursive = FALSE, use.names = FALSE)
  }

  select <- collapse(Filter(is.select, x$ops))
  filter <- collapse(Filter(is.filter, x$ops))
  arrange <- collapse(Filter(is.arrange, x$ops))
  summarise <- collapse(Filter(is.summarise, x$ops))
  transform <- collapse(Filter(is.transform, x$ops))

  standard_ops(x$source, select = select, filter = filter, arrange = arrange,
    summarise = summarise, transform = transform)
}

#' @S3method as.data.frame standard_ops
as.data.frame.standard_ops <- function(x, ..., env = parent.frame()) {
  if (is.null(x$source)) {
    stop("No source data set present", call. = FALSE)
  }

  render(x$source, x$ops, ..., env = env)
}

render <- function(source, ..., env = env) {
  UseMethod("render")
}

# baseball_s <- sqlite_source("inst/db/baseball.sqlite3", "baseball")
# subsets(year > 1960)
# subsets(year > 1960) + summarises(g = mean(g))
# subsets(year > 1960) + baseball_s
# (subsets(year > 1960) + summarises(g = mean(g))) + baseball_s
#
# baseball_s + subsets(year > 1960)
# baseball_s + (subsets(year > 1960) + summarises(g = mean(g)))
# (baseball_s + subsets(year > 1960)) + summarises(g = mean(g))
#
# (subsets(year > 1960) + mutates(cyear = year - min(year) - 1)) +
#  (arranges(cyear) + summarises(g = mean(g)))
