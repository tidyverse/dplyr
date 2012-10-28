library(plyr)

# Restrictions:
#  * summary functions must return single value
#  * types must be the same in all groups
#
# Need to benchmark with and without checks for broken assumptions.
# Compare to aggregate, and to data.table
#
# Test with:
#  * dates and factors
#  * variables that depend on previous
#
# system.time({
#   grp <- plyr:::split_indices(id(baseball["id"]))
#   vars <- list(n = quote(length(id)), m = quote(n + 1))
#   a <- summarise_by(baseball, grp, vars)
# })
#
# system.time(b <- ddply(baseball, "id", summarise, n = length(id)))
# stopifnot(all.equal(a$n, b$n))
# # ~20x slower
#
# system.time(count(baseball, "id"))
# # ~2x faster - in this case it's basically id + tabulate
# # so maybe able to eke out a little more with a C loop ?
#
# baseball2 <- data.table(baseball)
# system.time(baseball2[, list(n = length(year), m = n + 1), by = id])
# # ~ 0.007 - holy shit that's fast
# # but now only ~10x faster than summarise_by
# setkey(baseball2, id)
# system.time(baseball2[, length(year), by = id])
# # ~ 0.002 - even more insanely fast
summarise_by <- function(data, groups, cols) {
  n <- length(groups)
  p <- length(cols)

  out <- vector("list", p)
  names(out) <- names(cols)

  grp <- new.env(size = p, parent = parent.frame())
  get_input <- function(j) {
    force(j)
    function(v) {
      if (!missing(v)) stop("Immutable view")
      # equivalent to data[[j]][rows] but avoids costly S3 dispatch
      .subset2(data, j)[rows]
    }
  }
  get_output <- function(j) {
    force(j)
    function(v) {
      if (!missing(v)) stop("Immutable view")
      .subset2(out, j)[i]
    }
  }
  for (name in names(data)) {
    makeActiveBinding(name, get_input(name), grp)
  }

  for (i in seq_len(n)) {
    rows <- groups[[i]]

    for (j in seq_len(p)) {
      if (i == 1L) {
        # Run summarise once to make vector of right type
        out[[j]] <- eval(cols[[j]], grp)
        length(out[[j]]) <- n

        name <- names(cols)[[i]]
        makeActiveBinding(name, get_output(name), grp)
      } else {
        out[[j]][[i]] <- eval(cols[[j]], grp)
      }
    }
  }

  # Coerce to data frame in place to avoid copying
  class(out) <- "data.frame"
  attr(out, "row.names") <- c(NA_integer_, -n)

  out
}

