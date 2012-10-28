# system.time({
#   vars <- list(cyear = quote(year - min(year) + 1))
#   a <- mutate_by(baseball, group("id"), vars)
# })
# a <- a[order(baseball$id), , drop = FALSE]
#
# system.time(b <- ddply(baseball, "id", mutate, cyear = year - min(year) + 1))
#
# all.equal(a$cyear, b$cyear)
mutate_by <- function(data, groups, cols) {
  n <- nrow(data)
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

  for (i in seq_along(groups)) {
    rows <- groups[[i]]

    for (j in seq_len(p)) {
      if (i == 1L) {
        # Run mutate once to make vector of right type
        template <- eval(cols[[j]], grp)
        out[[j]] <- template[1]
        length(out[[j]]) <- n
        out[[j]][rows] <- template

        name <- names(cols)[[i]]
        makeActiveBinding(name, get_output(name), grp)
      } else {
        out[[j]][rows] <- eval(cols[[j]], grp)
      }
    }
  }

  # Coerce to data frame in place to avoid copying
  class(out) <- "data.frame"
  attr(out, "row.names") <- c(NA_integer_, -n)

  out
}

