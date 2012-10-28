# system.time({
#   a <- subset_by(baseball, group("id"), quote(g == max(g)))
# })
#
# system.time(b <- ddply(baseball, "id", subset, g == max(g)))
#
# a <- arrange(a, id)
subset_by <- function(data, groups, call) {
  n <- length(groups)

  out <- rep(NA, nrow(data))

  grp <- new.env(size = ncol(data), parent = parent.frame())
  get_input <- function(j) {
    force(j)
    function(v) {
      if (!missing(v)) stop("Immutable view")
      # equivalent to data[[j]][rows] but avoids costly S3 dispatch
      .subset2(data, j)[rows]
    }
  }
  for (name in names(data)) {
    makeActiveBinding(name, get_input(name), grp)
  }

  for (i in seq_len(n)) {
    rows <- groups[[i]]

    r <- eval(call, grp)
    out[rows] <- r & !is.na(r)
  }

  data[out, ]
}

