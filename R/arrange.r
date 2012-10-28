# system.time({
#   a <- arrange_by(baseball, group("id"), quote(g))
# })
# a <- arrange(a, id)
# system.time(b <- ddply(baseball, "id", arrange, g))
arrange_by <- function(data, groups, call) {
  n <- length(groups)

  out <- rep(NA_integer_, nrow(data))
  order_call <- as.call(c(list(quote(order)), call))

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

    ord <- eval(order_call, grp)
    out[rows] <- rows[ord]
  }

  data[out, ]
}
