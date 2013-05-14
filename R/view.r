#' data(baseball, package = "plyr")
#' players <- group_by(baseball, id)
#' v <- view(players$obj, players$index)
#' v$set_group(1)
#' v$eval(quote(id))
view <- function(data, index, parent = parent.frame()) {
  # Current group and rows
  i <- 1
  rows <- index[[1]]

  set_group <- function(value) {
    if (i == value) return()
    i <<- value
    rows <<- index[[value]]
    rows
  }

  # Tools to manage active bindings
  add_binding <- function(name, fun) {
    makeActiveBinding(name, fun, grp_env)
  }
  from_data <- function(col) {
    force(col)
    function(v) {
      if (!missing(v)) stop("Immutable view")
      .subset2(data, col)[rows]
    }
  }

  grp_env <- new.env(parent, size = nrow(data))
  for (name in names(data)) {
    add_binding(name, from_data(name))
  }

  local_eval <- function(expr) {
    eval(expr, grp_env)
  }

  list(set_group = set_group, eval = local_eval)
}
