# colwise(df, "as.POSIXct")
# colwise(df, "as.POSIXct", x, y, z)
# colwise(df, "as.POSIXct", contains('date'))
# colwise(df, c("min", "mean", "max"), x)
# colwise(df, c("min", "mean", "max"), x, y, z)
# colwise(df, c("min", "mean", "max"), contains("date"))
# colwise(df, c("min", "mean", "max"))
# would be nice to supply additional ...

# This function is the workhorse. Powers all the others.

#' @examples
#' colwise(iris, "mean", dots(Species))
#' colwise(iris, c("min", "max"), dots(matches("Sepal")), na.rm = TRUE)
colwise <- function(tbl, funs, vars, ...) {
  vars <- lapply(select_vars(tbl_vars(tbl), vars, env = parent.frame()), as.name)
  funs <- lapply(funs, as.name)


  calls <- vector("list", length(vars) * length(funs))
  dim(calls) <- c(length(vars), length(funs))

  for (i in seq_along(vars)) {
    for (j in seq_along(funs)) {
      call <- substitute(`__f`(`__x`, ...))
      call[[1]] <- funs[[j]]
      call[[2]] <- vars[[i]]
      calls[[i, j]] <- call
    }
  }

  dim(calls) <- NULL
  calls
}

# colwise(iris, "mean", dots(x ))

