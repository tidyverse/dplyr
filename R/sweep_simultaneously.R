#' Sweep over several columns sweep_simultaneously.
#'
sweep_simultaneously <- function(df, margin, stat, fun, ...) {
  fun <- match.fun(fun)

  stat <- as.data.frame(t(as.data.frame(stat)))
  if (margin == 2) {
    return(mutate_each(df, funs(fun(., stat$.))))
  }
  else {
    result <- as.data.frame(t(mutate_each(as.data.frame(t(df)), funs(fun(., stat$.)))))
    setnames(result, colnames(df))
    return(result)
  }
}