#' Sweep over several rows or columns sweep_simultaneously.
#'
#' @param df object to sweep.
#' @param margin dimention to be swept over (1 indicates rows, 2 indicates columns).
#' @param stat the summary statistic which is to be swept out.
#' @param fun the function to be applied.
#' @param ... optional arguments to fun.
sweep_simultaneously <- function(df, margin, stat, fun, ...) {
  fun <- match.fun(fun)
  args <- lazyeval::lazy_dots(...)

  stat <- as.data.frame(t(as.data.frame(stat)))
  if (margin == 2) {
    if(length(args) == 0) {
      return(mutate_each(df, funs(fun(., stat$.))))
    } else {
      return(mutate_each(df, funs(fun(., stat$., ...))))
    }
  }
  else {
    if(length(args) == 0) {
      result <- as.data.frame(t(mutate_each(as.data.frame(t(df)), funs(fun(., stat$.)))))
    } else {
      result <- as.data.frame(t(mutate_each(as.data.frame(t(df)), funs(fun(., stat$., ...)))))
    }
    setnames(result, colnames(df))
    return(result)
  }
}