#' Sweep over several columns sweep_simultaneously.
#'
sweep_simultaneously <- function(df, margin, stat, fun, ...) {
  stat <- as.data.frame(t(as.data.frame(stat)))
  if (margin == 2) {
    return(mutate_each(df, funs(fun(., stat$.))))
  }
  else {
    cn <- colnames(df)
    result <- as.data.frame(t(mutate_each(as.data.frame(t(df)), funs(fun(., stat$.)))))
    colnames(result) <- cn
    return(result)
  }
}