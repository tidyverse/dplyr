match_vars <- function(vars, data) {
  if (is.numeric(vars)) return(vars)
  match(vars, names(data))
}
