star <- function() quote(`*`)
has_star <- function(x) any_apply(x, is.star)
is.star <- function(x) identical(x, star())
remove_star <- function(x) {
  if (is.null(x)) return(x)
  
  is_star <- vapply(x, is.star, logical(1))  
  x[!is_star]
}

expand_star <- function(x, tbl) {
  if (!has_star(x)) return(x)
  
  c(remove_star(x), lapply(colnames(tbl), as.name))
}