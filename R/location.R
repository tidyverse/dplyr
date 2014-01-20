#' Print the location in memory of a data frame
#' 
#' This is useful for understand how and when dplyr makes copies of data 
#' frames
#' 
#' @param df, a data frame
#' @export
#' @examples
#' location(mtcars)
location <- function(df) {
  assert_that(is.data.frame(df))
  
  structure(list(
    df = loc(df),
    vars = dfloc(df),
    attr = plfloc(attributes(df))
  ), class = "location")
}

#' @export
print.location <- function(x, ...) {
  cat("<", x$df, ">\n", sep = "")
  
  width <- max(nchar(c(names(x$vars), names(x$attr)))) + 1
  def_list <- function(x) {
    term <- format(paste0(names(x), ":"), width = width)
    paste0(" * ", term, " <", format(x), ">")
  }
    
  vars <- paste0(def_list(x$vars), collapse = "\n")
  cat("Variables:\n", vars, "\n", sep = "")

  attr <- paste0(def_list(x$attr), collapse = "\n")
  cat("Attributes:\n", attr, "\n", sep = "")
}
