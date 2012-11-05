dots <- function() {
  call <- sys.call(-1)
  def <- eval(call[[1]], parent.frame(2))
  as.list(match.call(def, call, expand.dots = FALSE)$`...`)
}

names2 <- function(x) {
  names(x) %||% rep("", length(x))
}

"%||%" <- function(x, y) if(is.null(x)) y else x
