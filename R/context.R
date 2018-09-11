
context_env <- new_environment()
context_env[[".group_size"]] <- NULL
context_env[[".group_index"]] <- NULL

from_context <- function(what){
  context_env[[what]] %||% abort(glue("{expr} should only be called in a data context", expr = deparse(sys.call(-1))))
}
