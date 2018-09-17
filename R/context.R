context_env <- new_environment()

from_context <- function(what){
  context_env[[what]] %||% abort(glue("{expr} should only be called in a data context", expr = deparse(sys.call(-1))))
}
