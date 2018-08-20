
#' @export
hybrid_call <- function(.data, expr){
  UseMethod("hybrid_call")
}

#' @export
hybrid_call.data.frame <- function(.data, expr){
  hybrid_impl(.data, enquo(expr))
}

#' @export
print.hybrid_call <- function(x, ...){
  if(isTRUE(x)){
    cat("<hybrid evaluation>\n")
    cat( "  call      : ")
    print(attr(x, "call"))
    cat( "  C++ class :", attr(x, "cpp_class"))
  } else {
    cat("<standard evaluation>\n")
    cat( "  call      : ")
    print(attr(x, "call"))
  }
}
