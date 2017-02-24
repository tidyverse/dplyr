
expr_type_of <- function(x) {
  type <- typeof(x)
  if (type %in% c("symbol", "language", "pairlist", "NULL")) {
    type
  } else {
    "literal"
  }
}
switch_expr <- function(.x, ...) {
  switch(expr_type_of(.x), ...)
}

expr_walk_replace <- function(lsp, old, new) {
  while(!is_null(lsp)) {
    switch_expr(car(lsp),
      language = expr_walk_replace(cdar(lsp), old, new),
      symbol = if (identical(car(lsp), old)) set_car(lsp, new)
    )
    lsp <- cdr(lsp)
  }
}
expr_substitute <- function(expr, old, new) {
  expr <- duplicate(expr)
  switchpatch(expr,
    quote = ,
    language = expr_walk_replace(cdr(expr), old, new),
    symbol = if (identical(expr, old)) return(new)
  )
  expr
}
