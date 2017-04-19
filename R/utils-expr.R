
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

node_walk_replace <- function(node, old, new) {
  while(!is_null(node)) {
    switch_expr(node_car(node),
      language = node_walk_replace(node_cdar(node), old, new),
      symbol = if (identical(node_car(node), old)) mut_node_car(node, new)
    )
    node <- node_cdr(node)
  }
}
expr_substitute <- function(expr, old, new) {
  expr <- duplicate(expr)
  switch_type(expr,
    formula = ,
    language = node_walk_replace(node_cdr(expr), old, new),
    symbol = if (identical(expr, old)) return(new)
  )
  expr
}
