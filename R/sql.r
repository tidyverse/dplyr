# Difference between mutate and summarise: in summarise, no original column
#  names in FROM - all aggregated
#
# For sql:
#
# y = var(x) -> sql_var(quote(x)) -> VAR(X) as y
#
# a <- 1:5
# id %in% a -> sql_in(quote(id), a) -> id IN (1, 2, 3, 4, 5)
#
# x == y && x < 4 -> sql_and(sql_eq(quote(x), quote(y)), sql_lt(quote(x), 4))
#
# median(x) -> NO SQL EQUIV
#


sql_var <- function(x) {

  x
}

sql_mean <- function(x) {
  str_c("MEAN(", x, ")")
}
sql_sum <- function(x) {
  str_c("SUM(", x, ")")
}

sql_and <- function(x, y) {
  str_c(x, " AND ", y)
}
sql_or <- function(x, y) {
  str_c(x, " OR ", y)
}
sql_not <- function(x) {
  str_c("NOT", x)
}
sql_parens <- function(x) {
  str_c("(", x, ")")
}
sql_eq <- function(x, y) {
  str_c(x, " == ", y)
}
sql_gt <- function(x, y) {
  str_c(x, " > ", y)
}

trans_name <- function(symbol, type) {
  x <- as.character(symbol)
  if (x %in% names(mappings)) x <- mappings[[x]]

  as.name(str_c(type, "_", x))
}
mappings <- c(
  # Logical operators
  "==" = "eq",
  "!=" = "neq",
  "<" = "lt",
  ">" = "gt",
  "<=" = "lte",
  ">=" = "gte",

  # Boolean comparison
  "&&" = "and",
  "||" = "or",
  "!" = "not",

  # Numerical
  "+" = "plus",
  "%%" = "mod",

  # Misc
  "%in%" = "in",
  "(" = "parens"
)
