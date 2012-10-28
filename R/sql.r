# Need to do something when converting literal factors/dates
#
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

# http://www.sqlite.org/lang_corefunc.html
# http://www.sqlite.org/lang_datefunc.html
# http://www.sqlite.org/lang_aggfunc.html

sql_atomic <- function(x) UseMethod("sql_atomic")
sql_atomic.character <- function(x) {
  x <- str_c('"', x, '"')
  if (length(x) == 1) return(x)

  str_c("(", str_c(x, collapse = ", "), ")")
}


sql_mean <- function(x) {
  str_c("AVG(", x, ")")
}
sql_sum <- function(x) {
  str_c("TOTAL(", x, ")")
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
sql_lt <- function(x, y) {
  str_c(x, " < ", y)
}
sql_in <- function(x, y) {
  str_c(x, " IN ", y)
}

sql_c <- function(...) {
  sql_atomic(c(...))
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

#' @importFrom stringr str_detect str_c
sql_var <- function(x) {
  ok <- "^[a-zA-Z_][a-zA-Z0-9_]*$"

  escape <- !str_detect(x, ok) || toupper(x) %in% sql_keywords
  if (escape) {
    str_c('"', x, '"')
  } else {
    x
  }
}

sql_keywords <- c(
  "ABORT", "ACTION", "ADD", "AFTER", "ALL", "ALTER", "ANALYZE", "AND", "AS",
  "ASC", "ATTACH", "AUTOINCREMENT", "BEFORE", "BEGIN", "BETWEEN", "BY",
  "CASCADE", "CASE", "CAST", "CHECK", "COLLATE", "COLUMN", "COMMIT", "CONFLICT",
  "CONSTRAINT", "CREATE", "CROSS", "CURRENT_DATE", "CURRENT_TIME",
  "CURRENT_TIMESTAMP", "DATABASE", "DEFAULT", "DEFERRABLE", "DEFERRED", "DELETE",
  "DESC", "DETACH", "DISTINCT", "DROP", "EACH", "ELSE", "END", "ESCAPE",
  "EXCEPT", "EXCLUSIVE", "EXISTS", "EXPLAIN", "FAIL", "FOR", "FOREIGN", "FROM",
  "FULL", "GLOB", "GROUP", "HAVING", "IF", "IGNORE", "IMMEDIATE", "IN", "INDEX",
  "INDEXED", "INITIALLY", "INNER", "INSERT", "INSTEAD", "INTERSECT", "INTO",
  "IS", "ISNULL", "JOIN", "KEY", "LEFT", "LIKE", "LIMIT", "MATCH", "NATURAL",
  "NO", "NOT", "NOTNULL", "NULL", "OF", "OFFSET", "ON", "OR", "ORDER", "OUTER",
  "PLAN", "PRAGMA", "PRIMARY", "QUERY", "RAISE", "REFERENCES", "REGEXP",
  "REINDEX", "RELEASE", "RENAME", "REPLACE", "RESTRICT", "RIGHT", "ROLLBACK",
  "ROW", "SAVEPOINT", "SELECT", "SET", "TABLE", "TEMP", "TEMPORARY", "THEN",
  "TO", "TRANSACTION", "TRIGGER", "UNION", "UNIQUE", "UPDATE", "USING", "VACUUM",
  "VALUES", "VIEW", "VIRTUAL", "WHEN", "WHERE")
