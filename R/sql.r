sql_infix <- function(op) {
  op <- toupper(op)
  function(x, y) sprintf("%s %s %s", x, op, y)
}
sql_unary <- function(f) {
  f <- toupper(f)
  function(x) sprintf("%s(%s)", f, x)
}


#' @importFrom stringr str_detect str_c
escape_sql <- function(x) {
  if (x == "") return("")
  ok <- "^[a-zA-Z_][a-zA-Z0-9_]*$"

  escape <- !str_detect(x, ok) || toupper(x) %in% sql_keywords
  if (escape) {
    str_c('"', x, '"')
  } else {
    x
  }
}

sql_atomic <- function(x) {
  if (is.character(x)) x <- str_c('"', x, '"')
  if (length(x) == 1) return(x)

  str_c("(", str_c(x, collapse = ", "), ")")
}

sql <- list(
  local_value = function(...) sql_atomic(c(...)),
  remote_var = escape_sql,

  # Operators
  "&" = sql_infix("AND"),
  "|" = sql_infix("OR"),
  "!" =  sql_unary("NOT"),
  xor =  function(x, y) sprintf("%s OR %s AND NOT (%s AND %s)", x, y, x, y),

  # Arithmetic
  "+" =  sql_infix("+"),
  "-" =  sql_infix("-"),
  "*" =  sql_infix("*"),
  "/" =  sql_infix("/"),
  "%%" = sql_infix("%"),

  # Math
  "abs" = sql_unary("abs"),
  "round" = function(x, y = 0L) sprintf("ROUND(%s, %d)", x, y),

  # String
  "tolower" = sql_unary("lower"),
  "toupper" = sql_unary("upper"),
  "nchar" =   sql_unary("length"),

  # Date
  "strftime" = function(x, y) sprintf("STRFTIME(%s, %s)", x, y),

  # Logical comparison
  "<" =  sql_infix("<"),
  "<=" = sql_infix("<="),
  ">" =  sql_infix(">"),
  ">=" = sql_infix(">="),
  "!=" = sql_infix("!="),
  "==" = sql_infix("=="),

  # Aggregate functions
  "mean" =  sql_unary("AVG"),
  "count" = function(x = "*")  sprintf("COUNT(%s)", x),
  "min" =   sql_unary("MIN"),
  "max" =   sql_unary("MAX"),
  "sum" =   sql_unary("TOTAL"),
  "paste" = function(x, sep = ",") sprintf("GROUP_CONCAT(%s, %s)", x, sep),

  # Other special functions
  "(" = function(x) sprintf("(%s)", x),

  # Special sql functions
  "%in%" =   sql_infix("IN"),
  "%like%" = sql_infix("LIKE")
)
source_translator.source_sql <- function(x) sql

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

sql_vars <- function(vars) {
  nms <- names(vars)
  if (is.null(nms)) nms <- rep("", length(vars))
  nms <- vapply(nms, escape_sql, character(1))

  str_c(vars, ifelse(nms == "", "", " AS "), nms, collapse = ", ")
}
