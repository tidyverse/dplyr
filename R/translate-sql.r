# Helper functionals used in multiple sql expressions
sql_infix <- function(op) {
  op <- toupper(op)
  function(x, y) sprintf("%s %s %s", x, op, y)
}
sql_unary <- function(f) {
  f <- toupper(f)
  function(x) sprintf("%s(%s)", f, x)
}
sql_atomic <- function(x) {
  if (is.character(x)) x <- str_c('"', x, '"')
  if (length(x) == 1) return(x)

  str_c("(", str_c(x, collapse = ", "), ")")
}

# Conversion between R and sql function
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

