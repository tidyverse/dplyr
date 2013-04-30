op <- function(calls, subclass = "") {
  structure(list(calls = calls), class = c(subclass, "op"))
}

filters <- function(...) {
  op(dots(...), "filter_op")
}
subsets <- filters

arranges <- function(...) {
  op(dots(...), "arrange_op")
}
reorders <- arranges

transforms <- function(...) {
  op(dots(...), "transform_op")
}
mutates <- transforms

summarises <- function(...) {
  op(dots(...), "summarise_op")
}
summarizes <- summarises

ops <- function(ops) {
  structure(list(ops = ops), class = c("ops", "op"))
}
cops <- function(source, ops) {
  structure(list(source = source, ops = ops),
    class = c("cops", "ops", "op"))
}

is.op <- function(x) inherits(x, "op")
is.ops <- function(x) inherits(x, "ops")
is.complete_ops <- function(x) inherits(x, "complete_op")

abbr <- function(x) UseMethod("abbr")
abbr.cops <- function(x) "cops"
abbr.ops <- function(x) "ops"
abbr.op <- function(x) "op"
abbr.source <- function(x) "src"

print_calls <- function(name, x) {
  print(as.call(c(as.name(name), x$calls)))
}
print.filter_op <- function(x, ...) print_calls("filters", x)
print.arrange_op <- function(x, ...) print_calls("arranges", x)
print.summarise_op <- function(x, ...) print_calls("summarises", x)
print.transform_op <- function(x, ...) print_calls("transforms", x)
print.ops <- function(x, ...) for(op in x$ops) print(op)
print.cops <- function(x, ...) {
  print(x$source)
  print.ops(x)
}

"+.op" <- function(e1, e2) {
  stopifnot(is.op(e1))
  stopifnot(is.op(e2))

  op_op <-     function(e1, e2) ops(list(e1, e2))
  op_ops <-    function(e1, e2) ops(c(list(e1), e2$ops))
  op_cops <-   function(e1, e2) cops(e2$source, c(list(e1), e2$ops))
  op_src <-    function(e1, e2) cops(e2, list(e1))

  ops_ops <-   function(e1, e2) ops(c(e1$ops, e2$ops))
  ops_cops <-  function(e1, e2) cops(e2$source, c(e1$ops, e2$ops))
  ops_src <-   function(e1, e2) cops(e2, e1$ops)

  cops_cops <- function(e1, e2) cops(e2$source, c(e1$ops, e2$ops))
  cops_src <-  function(e1, e2) cops(e2$source, e1$ops)

  src_src <-   function(e1, e2) e2

  # Compute abbreviated names and reorder from simplest to most complex
  e <- list(e1, e2)
  abbr <- factor(c(abbr(e1), abbr(e2)), levels = c("op", "ops", "cops", "src"))

  ord <- order(abbr)
  e <- e[ord]
  abbr <- abbr[ord]

  method <- paste(abbr[1], abbr[2], sep = "_")
  f <- get(method, mode = "function", inherits = FALSE)
  f(e[[1]], e[[2]])
}

# baseball_s <- sqlite_source("inst/db/baseball.sqlite3", "baseball")
# subsets(year > 1960)
# subsets(year > 1960) + summarises(g = mean(g))
# subsets(year > 1960) + baseball_s
# (subsets(year > 1960) + summarises(g = mean(g))) + baseball_s
#
# baseball_s + subsets(year > 1960)
# baseball_s + (subsets(year > 1960) + summarises(g = mean(g)))
# (baseball_s + subsets(year > 1960)) + summarises(g = mean(g))
#
# (subsets(year > 1960) + mutates(cyear = year - min(year) - 1)) +
#  (arranges(cyear) + summarises(g = mean(g)))
