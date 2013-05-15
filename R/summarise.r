# Test with:
#  * dates and factors
#  * variables that depend on previous

do_summarise_by.source_data_table <- function(source, group, calls,
                                           env = parent.frame()) {
  by_call <- as.call(c(quote(list), group))
  list_call <- as.call(c(quote(list), calls))

  dt_call <- substitute(source$obj[, calls, by = by],
    list(calls = list_call, by = by_call))
  eval(dt_call)
}
