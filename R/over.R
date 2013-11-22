# Purely for SQL generation: to hard to efficiently do in R.
over <- function(expr, partition = NULL, order = NULL, frame = NULL) {
  args <- !is.null(partition) + !is.null(order) + !is.null(frame)
  if (args == 0) {
    stop("Must supply at least one of partition, order, frame", call. = FALSE)
  }
  
  if (!is.null(partition)) {
    partition <- paste0("PARTITION BY ", paste0(partition, collapse = ", "))
  }
  if (!is.null(order)) {
    order <- paste0("ORDER BY ", paste0(order, collapse = ", "))
  }
  if (!is.null(rows)) {
    if (is.numeric(frame)) frame <- rows(frame[1], frame[2])
    rows <- paste0("ROWS ", frame)
  }
  
  over <- paste0(c(partition, order, rows), collapse = " ")
  
  paste0(expr, " OVER (", over, ")")
}

rows <- function(from = -Inf, to = 0) {
  if (from >= to) stop("from must be less than to", call. = FALSE)
  
  dir <- function(x) if (x < 0) "PRECEDING" else "FOLLOWING"
  val <- function(x) if (is.finite(x)) as.integer(abs(x)) else "UNBOUNDED"
  bound <- function(x) {
    if (x == 0) return("CURRENT ROW")
    paste(val(x), dir(x))
  }

  if (to == 0) {
    bound(from)
  } else {
    paste0("BETWEEN ", bound(from), " AND ", bound(to))
  }
}
