# Test with:
#  * dates and factors
#  * variables that depend on previous

# User friendly version.  Needs a different name.
#
# Restrictions:
#  * summary functions must return single value
#  * types must be the same in all groups
summarise_by <- function(.source, .group, ..., .env = parent.frame()) {
  if (is.data.frame(.source)) {
    source <- source_data_frame(.source, deparse(substitute(.source)))
  }

  if (!is.quoted(.group)) {
    .group <- as.quoted(.group, env = .env)
  }

  calls <- dots()
  do_summarise_by(.source, .group, calls, env = .env)
}

do_summarise_by <- function(source, group, calls, env = parent.frame()) {
  UseMethod("do_summarise_by")
}

do_summarise_by.source_sqlite <- function(source, group, calls, env = parent.frame()) {
  select <- vapply(calls, translate, source = source, env = env,
    FUN.VALUE = character(1))
  group_by <- vapply(group, translate, source = source, env = env,
    FUN.VALUE = character(1))

  sql <- str_c("SELECT ", sql_vars(c(group_by, select)), "\n",
    "FROM ", escape_sql(source_name(source)), "\n",
    "GROUP BY ", sql_vars(group_by), ";"
  )

  qry <- dbSendQuery(source$con, sql)
  on.exit(dbClearResult(qry))

  fetch(qry, -1)
}

do_summarise_by.source_data_table <- function(source, group, calls,
                                           env = parent.frame()) {
  by_call <- as.call(c(quote(list), group))
  list_call <- as.call(c(quote(list), calls))

  dt_call <- substitute(source$obj[, calls, by = by],
    list(calls = list_call, by = by_call))
  eval(dt_call)
}

do_summarise_by.source_data_frame <- function(source, group, calls, env = parent.frame()) {
  data <- source$obj
  groups <- group_ids(group, data)

  n <- length(groups)
  p <- length(calls)

  out <- vector("list", p)
  names(out) <- names(calls)

  grp <- new.env(size = p, parent = parent.frame())
  get_input <- function(j) {
    force(j)
    function(v) {
      if (!missing(v)) stop("Immutable view")
      # equivalent to data[[j]][rows] but avoids costly S3 dispatch
      .subset2(data, j)[rows]
    }
  }
  get_output <- function(j) {
    force(j)
    function(v) {
      if (!missing(v)) stop("Immutable view")
      .subset2(out, j)[i]
    }
  }
  for (name in names(data)) {
    makeActiveBinding(name, get_input(name), grp)
  }

  for (i in seq_len(n)) {
    rows <- groups[[i]]

    for (j in seq_len(p)) {
      if (i == 1L) {
        # Run summarise once to make vector of right type
        out[[j]] <- eval(calls[[j]], grp)
        length(out[[j]]) <- n

        name <- names(calls)[[i]]
        ## Need to think about enabling this: what happens when you do
        ## x = mean(x)
        # makeActiveBinding(name, get_output(name), grp)
      } else {
        out[[j]][[i]] <- eval(calls[[j]], grp)
      }
    }
  }

  # Coerce to data frame in place to avoid copying
  class(out) <- "data.frame"
  attr(out, "row.names") <- c(NA_integer_, -n)

  out
}

