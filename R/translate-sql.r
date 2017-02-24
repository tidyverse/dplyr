#' Translate an expression to sql.
#'
#' @section Base translation:
#' The base translator, `base_sql`,
#' provides custom mappings for `!` (to NOT), `&&` and `&` to
#' `AND`, `||` and `|` to `OR`, `^` to `POWER`,
#' \code{\%>\%} to \code{\%}, `ceiling` to `CEIL`, `mean` to
#' `AVG`, `var` to `VARIANCE`, `tolower` to `LOWER`,
#' `toupper` to `UPPER` and `nchar` to `LENGTH`.
#'
#' `c()` and `:` keep their usual R behaviour so you can easily create
#' vectors that are passed to sql.
#'
#' All other functions will be preserved as is. R's infix functions
#' (e.g. \code{\%like\%}) will be converted to their SQL equivalents
#' (e.g. `LIKE`). You can use this to access SQL string concatenation:
#' `||` is mapped to `OR`, but \code{\%||\%} is mapped to `||`.
#' To suppress this behaviour, and force errors immediately when dplyr doesn't
#' know how to translate a function it encounters, using set the
#' `dplyr.strict_sql` option to `TRUE`.
#'
#' You can also use [sql()] to insert a raw sql string.
#'
#' @section SQLite translation:
#' The SQLite variant currently only adds one additional function: a mapping
#' from `sd()` to the SQL aggregation function `STDEV`.
#'
#' @param ...,dots Expressions to translate. `sql_translate()`
#'   automatically quotes them for you.  `sql_translate_()` expects
#'   a list of already quoted objects.
#' @param con An optional database connection to control the details of
#'   the translation. The default, `NULL`, generates ANSI SQL.
#' @param vars Deprecated. Now call [partial_eval()] directly.
#' @param vars_group,vars_order Grouping and ordering variables used for
#'   windowed functions.
#' @param window Use `FALSE` to suppress generation of the `OVER`
#'   statement used for window functions. This is necessary when generating
#'   SQL for a grouped summary.
#' @export
#' @examples
#' # Regular maths is translated in a very straightforward way
#' translate_sql(x + 1)
#' translate_sql(sin(x) + tan(y))
#'
#' # Note that all variable names are escaped
#' translate_sql(like == "x")
#' # In ANSI SQL: "" quotes variable _names_, '' quotes strings
#'
#' # Logical operators are converted to their sql equivalents
#' translate_sql(x < 5 & !(y >= 5))
#' # xor() doesn't have a direct SQL equivalent
#' translate_sql(xor(x, y))
#'
#' # If is translated into case when
#' translate_sql(if (x > 5) "big" else "small")
#'
#' # Infix functions are passed onto SQL with % removed
#' translate_sql(first %like% "Had%")
#' translate_sql(first %is% NULL)
#' translate_sql(first %in% c("John", "Roger", "Robert"))
#'
#' # And be careful if you really want integers
#' translate_sql(x == 1)
#' translate_sql(x == 1L)
#'
#' # If you have an already quoted object, use translate_sql_:
#' x <- quote(y + 1 / sin(t))
#' translate_sql_(list(x))
#'
#' # Windowed translation --------------------------------------------
#' # Known window functions automatically get OVER()
#' translate_sql(mpg > mean(mpg))
#'
#' # Suppress this with window = FALSE
#' translate_sql(mpg > mean(mpg), window = FALSE)
#'
#' # vars_group controls partition:
#' translate_sql(mpg > mean(mpg), vars_group = "cyl")
#'
#' # and vars_order controls ordering for those functions that need it
#' translate_sql(cumsum(mpg))
#' translate_sql(cumsum(mpg), vars_order = "mpg")
translate_sql <- function(...,
                          con = NULL,
                          vars = character(),
                          vars_group = NULL,
                          vars_order = NULL,
                          window = TRUE) {
  if (!missing(vars)) {
    abort("`vars` is deprecated. Please use partial_eval() directly.")
  }

  translate_sql_(
    tidy_dots(...),
    con = con,
    vars_group = vars_group,
    vars_order = vars_order,
    window = window
  )
}

#' @export
#' @rdname translate_sql
translate_sql_ <- function(dots,
                           con = NULL,
                           vars_group = NULL,
                           vars_order = NULL,
                           window = TRUE) {

  if (length(dots) == 0) {
    return(sql())
  }

  stopifnot(is.list(dots))

  if (!any(have_names(dots))) {
    names(dots) <- NULL
  }

  if (window) {
    old_con <- set_win_current_con(con)
    on.exit(set_win_current_con(old_con), add = TRUE)

    old_group <- set_win_current_group(vars_group)
    on.exit(set_win_current_group(old_group), add = TRUE)

    old_order <- set_win_current_order(vars_order)
    on.exit(set_win_current_order(old_order), add = TRUE)
  }

  variant <- sql_translate_env(con)
  pieces <- map(dots, function(x) {
    if (is_atomic(x)) {
      escape(x, con = con)
    } else {
      env <- sql_env(x, variant, con, window = window) 
      escape(expr_eval(x, env))
    }
  })

  sql(unlist(pieces))
}

sql_env <- function(expr, variant, con, window = FALSE,
                    strict = getOption("dplyr.strict_sql")) {
  stopifnot(is.sql_variant(variant))

  # Default for unknown functions
  if (!strict) {
    unknown <- setdiff(all_calls(expr), names(variant))
    default_env <- ceply(unknown, default_op, parent = emptyenv())
  } else {
    default_env <- new.env(parent = emptyenv())
  }


  # Known R -> SQL functions
  special_calls <- copy_env(variant$scalar, parent = default_env)
  if (!window) {
    special_calls2 <- copy_env(variant$aggregate, parent = special_calls)
  } else {
    special_calls2 <- copy_env(variant$window, parent = special_calls)
  }

  # Existing symbols in expression
  names <- all_names(expr)
  name_env <- ceply(
    names, function(x) escape(ident(x), con = con),
    parent = special_calls2
  )

  # Known sql expressions
  symbol_env <- env_clone(base_symbols, parent = name_env)

  # Make tidy quotes self-evaluating
  symbol_env <- tidy_eval_env_install(default_env, symbol_env, default_env)

  symbol_env
}

default_op <- function(x) {
  assert_that(is_scalar_character(x))
  infix <- c("::", "$", "@", "^", "*", "/", "+", "-", ">", ">=", "<", "<=",
    "==", "!=", "!", "&", "&&", "|", "||", "~", "<-", "<<-")

  if (x %in% infix) {
    sql_infix(x)
  } else if (grepl("^%.*%$", x)) {
    x <- substr(x, 2, nchar(x) - 1)
    sql_infix(x)
  } else {
    sql_prefix(x)
  }
}


all_calls <- function(x) {
  if (!is.call(x)) return(NULL)

  fname <- as.character(x[[1]])
  unique(c(fname, unlist(lapply(x[-1], all_calls), use.names = FALSE)))
}

all_names <- function(x) {
  if (is.name(x)) return(as.character(x))
  if (!is.call(x)) return(NULL)

  unique(unlist(lapply(x[-1], all_names), use.names = FALSE))
}

# character vector -> environment
ceply <- function(x, f, ..., parent = parent.frame()) {
  if (length(x) == 0) return(new.env(parent = parent))
  l <- lapply(x, f, ...)
  names(l) <- x
  list2env(l, parent = parent)
}
