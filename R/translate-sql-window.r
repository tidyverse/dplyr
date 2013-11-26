translate_window_env <- function(x, groups, order) {
  UseMethod("translate_window_env")
}

#' @export
translate_window_env.tbl_sql <- function(x, groups = NULL, order = NULL) {
  variant <- translate_env(x)
  groups <- translate_sql_q(groups(x), variant = variant)
  order <- translate_sql_q(x$order_by, variant = order)
  
  translate_window_env(x$src, groups, order)
}

translate_window_env_base <- function(x, group_by = NULL, order_by = NULL) {
  stopifnot(is.sql(group_by) || is.null(group_by))
  stopifnot(is.sql(order_by) || is.null(order_by))
  
  # rank functions have a single order argument that overrides the default
  win_rank <- function(f) {
    force(f)
    function(order = NULL) {
      over(build_sql(sql(f), list()), group_by, order %||% order_by)
    }
  }
  rank <- named("row_number", "min_rank" = "rank", "rank", "dense_rank", 
    "percent_rank", "cume_dist")
  rank_f <- lapply(rank, win_rank)
  rank_f$ntile <- function(order, n) {
    over(build_sql("NTILE", list(n)), group_by, order %||% order_by)
  }
  
  # Recycled aggregate fuctions take single argument, don't need order and 
  # include entire partition in frame.
  win_recycled <- function(f) {
    force(f)
    function(x) {
      over(build_sql(sql(f), list(x)), group_by, NULL, frame = c(-Inf, Inf))
    }
  }
  recycled <- named("mean" = "avg", "sum", "min", "max", "any", "all")
  recycled_f <- lapply(recycled, win_recycled)
  recycled_f$n <- function() {
    over(sql("COUNT(*)"), group_by, frame = c(-Inf, Inf))
  }
  
  # Cumulative function are like recycled aggregates except that R names
  # have cum prefix, order_by is inherited and frame goes from -Inf to 0.
  win_cumulative <- function(f) {
    force(f)
    function(x) {
      over(build_sql(sql(f), list(x)), group_by, order_by, frame = c(-Inf, 0))
    }
  }
  cum <- setNames(recycled, paste0("cum", names(recycled)))
  cumulative_f <- lapply(cum, win_cumulative)
  
  # Finally there are a few miscellaenous functions that don't follow any
  # particular pattern
  misc_f <- list()
  
  misc_f$nth_value <- function(x, order = NULL) {
    over(build_sql("NTH_VALUE", list(x)), group_by, order %||% order_by)
  }
  misc_f$first_value <- function(x, order = NULL) {
    over(sql("FIRST_VALUE()"), group_by, order %||% order_by)
  }
  misc_f$last_value <- function(x, order = NULL) {
    over(sql("LAST_VALUE()"), group_by, order %||% order_by)
  }
  
  misc_f$lead <- function(x, n = 1L, default = NA, order = NULL) {
    over(build_sql("LEAD", list(x, n, default)), group_by, order %||% order_by)
  }
  misc_f$lag <- function(x, n = 1L, default = NA, order = NULL) {
    over(build_sql("LAG", list(x, n, default)), group_by, order %||% order_by)
  }
  
  misc_f$order_by <- function(order_by, expr) {
    over(expr, group_by, order_by)
  }
  
  all <- c(rank_f, recycled_f, cumulative_f, misc_f)
  sql_variant(.funs = all, .parent = translate_env(x))
}

#' @export
translate_window_env.default <- function(x, groups = NULL, order = NULL) {
  rank <- c("row_number", "min_rank", "rank", "dense_rank", "percent_rank",
    "cume_dist")
  agg <-  c("mean", "sum", "min", "max", "any", "all")
  c_agg <- paste0("cum", agg)
  misc <- c("nth_value", "first_value", "last_value", "lead", "lag", "order_by")
  
  windows <- c(rank, agg, c_agg, misc)
  
  no_windows <- function(f) {
    force(f)
    function(...) {
      msg <- paste0(class(x)[1], " does not support windowed function ", f, "()")
      stop(msg, call. = FALSE)
    }
  }
  window_f <- lapply(windows, no_windows)
  names(window_f) <- windows
  sql_variant(.funs = window_f, .parent = translate_env(x))
}

# players <- group_by(batting, teamID)
# translate_window_where(quote(1), players)
# translate_window_where(quote(x), players)
# translate_window_where(quote(x == 1), players)
# translate_window_where(quote(x == 1 && y == 2), players)
# translate_window_where(quote(n() > 10), players)
# translate_window_where(quote(rank() > cumsum(AB)), players)
# translate_window_where(list(quote(x == 1), quote(n() > 2)), players)

translate_window_where <- function(expr, tbl, 
                                   variant = translate_window_env(tbl), 
                                   con = NULL) {
  
  # Simplest base case: atomic vector or name ---------------------------------
  if (is.atomic(expr) || is.name(expr)) {
    return(list(
      expr = expr,
      comp = list()
    ))
  }
  
  
  # Other base case is an aggregation function --------------------------------
  agg_f <- c("mean", "sum", "min", "max", "n", "cummean", "cummax", "cummin",
    "cumsum", "order", "rank", "lag", "lead")
  if (is.call(expr) && as.character(expr[[1]]) %in% agg_f) {
    # For now, hard code aggregation functions 
    name <- unique_name()
    
    env <- sql_env(expr, variant, con = con)
    sql <- eval(expr, envir = env)
    
    return(list(
      expr = as.name(name),
      comp = setNames(list(sql), name)
    ))
  }
  
  # Recursive cases: list and all other functions -----------------------------
  
  if (is.list(expr)) {
    args <- lapply(expr, translate_window_where, 
      tbl = tbl, variant = variant, con = con)
    
    env <- sql_env(call, variant, con = con)
    sql <- lapply(lapply(args, "[[", "expr"), eval, env = env)    
  } else {
    args <- lapply(expr[-1], translate_window_where, 
      tbl = tbl, variant = variant, con = con)
    
    call <- as.call(c(expr[[1]], lapply(args, "[[", "expr")))
    env <- sql_env(call, variant, con = con)
    sql <- eval(call, envir = env)
  }
  
  comps <- unlist(lapply(args, "[[", "comp"), recursive = FALSE)
  
  list(
    expr = sql, 
    comp = comps
  )
}

unique_name <- local({
  i <- 0
  
  function() {
    i <<- i + 1
    paste0("_W", i)
  }
})

