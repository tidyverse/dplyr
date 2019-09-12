# Grouping methods ------------------------------------------------------------

#' Convert row names to an explicit variable.
#'
#' Deprecated, use [tibble::rownames_to_column()] instead.
#'
#' @param df Input data frame with rownames.
#' @param var Name of variable to use
#' @keywords internal
#' @export
#' @examples
#' mtcars %>% tbl_df()
#'
#' mtcars %>% add_rownames()
add_rownames <- function(df, var = "rowname") {
  warning(
    "Deprecated, use tibble::rownames_to_column() instead.",
    call. = FALSE
  )

  stopifnot(is.data.frame(df))

  rn <- as_tibble(setNames(list(rownames(df)), var))
  rownames(df) <- NULL

  bind_cols(rn, df)
}

# Grouping methods ------------------------------------------------------------

#' @export
group_by.data.frame <- function(.data, ..., add = FALSE, .drop = group_by_drop_default(.data)) {
  groups <- group_by_prepare(.data, ..., add = add)
  grouped_df(groups$data, groups$group_names, .drop)
}
#' @export
group_by_.data.frame <- function(.data, ..., .dots = list(), add = FALSE) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  group_by(.data, !!!dots, add = add)
}

#' @export
ungroup.data.frame <- function(x, ...) x

#' @export
group_size.data.frame <- function(x) nrow(x)

#' @export
n_groups.data.frame <- function(x) 1L

# Manipulation functions ------------------------------------------------------

# These could potentially be rewritten to avoid any copies, but since this
# is just a convenience layer, I didn't bother. They should still be fast.

#' @export
filter.data.frame <- function(.data, ..., .preserve = FALSE) {
  as.data.frame(filter(tbl_df(.data), ..., .preserve = .preserve))
}
#' @export
filter_.data.frame <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  filter(.data, !!!dots)
}

#' @export
slice.data.frame <- function(.data, ..., .preserve = FALSE) {
  as.data.frame(slice(tbl_df(.data), ..., .preserve = .preserve))
}
#' @export
slice_.data.frame <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  slice(.data, !!!dots)
}

#' @export
summarise.data.frame <- function(.data, ...) {
  as.data.frame(summarise(tbl_df(.data), ...))
}
#' @export
summarise_.data.frame <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  summarise(.data, !!!dots)
}

#' @export
mutate.data.frame <- function(.data, ...) {
  as.data.frame(mutate(tbl_df(.data), ...))
}
#' @export
mutate_.data.frame <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  mutate(.data, !!!dots)
}

#' @export
arrange.data.frame <- function(.data, ..., .by_group = FALSE) {
  as.data.frame(arrange(tbl_df(.data), ..., .by_group = .by_group))
}
#' @export
arrange_.data.frame <- function(.data, ..., .dots = list(), .by_group = FALSE) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  arrange(.data, !!!dots, .by_group = .by_group)
}

#' @export
select.data.frame <- function(.data, ...) {
  # Pass via splicing to avoid matching vars_select() arguments
  vars <- tidyselect::vars_select(tbl_vars(.data), !!!enquos(...))
  select_impl(.data, vars)
}
#' @export
select_.data.frame <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  select(.data, !!!dots)
}

#' @export
rename.data.frame <- function(.data, ...) {
  vars <- tidyselect::vars_rename(names(.data), !!!enquos(...))
  select_impl(.data, vars)
}
#' @export
rename_.data.frame <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  rename(.data, !!!dots)
}


# Joins ------------------------------------------------------------------------

#' @export
inner_join.data.frame <- function(x, y, by = NULL, copy = FALSE, ...) {
  as.data.frame(inner_join(tbl_df(x), y, by = by, copy = copy, ...))
}

#' @export
left_join.data.frame <- function(x, y, by = NULL, copy = FALSE, ...) {
  as.data.frame(left_join(tbl_df(x), y, by = by, copy = copy, ...))
}

#' @export
#' @rdname join.tbl_df
nest_join.data.frame <- function(x, y, by = NULL, copy = FALSE, keep = FALSE, name = NULL, ... ) {
  as.data.frame(nest_join(tbl_df(x), y, by = by, copy = copy, ..., keep = keep, name = name))
}

#' @export
right_join.data.frame <- function(x, y, by = NULL, copy = FALSE, ...) {
  as.data.frame(right_join(tbl_df(x), y, by = by, copy = copy, ...))
}

#' @export
full_join.data.frame <- function(x, y, by = NULL, copy = FALSE, ...) {
  as.data.frame(full_join(tbl_df(x), y, by = by, copy = copy, ...))
}

#' @export
semi_join.data.frame <- function(x, y, by = NULL, copy = FALSE, ...) {
  as.data.frame(semi_join(tbl_df(x), y, by = by, copy = copy, ...))
}

#' @export
anti_join.data.frame <- function(x, y, by = NULL, copy = FALSE, ...) {
  as.data.frame(anti_join(tbl_df(x), y, by = by, copy = copy, ...))
}

# Set operations ---------------------------------------------------------------

is_compatible_data_frame <- function(x, y, ignore_col_order = TRUE, convert = TRUE) {
  nc <- ncol(x)
  if (nc != ncol(y)) {
    return(glue("- different number of columns : {nc} vs {ncol(y)}"))
  }

  names_x <- names(x)
  names_y <- names(y)

  names_y_not_in_x <- setdiff(names_y, names_x)
  names_x_not_in_y <- setdiff(names_x, names_y)

  if (length(names_y_not_in_x) == 0L && length(names_x_not_in_y) == 0L) {
    # check if same order
    if (!isTRUE(ignore_col_order)) {
      if (!identical(names_x, names_y)) {
        return("- Same column names, but different order")
      }
    }
  } else {
    # names are not the same, explain why

    msg <- "not compatible: \n"
    if (length(names_y_not_in_x)) {
      msg <- paste0(msg, "- Cols in y but not x: ", glue_collapse(glue('`{names_y_not_in_x}`'), sep = ", "), ".\n")
    }
    if (length(names_x_not_in_y)) {
      msg <- paste0(msg, "- Cols in x but not y: ", glue_collapse(glue('`{names_x_not_in_y}`'), sep = ", "), ".\n")
    }
    return(msg)
  }

  msg <- ""
  for (name in names_x) {
    x_i <- x[[name]]
    y_i <- y[[name]]

    if (convert) {
      tryCatch(
        vec_ptype2(x_i, y_i),
        error = function(e) {
          msg <<- paste0(msg,
            glue("- Incompatible types for column `{name}`: {vec_ptype_full(x_i)} vs {vec_ptype_full(y_i)}"),
            "\n"
          )
        }
      )
    } else {
      if (!identical(vec_ptype(x_i), vec_ptype(y_i))) {
        msg <- paste0(msg,
          glue("- Different types for column `{name}`: {vec_ptype_full(x_i)} vs {vec_ptype_full(y_i)}"),
          "\n"
        )
      }
    }
  }
  if (msg != "") {
    return(msg)
  }

  TRUE
}

check_compatible <- function(x, y, ignore_col_order = TRUE, convert = TRUE) {
  compat <- is_compatible_data_frame(x, y, ignore_col_order = ignore_col_order, convert = convert)
  if (is.character(compat)) {
    abort(paste0("not compatible: \n", glue_collapse(compat, sep = "\n")))
  }
}

equal_data_frame <- function(x, y, ignore_col_order = TRUE, ignore_row_order = TRUE, convert = FALSE) {
  compat <- is_compatible_data_frame(x, y, ignore_col_order = ignore_col_order, convert = convert)
  if (!isTRUE(compat)) {
    return(compat)
  }

  nrows_x <- nrow(x)
  nrows_y <- nrow(y)
  if (nrows_x != nrows_y) {
    return("Different number of rows")
  }

  if (ncol(x) == 0L) {
    return(TRUE)
  }

  x <- as_tibble(x)
  y <- as_tibble(y)

  x_split <- vec_split_id_order(x)
  y_split <- vec_split_id_order(y[, names(x), drop = FALSE])

  # keys must be identical
  msg <- ""
  if (any(wrong <- !vec_in(x_split$key, y_split$key))) {
    rows <- sort(map_int(x_split$id[which(wrong)], function(.x) .x[1L]))
    msg <- paste0(msg, "- Rows in x but not in y: ", glue_collapse(rows, sep = ", "), "\n")
  }

  if (any(wrong <- !vec_in(y_split$key, x_split$key))) {
    rows <- sort(map_int(y_split$id[which(wrong)], function(.x) .x[1L]))
    msg <- paste0(msg, "- Rows in y but not in x: ", glue_collapse(rows, sep = ", "), "\n")
  }
  if (msg != "") {
    return(msg)
  }

  # keys are identical, check that rows occur the same number of times
  if (any(wrong <- lengths(x_split$id) != lengths(y_split$id))) {
    rows <- sort(map_int(x_split$id[which(wrong)], function(.x) .x[1L]))
    return(paste0("- Rows with difference occurences in x and y: ",
      glue_collapse(rows, sep = ", "),
      "\n"
    ))
  }

  # then if we care about row order, the id need to be identical
  if (!ignore_row_order && !all(vec_equal(x_split$id, y_split$id))) {
    return("Same row values, but different order")
  }

  TRUE
}

#' @export
intersect.data.frame <- function(x, y, ...) {
  check_compatible(x, y)
  original_x <- x
  c(x, y) %<-% vec_cast_common(x, y)
  out <- vec_unique(vec_slice(x, vec_in(x, y)))
  reconstruct_set(out, original_x)
}

#' @export
union.data.frame <- function(x, y, ...) {
  check_compatible(x, y)
  out <- vec_unique(vec_rbind(!!!vec_cast_common(x, y)))
  reconstruct_set(out, x)
}

#' @export
union_all.data.frame <- function(x, y, ...) {
  out <- bind_rows(x, y)
  reconstruct_set(out, x)
}

#' @export
setdiff.data.frame <- function(x, y, ...) {
  check_compatible(x, y)
  original_x <- x
  c(x, y) %<-% vec_cast_common(x, y)
  out <- vec_unique(vec_slice(x, !vec_in(x, y)))
  reconstruct_set(out, original_x)
}

#' @export
setequal.data.frame <- function(x, y, ...) {
  isTRUE(equal_data_frame(x, y))
}

reconstruct_set <- function(out, x) {
  if (is_grouped_df(x)) {
    out <- grouped_df(out, group_vars(x), group_by_drop_default(x))
  }

  out
}

#' @export
distinct.data.frame <- function(.data, ..., .keep_all = FALSE) {
  dist <- distinct_prepare(.data, enquos(...), .keep_all = .keep_all)
  vec_slice(
    dist$data[, dist$keep, drop = FALSE],
    vec_unique_loc(dist$data[, dist$vars, drop = FALSE])
  )
}
#' @export
distinct_.data.frame <- function(.data, ..., .dots = list(), .keep_all = FALSE) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  distinct(.data, !!!dots, .keep_all = .keep_all)
}


# Do ---------------------------------------------------------------------------

#' @export
do.data.frame <- function(.data, ...) {
  args <- enquos(...)
  named <- named_args(args)

  # Create custom data mask with `.` pronoun
  mask <- new_data_mask(new_environment())
  env_bind_do_pronouns(mask, .data)

  if (!named) {
    out <- eval_tidy(args[[1]], mask)
    if (!inherits(out, "data.frame")) {
      bad("Result must be a data frame, not {fmt_classes(out)}")
    }
  } else {
    out <- map(args, function(arg) list(eval_tidy(arg, mask)))
    names(out) <- names(args)
    out <- tibble::as_tibble(out, validate = FALSE)
  }

  out
}
#' @export
do_.data.frame <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  do(.data, !!!dots)
}

# Random samples ---------------------------------------------------------------


#' @export
sample_n.data.frame <- function(tbl, size, replace = FALSE,
                                weight = NULL, .env = NULL, ...) {
  if (!is_null(.env)) {
    inform("`.env` is deprecated and no longer has any effect")
  }

  size <- enquo(size)
  weight <- enquo(weight)

  slice(tbl, sample.int(n(), check_size(!!size, n(), replace = replace), replace = replace, prob = !!weight))
}


#' @export
sample_frac.data.frame <- function(tbl, size = 1, replace = FALSE,
                                   weight = NULL, .env = NULL, ...) {
  if (!is_null(.env)) {
    inform("`.env` is deprecated and no longer has any effect")
  }

  size <- enquo(size)
  weight <- enquo(weight)

  slice(tbl, sample.int(n(), round(n() * check_frac(!!size, replace = replace)), replace = replace, prob = !!weight))
}

# Misc -------------------------------------------------------------------------

#' @export
collect.data.frame <- function(x, ...) x
#' @export
compute.data.frame <- function(x, ...) x
#' @export
collapse.data.frame <- function(x, ...) x
