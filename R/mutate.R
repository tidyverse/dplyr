#' Create, modify, and delete columns
#'
#' `mutate()` creates new columns that are functions of existing variables.
#' It can also modify (if the name is the same as an existing
#' column) and delete columns (by setting their value to `NULL`).
#'
#' @section Useful mutate functions:
#'
#' * [`+`], [`-`], [log()], etc., for their usual mathematical meanings
#'
#' * [lead()], [lag()]
#'
#' * [dense_rank()], [min_rank()], [percent_rank()], [row_number()],
#'   [cume_dist()], [ntile()]
#'
#' * [cumsum()], [cummean()], [cummin()], [cummax()], [cumany()], [cumall()]
#'
#' * [na_if()], [coalesce()]
#'
#' * [if_else()], [recode()], [case_when()]
#'
#' @section Grouped tibbles:
#'
#' Because mutating expressions are computed within groups, they may
#' yield different results on grouped tibbles. This will be the case
#' as soon as an aggregating, lagging, or ranking function is
#' involved. Compare this ungrouped mutate:
#'
#' ```
#' starwars %>%
#'   select(name, mass, species) %>%
#'   mutate(mass_norm = mass / mean(mass, na.rm = TRUE))
#' ```
#'
#' With the grouped equivalent:
#'
#' ```
#' starwars %>%
#'   select(name, mass, species) %>%
#'   group_by(species) %>%
#'   mutate(mass_norm = mass / mean(mass, na.rm = TRUE))
#' ```
#'
#' The former normalises `mass` by the global average whereas the
#' latter normalises by the averages within species levels.
#'
#' @export
#' @inheritParams arrange
#' @param ... <[`data-masking`][dplyr_data_masking]> Name-value pairs.
#'   The name gives the name of the column in the output.
#'
#'   The value can be:
#'
#'   * A vector of length 1, which will be recycled to the correct length.
#'   * A vector the same length as the current group (or the whole data frame
#'     if ungrouped).
#'   * `NULL`, to remove the column.
#'   * A data frame or tibble, to create multiple columns in the output.
#' @family single table verbs
#' @return
#' An object of the same type as `.data`. The output has the following
#' properties:
#'
#' * Columns from `.data` will be preserved according to the `.keep` argument.
#' * Existing columns that are modified by `...` will always be returned in
#'   their original location.
#' * New columns created through `...` will be placed according to the
#'   `.before` and `.after` arguments.
#' * The number of rows is not affected.
#' * Columns given the value `NULL` will be removed.
#' * Groups will be recomputed if a grouping variable is mutated.
#' * Data frame attributes are preserved.
#' @section Methods:
#' This function is a **generic**, which means that packages can provide
#' implementations (methods) for other classes. See the documentation of
#' individual methods for extra arguments and differences in behaviour.
#'
#' Methods available in currently loaded packages:
#' \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("mutate")}.
#' @examples
#' # Newly created variables are available immediately
#' starwars %>%
#'   select(name, mass) %>%
#'   mutate(
#'     mass2 = mass * 2,
#'     mass2_squared = mass2 * mass2
#'   )
#'
#' # As well as adding new variables, you can use mutate() to
#' # remove variables and modify existing variables.
#' starwars %>%
#'   select(name, height, mass, homeworld) %>%
#'   mutate(
#'     mass = NULL,
#'     height = height * 0.0328084 # convert to feet
#'   )
#'
#' # Use across() with mutate() to apply a transformation
#' # to multiple columns in a tibble.
#' starwars %>%
#'   select(name, homeworld, species) %>%
#'   mutate(across(!name, as.factor))
#' # see more in ?across
#'
#' # Window functions are useful for grouped mutates:
#' starwars %>%
#'   select(name, mass, homeworld) %>%
#'   group_by(homeworld) %>%
#'   mutate(rank = min_rank(desc(mass)))
#' # see `vignette("window-functions")` for more details
#'
#' # By default, new columns are placed on the far right.
#' df <- tibble(x = 1, y = 2)
#' df %>% mutate(z = x + y)
#' df %>% mutate(z = x + y, .before = 1)
#' df %>% mutate(z = x + y, .after = x)
#'
#' # By default, mutate() keeps all columns from the input data.
#' df <- tibble(x = 1, y = 2, a = "a", b = "b")
#' df %>% mutate(z = x + y, .keep = "all") # the default
#' df %>% mutate(z = x + y, .keep = "used")
#' df %>% mutate(z = x + y, .keep = "unused")
#' df %>% mutate(z = x + y, .keep = "none")
#'
#' # Grouping ----------------------------------------
#' # The mutate operation may yield different results on grouped
#' # tibbles because the expressions are computed within groups.
#' # The following normalises `mass` by the global average:
#' starwars %>%
#'   select(name, mass, species) %>%
#'   mutate(mass_norm = mass / mean(mass, na.rm = TRUE))
#'
#' # Whereas this normalises `mass` by the averages within species
#' # levels:
#' starwars %>%
#'   select(name, mass, species) %>%
#'   group_by(species) %>%
#'   mutate(mass_norm = mass / mean(mass, na.rm = TRUE))
#'
#' # Indirection ----------------------------------------
#' # Refer to column names stored as strings with the `.data` pronoun:
#' vars <- c("mass", "height")
#' mutate(starwars, prod = .data[[vars[[1]]]] * .data[[vars[[2]]]])
#' # Learn more in ?dplyr_data_masking
mutate <- function(.data, ...) {
  UseMethod("mutate")
}

#' @rdname mutate
#'
#' @inheritParams args_by
#'
#' @param .keep
#'   Control which columns from `.data` are retained in the output. Grouping
#'   columns and columns created by `...` are always kept.
#'
#'   * `"all"` retains all columns from `.data`. This is the default.
#'   * `"used"` retains only the columns used in `...` to create new
#'     columns. This is useful for checking your work, as it displays inputs
#'     and outputs side-by-side.
#'   * `"unused"` retains only the columns _not_ used in `...` to create new
#'     columns. This is useful if you generate new columns, but no longer need
#'     the columns used to generate them.
#'   * `"none"` doesn't retain any extra columns from `.data`. Only the grouping
#'     variables and columns created by `...` are kept.
#' @param .before,.after
#'   <[`tidy-select`][dplyr_tidy_select]> Optionally, control where new columns
#'   should appear (the default is to add to the right hand side). See
#'   [relocate()] for more details.
#' @export
mutate.data.frame <- function(.data,
                              ...,
                              .by = NULL,
                              .keep = c("all", "used", "unused", "none"),
                              .before = NULL,
                              .after = NULL) {
  keep <- arg_match(.keep)

  by <- compute_by({{ .by }}, .data, by_arg = ".by", data_arg = ".data")

  cols <- mutate_cols(.data, dplyr_quosures(...), by)
  used <- attr(cols, "used")

  out <- dplyr_col_modify(.data, cols)

  names_original <- names(.data)

  out <- mutate_relocate(
    out = out,
    before = {{ .before }},
    after = {{ .after }},
    names_original = names_original
  )

  names_new <- names(cols)
  names_groups <- by$names

  out <- mutate_keep(
    out = out,
    keep = keep,
    used = used,
    names_new = names_new,
    names_groups = names_groups
  )

  out
}

# Helpers -----------------------------------------------------------------

mutate_relocate <- function(out, before, after, names_original) {
  before <- enquo(before)
  after <- enquo(after)

  if (quo_is_null(before) && quo_is_null(after)) {
    return(out)
  }

  # Only change the order of completely new columns that
  # didn't exist in the original data
  names <- names(out)
  names <- setdiff(names, names_original)

  relocate(
    out,
    all_of(names),
    .before = !!before,
    .after = !!after
  )
}

mutate_keep <- function(out, keep, used, names_new, names_groups) {
  names <- names(out)

  if (keep == "all") {
    names_out <- names
  } else {
    names_keep <- switch(
      keep,
      used = names(used)[used],
      unused = names(used)[!used],
      none = character(),
      abort("Unknown `keep`.", .internal = TRUE)
    )
    names_out <- intersect(names, c(names_new, names_groups, names_keep))
  }

  dplyr_col_select(out, names_out)
}

mutate_cols <- function(data, dots, by, error_call = caller_env()) {
  # Collect dots before setting up error handlers (#6178)
  force(dots)

  error_call <- dplyr_error_call(error_call)

  mask <- DataMask$new(data, by, "mutate", error_call = error_call)
  old_current_column <- context_peek_bare("column")

  on.exit(context_poke("column", old_current_column), add = TRUE)
  on.exit(mask$forget(), add = TRUE)

  new_columns <- set_names(list(), character())
  warnings_state <- env(warnings = list())

  local_error_context(dots, 0L, mask = mask)

  withCallingHandlers(
    for (i in seq_along(dots)) {
      poke_error_context(dots, i, mask = mask)
      context_poke("column", old_current_column)

      new_columns <- mutate_col(dots[[i]], data, mask, new_columns)
    },
    error = dplyr_error_handler(
      dots = dots,
      mask = mask,
      bullets = mutate_bullets,
      error_call = error_call,
      error_class = "dplyr:::mutate_error"
    ),
    warning = dplyr_warning_handler(
      state = warnings_state,
      mask = mask,
      error_call = error_call
    )
  )

  signal_warnings(warnings_state, error_call)

  is_zap <- map_lgl(new_columns, inherits, "rlang_zap")
  new_columns[is_zap] <- rep(list(NULL), sum(is_zap))

  used <- mask$get_used()
  names(used) <- mask$current_vars()
  attr(new_columns, "used") <- used

  new_columns
}

mutate_col <- function(dot, data, mask, new_columns) {
  rows <- mask$get_rows()

  # get results from all the quosures that are expanded from ..i
  # then ingest them after
  dot <- expand_pick(dot, mask)
  quosures <- expand_across(dot)
  quosures_results <- vector(mode = "list", length = length(quosures))

  # First pass
  for (k in seq_along(quosures)) {
    quo <- quosures[[k]]
    quo_data <- attr(quo, "dplyr:::data")
    if (!is.null(quo_data$column)) {
      context_poke("column", quo_data$column)
    }
    # a list in which each element is the result of
    # evaluating the quosure in the "sliced data mask"
    # recycling it appropriately to match the group size
    #
    # TODO: reinject hybrid evaluation at the R level
    chunks <- NULL

    # result after unchopping the chunks
    result <- NULL

    if (quo_is_symbol(quo)){
      name <- as_string(quo_get_expr(quo))

      if (name %in% names(new_columns)) {
        # already have result and chunks
        result <- new_columns[[name]]
        chunks <- mask$resolve(name)
      } else if (name %in% names(data)) {
        # column from the original data
        result <- data[[name]]
        chunks <- mask$resolve(name)
      }

      if (mask$is_rowwise() && vec_is_list(result)) {
        sizes <- list_sizes(result)
        wrong <- which(sizes != 1)
        if (length(wrong)) {
          # same error as would have been generated by mask$eval_all_mutate()
          group <- wrong[1L]
          mask$set_current_group(group)

          abort(
            class = c("dplyr:::mutate_incompatible_size", "dplyr:::internal_error"),
            dplyr_error_data = list(result_size = sizes[group], expected_size = 1)
          )
        }
        result_ptype <- attr(result, "ptype", exact = TRUE)
        if (length(result) == 0 && is.null(result_ptype)) {
          # i.e. `vec_ptype_finalise(unspecified())` (#6369)
          result <- logical()
        } else {
          result <- list_unchop(result, ptype = result_ptype)
        }
      }
    } else if (!quo_is_symbolic(quo) && !is.null(quo_get_expr(quo))) {
      # constant, we still need both `result` and `chunks`
      result <- quo_get_expr(quo)

      result <- withCallingHandlers(
        vec_recycle(result, vec_size(data)),
        error = function(cnd) {
          abort(
            class = c("dplyr:::mutate_constant_recycle_error", "dplyr:::internal_error"),
            constant_size = vec_size(result), data_size = vec_size(data)
          )
        }
      )

      chunks <- vec_chop(result, rows)
    }

    if (is.null(chunks)) {
      if (is.null(quo_data$column)) {
        chunks <- mask$eval_all_mutate(quo)
      } else {
        chunks <- withCallingHandlers(
          mask$eval_all_mutate(quo),
          error = function(cnd) {
            msg <- glue("Can't compute column `{quo_data$name_auto}`.")
            abort(msg, call = call("across"), parent = cnd)
          }
        )
      }
    }

    if (is.null(chunks)) {
      next
    }

    # only unchop if needed
    if (is.null(result)) {
      if (length(rows) == 1) {
        result <- chunks[[1]]
      } else {
        chunks <- dplyr_vec_cast_common(chunks, quo_data$name_auto)
        result <- list_unchop(chunks, indices = rows)
      }
    }

    quosures_results[[k]] <- list(result = result, chunks = chunks)
  }

  # Second pass
  for (k in seq_along(quosures)) {
    quo <- quosures[[k]]
    quo_data <- attr(quo, "dplyr:::data")

    quo_result <- quosures_results[[k]]
    if (is.null(quo_result)) {
      if (quo_data$is_named) {
        name <- quo_data$name_given
        new_columns[[name]] <- zap()
        mask$remove(name)
      }
      next
    }

    result <- quo_result$result
    chunks <- quo_result$chunks

    if (!quo_data$is_named && is.data.frame(result)) {
      types <- vec_ptype(result)
      types_names <- names(types)
      chunks_extracted <- .Call(dplyr_extract_chunks, chunks, types)

      for (j in seq_along(types)) {
        mask$add_one(types_names[j], chunks_extracted[[j]], result = result[[j]])
      }

      new_columns[types_names] <- result
    } else {
      # treat as a single output otherwise
      name <- quo_data$name_auto
      mask$add_one(name = name, chunks = chunks, result = result)

      new_columns[[name]] <- result
    }
  }

  new_columns
}

mutate_bullets <- function(cnd, ...) {
  UseMethod("mutate_bullets")
}

#' @export
`mutate_bullets.dplyr:::mutate_incompatible_size` <- function(cnd, ...) {
  label <- ctxt_error_label()

  result_size <- cnd$dplyr_error_data$result_size
  expected_size <- cnd$dplyr_error_data$expected_size
  c(
    glue("`{label}` must be size {or_1(expected_size)}, not {result_size}."),
    i = cnd_bullet_rowwise_unlist()
  )
}
#' @export
`mutate_bullets.dplyr:::mutate_mixed_null` <- function(cnd, ...) {
  label <- ctxt_error_label()
  c(
    glue("`{label}` must return compatible vectors across groups."),
    x = "Can't combine NULL and non NULL results.",
    i = cnd_bullet_rowwise_unlist()
  )
}
#' @export
`mutate_bullets.dplyr:::mutate_not_vector` <- function(cnd, ...) {
  label <- ctxt_error_label()
  result <- cnd$dplyr_error_data$result
  c(
    glue("`{label}` must be a vector, not {obj_type_friendly(result)}."),
    i = cnd_bullet_rowwise_unlist()
  )
}
#' @export
`mutate_bullets.dplyr:::error_incompatible_combine` <- function(cnd, ...) {
  # the details are included in the parent error
  c()
}
#' @export
`mutate_bullets.dplyr:::mutate_constant_recycle_error` <- function(cnd, ...) {
  label <- ctxt_error_label()
  constant_size <- cnd$constant_size
  data_size <- cnd$data_size
  c(
    glue("Inlined constant `{label}` must be size {or_1(data_size)}, not {constant_size}.")
  )
}

check_muffled_warning <- function(cnd) {
  early_exit <- TRUE

  # Cancel early exits, e.g. from an exiting handler. This way we can
  # still instrument caught warnings to avoid confusing
  # inconsistencies. This doesn't work on versions of R older than
  # 3.5.0 because they don't include this change:
  # https://github.com/wch/r-source/commit/688eaebf. So with
  # `tryCatch(warning = )`, the original warning `cnd` will be caught
  # instead of the instrumented warning.
  on.exit(
    if (can_return_from_exit && early_exit) {
      return(FALSE)
    }
  )

  muffled <- withRestarts(
    muffleWarning = function(...) TRUE,
    {
      signalCondition(cnd)
      FALSE
    }
  )

  early_exit <- FALSE
  muffled
}

on_load(
  can_return_from_exit <- getRversion() >= "3.5.0"
)
