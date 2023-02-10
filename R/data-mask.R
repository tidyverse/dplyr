DataMask <- R6Class("DataMask",
  public = list(
    initialize = function(data, by, verb, error_call) {
      rows <- by$data$.rows
      if (length(rows) == 0) {
        # Specially handle case of zero groups
        rows <- new_list_of(list(integer()), ptype = integer())
      }
      private$rows <- rows

      frame <- caller_env(n = 2)
      local_mask(self, frame)

      names_bindings <- chr_unserialise_unicode(names2(data))
      if (anyDuplicated(names_bindings)) {
        abort("Can't transform a data frame with duplicate names.", call = error_call)
      }
      names(data) <- names_bindings

      private$size <- nrow(data)
      private$current_data <- dplyr_new_list(data)

      private$grouped <- by$type == "grouped"
      private$rowwise <- by$type == "rowwise"

      private$chops <- .Call(dplyr_lazy_vec_chop_impl, data, rows, private$grouped, private$rowwise)
      private$env_mask_bindings <- .Call(dplyr_make_mask_bindings, private$chops, data)

      private$keys <- group_keys0(by$data)
      private$by_names <- by$names
      private$verb <- verb
    },

    add_one = function(name, chunks, result) {
      if (self$is_rowwise()){
        is_scalar_list <- function(.x) {
          vec_is_list(.x) && length(.x) == 1L
        }
        if (all(map_lgl(chunks, is_scalar_list))) {
          chunks <- map(chunks, `[[`, 1L)
        }
      }

      .Call(`dplyr_mask_binding_add`, private, name, result, chunks)
    },

    remove = function(name) {
      .Call(`dplyr_mask_binding_remove`, private, name)
    },

    resolve = function(name) {
      private$chops[[name]]
    },

    eval_all = function(quo) {
      .Call(`dplyr_mask_eval_all`, quo, private)
    },

    eval_all_summarise = function(quo) {
      # Wrap in a function called `eval()` so that rlang ignores the
      # call in error messages. This only concerns errors that occur
      # directly in `quo`.
      eval <- function() .Call(`dplyr_mask_eval_all_summarise`, quo, private)
      eval()
    },

    eval_all_mutate = function(quo) {
      eval <- function() .Call(`dplyr_mask_eval_all_mutate`, quo, private)
      eval()
    },

    eval_all_filter = function(quos, env_filter) {
      eval <- function() .Call(`dplyr_mask_eval_all_filter`, quos, private, private$size, env_filter)
      eval()
    },

    pick_current = function(vars) {
      # Only used for deprecated `cur_data()`, `cur_data_all()`, and
      # `across(.fns = NULL)`. We should remove this when we defunct those.
      cols <- self$current_cols(vars)

      if (self$is_rowwise()) {
        cols <- map2(cols, names(cols), function(col, name) {
          if (vec_is_list(private$current_data[[name]])) {
            col <- list(col)
          }
          col
        })
      }

      size <- length(self$current_rows())
      dplyr_new_tibble(cols, size = size)
    },

    current_cols = function(vars) {
      env_get_list(private$env_mask_bindings, vars)
    },

    current_rows = function() {
      private$rows[[self$get_current_group()]]
    },

    current_key = function() {
      keys <- private$keys

      if (vec_size(keys) == 0L) {
        # Specially handle case of zero groups, like in `$initialize()`.
        # We always evaluate at least 1 group, so the slice call would attempt
        # to do `vec_slice(<0-row-df>, 1L)`, which is an error.
        keys
      } else {
        vec_slice(keys, self$get_current_group())
      }
    },

    current_vars = function() {
      names(private$current_data)
    },

    current_non_group_vars = function() {
      setdiff(self$current_vars(), private$by_names)
    },

    get_current_group = function() {
      parent.env(private$chops)$.current_group
    },

    set_current_group = function(group) {
      parent.env(private$chops)$.current_group[] <- group
    },

    get_used = function() {
      .Call(env_resolved, private$chops, names(private$current_data))
    },

    unused_vars = function() {
      used <- self$get_used()
      current_vars <- self$current_vars()
      current_vars[!used]
    },

    get_rows = function() {
      private$rows
    },

    get_current_data = function(groups = TRUE) {
      out <- private$current_data

      if (!groups) {
        out <- out[self$current_non_group_vars()]
      }

      out
    },

    forget = function() {
      names_bindings <- self$current_vars()
      verb <- private$verb

      osbolete_promise_fn <- function(name) {
        abort(c(
          "Obsolete data mask.",
          x = glue("Too late to resolve `{name}` after the end of `dplyr::{verb}()`."),
          i = glue("Did you save an object that uses `{name}` lazily in a column in the `dplyr::{verb}()` expression ?")
        ), call = NULL)
      }

      promises <- map(names_bindings, function(.x) expr(osbolete_promise_fn(!!.x)))
      env_mask_bindings <- private$env_mask_bindings
      suppressWarnings({
        rm(list = names_bindings, envir = env_mask_bindings)
        env_bind_lazy(env_mask_bindings, !!!set_names(promises, names_bindings))
      })
    },

    is_grouped = function() {
      private$grouped
    },

    is_rowwise = function() {
      private$rowwise
    },

    get_keys = function() {
      private$keys
    },

    get_size = function() {
      private$size
    },

    get_rlang_mask = function() {
      # Mimicking the data mask that is created during typical
      # expression evaluations, like in `DataMask$eval_all_mutate()`.
      # Important to insert a `.data` pronoun!
      mask <- new_data_mask(private$env_mask_bindings)
      mask[[".data"]] <- as_data_pronoun(private$env_mask_bindings)
      mask
    }

  ),

  private = list(
    # environment that contains lazy vec_chop()s for each input column
    # and list of result chunks as they get added.
    #
    # The parent environment of chops has:
    # - .indices: the list of indices
    # - .current_group: scalar integer that identifies the current group
    chops = NULL,

    # Environment with active bindings for each column.
    # Expressions are evaluated in a fresh data mask created from this
    # environment. Each group gets its own newly created data mask to avoid
    # cross group contamination of the data mask by lexical side effects, like
    # usage of `<-` (#6666).
    env_mask_bindings = NULL,

    # ptypes of all the variables
    current_data = list(),

    # names of the `by` variables
    by_names = character(),

    # list of indices, one integer vector per group
    rows = NULL,

    # data frame of keys, one row per group
    keys = NULL,

    # number of rows in `data`
    size = NULL,

    # Type of data frame
    grouped = NULL,
    rowwise = NULL,

    verb = character()
  )
)
