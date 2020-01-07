poke_mask <- function(mask) {
  old <- context_env[["..mask"]]
  context_env[["..mask"]] <- mask
  old
}

peek_mask <- function() {
  context_env[["..mask"]] %||% abort("No dplyr data mask registered")
}

scoped_mask <- function(mask, frame = caller_env()) {
  old_mask <- poke_mask(mask)
  old_group_size <- context_env[["..group_size"]]
  old_group_number <- context_env[["..group_number"]]

  expr <- call2(on.exit, expr({
    poke_mask(!!old_mask)
    context_env[["..group_size"]] <- !!old_group_size
    context_env[["..group_number"]] <- !!old_group_number
  }), add = TRUE)
  eval_bare(expr, frame)
}

DataMask <- R6Class("DataMask",
  public = list(
    initialize = function(data, caller, rows = group_rows(data)) {
      frame <- caller_env(n = 2)
      tidyselect::scoped_vars(tbl_vars(data), frame)
      scoped_mask(self, frame)

      private$rows <- rows
      private$data <- data
      private$caller <- caller
      private$bindings <- env(empty_env())
      private$keys <- group_keys(data)

      # A function that returns all the chunks for a column
      resolve_chunks <- if (inherits(data, "rowwise_df")) {
        function(index) {
          col <- .subset2(data, index)
          if (is_list(col) && !is.data.frame(col)) {
            map(rows, function(row) vec_slice(col, row)[[1L]])
          } else {
            map(rows, vec_slice, x = col)
          }
        }
      } else {
        function(index) map(rows, vec_slice, x = .subset2(data, index))
      }

      binding_fn <- function(index, chunks = resolve_chunks(index)){
        # chunks is a promise of the list of all chunks for the column
        # at this index, so resolve_chunks() is only called when
        # the active binding is touched
        function() .subset2(chunks, private$current_group)
      }
      env_bind_active(private$bindings, !!!set_names(map(seq_len(ncol(data)), binding_fn), names(data)))

      private$mask <- new_data_mask(private$bindings)
      private$mask$.data <- as_data_pronoun(private$mask)
    },

    add = function(name, chunks) {
      if (name %in% group_vars(private$data)) {
        abort(glue("Column `{name}` can't be modified because it's a grouping variable"))
      }
      force(chunks)
      env_bind_active(private$bindings, !!name := function() {
        .subset2(chunks, private$current_group)
      })
    },

    remove = function(name) {
      rm(list = name, envir = private$bindings)
    },

    eval_all = function(quo) {
      .Call(`dplyr_mask_eval_all`, quo, private, context_env)
    },

    eval_all_summarise = function(quo, dots_names, i) {
      .Call(`dplyr_mask_eval_all_summarise`, quo, private, context_env, dots_names, i)
    },

    eval_all_mutate = function(quo, dots_names, i) {
      .Call(`dplyr_mask_eval_all_mutate`, quo, private, context_env, dots_names, i)
    },

    eval_all_filter = function(quos, env_filter) {
      .Call(`dplyr_mask_eval_all_filter`, quos, private, context_env, nrow(private$data), private$data, env_filter)
    },

    pick = function(vars) {
      eval_tidy(quo(tibble(!!!syms(vars))), private$mask)
    },

    current_key = function() {
      vec_slice(keys, private$current_group)
    },

    get_current_group = function() {
      private$current_group
    },

    full_data = function() {
      private$data
    }

  ),

  private = list(
    data = NULL,
    mask = NULL,
    old_vars = character(),
    rows = NULL,
    keys = NULL,
    bindings = NULL,
    current_group = 0L,
    caller = NULL
  )
)
