DataMask <- R6Class("DataMask",
  public = list(
    initialize = function(data, caller, rows = group_rows(data), track_usage = FALSE) {
      frame <- caller_env(n = 2)
      local_mask(self, frame)

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

      if (track_usage) {
        private$used <- rep(FALSE, ncol(data))
        binding_fn <- function(index, chunks = resolve_chunks(index)) {
          function() {
            private$used[[index]] <- TRUE
            .subset2(chunks, private$current_group)
          }
        }
      } else {
        binding_fn <- function(index, chunks = resolve_chunks(index)){
          # chunks is a promise of the list of all chunks for the column
          # at this index, so resolve_chunks() is only called when
          # the active binding is touched
          function() .subset2(chunks, private$current_group)
        }
      }

      env_bind_active(private$bindings, !!!set_names(map(seq_len(ncol(data)), binding_fn), names(data)))

      private$mask <- new_data_mask(private$bindings)
      private$mask$.data <- as_data_pronoun(private$mask)
    },

    add = function(name, chunks) {
      force(chunks)
      env_bind_active(private$bindings, !!name := function() {
        .subset2(chunks, private$current_group)
      })
    },

    remove = function(name) {
      rm(list = name, envir = private$bindings)
    },

    eval_all = function(quo) {
      .Call(`dplyr_mask_eval_all`, quo, private)
    },

    eval_all_summarise = function(quo) {
      .Call(`dplyr_mask_eval_all_summarise`, quo, private)
    },

    eval_all_mutate = function(quo) {
      .Call(`dplyr_mask_eval_all_mutate`, quo, private)
    },

    eval_all_filter = function(quos, env_filter) {
      .Call(`dplyr_mask_eval_all_filter`, quos, private, nrow(private$data), env_filter)
    },

    pick = function(vars) {
      eval_tidy(quo(tibble(!!!syms(vars))), private$mask)
    },

    current_rows = function() {
      private$rows[[private$current_group]]
    },

    current_key = function() {
      vec_slice(private$keys, private$current_group)
    },

    get_current_group = function() {
      private$current_group
    },

    set_current_group = function(group) {
      private$current_group <- group
    },

    full_data = function() {
      private$data
    },

    get_used = function() {
      private$used
    }

  ),

  private = list(
    data = NULL,
    mask = NULL,
    old_vars = character(),
    used = logical(),
    rows = NULL,
    keys = NULL,
    bindings = NULL,
    current_group = 0L,
    caller = NULL
  )
)
