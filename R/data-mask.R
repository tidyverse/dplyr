DataMask <- R6Class("DataMask",
  public = list(
    initialize = function(data, caller) {
      rows <- group_rows(data)
      # workaround for whene there are 0 groups
      if (length(rows) == 0) {
        rows <- list(integer())
      }
      private$rows <- rows

      frame <- caller_env(n = 2)
      local_mask(self, frame)

      private$data <- data
      private$caller <- caller
      private$bindings <- env(empty_env())
      private$keys <- group_keys(data)

      # A function that returns all the chunks for a column
      resolve_chunks <- if (inherits(data, "rowwise_df")) {
        function(index) {
          col <- .subset2(data, index)
          if (is_list(col) && !is.data.frame(col)) {
            map(vec_chop(col, rows), `[[`, 1L)
          } else {
            vec_chop(col, rows)
          }
        }
      } else if (is_grouped_df(data)) {
        function(index) vec_chop(.subset2(data, index), rows)
      } else {
        # for ungrouped data frames, there is only one chunk that
        # is made of the full column
        function(index) list(.subset2(data, index))
      }

      private$used <- rep(FALSE, ncol(data))

      names_bindings <- chr_unserialise_unicode(names2(data))
      private$resolved <- set_names(vector(mode = "list", length = ncol(data)), names_bindings)

      promise_fn <- function(index, chunks = resolve_chunks(index)) {
          # resolve the chunks and hold the slice for current group
          res <- .subset2(chunks, self$get_current_group())

          # track
          private$used[[index]] <- TRUE
          private$resolved[[index]] <- chunks
          private$which_used <- c(private$which_used, index)

          # return result for current slice
          res
      }

      promises <- map(seq_len(ncol(data)), function(.x) expr(promise_fn(!!.x)))

      env_bind_lazy(private$bindings, !!!set_names(promises, names_bindings))

      private$mask <- new_data_mask(private$bindings)
      private$mask$.data <- as_data_pronoun(private$mask)
    },

    add = function(name, chunks) {
      if (inherits(private$data, "rowwise_df")){
        is_scalar_list <- function(.x) {
          is.list(.x) && !is.data.frame(.x) && length(.x) == 1L
        }
        if (all(map_lgl(chunks, is_scalar_list))) {
          chunks <- map(chunks, `[[`, 1L)
        }
      }

      pos <- which(names(private$resolved) == name)
      is_new_column <- length(pos) == 0L

      if (is_new_column) {
        pos <- length(private$resolved) + 1L
        used <- FALSE
      } else {
        used <- private$used[[pos]]
      }

      if (!used) {
        private$used[[pos]] <- TRUE
        private$which_used <- c(private$which_used, pos)
      }

      private$resolved[[name]] <- chunks
    },

    remove = function(name) {
      pos <- which(names(private$resolved) == name)
      private$resolved[[name]] <- NULL
      private$which_used <- setdiff(private$which_used, pos)
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
      cols <- env_get_list(private$bindings, vars)
      nrow <- length(self$current_rows())
      new_tibble(cols, nrow = nrow)
    },

    current_rows = function() {
      private$rows[[self$get_current_group()]]
    },

    current_key = function() {
      vec_slice(private$keys, self$get_current_group())
    },

    get_current_group = function() {
      # The [] is so that we get a copy, which is important for how
      # current_group is dealt with internally, to avoid defining it at each
      # iteration of the dplyr_mask_eval_*() loops.
      private$current_group[]
    },

    set_current_group = function(group) {
      private$current_group <- group
    },

    full_data = function() {
      private$data
    },

    get_used = function() {
      private$used
    },

    get_rows = function() {
      private$rows
    }

  ),

  private = list(
    data = NULL,
    mask = NULL,
    old_vars = character(),
    used = logical(),
    resolved = list(),
    which_used = integer(),
    rows = NULL,
    keys = NULL,
    bindings = NULL,
    current_group = 0L,
    caller = NULL
  )
)
