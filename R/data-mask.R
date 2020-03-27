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
      private$group_vars <- group_vars(data)

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

      promise_fn <- function(index, chunks = resolve_chunks(index), name = names_bindings[index]) {
          # resolve the chunks and hold the slice for current group
          res <- .subset2(chunks, self$get_current_group())

          # track - not safe to directly use `index`
          self$set(name, chunks)

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

    set = function(name, chunks) {
      private$resolved[[name]] <- chunks
      private$used <- !map_lgl(private$resolved, is.null)
      private$which_used <- which(private$used)
    },

    remove = function(name) {
      self$set(name, NULL)
      rm(list = name, envir = private$bindings)
    },

    get_resolved = function(name) {
      private$resolved[[name]]
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
      cols <- self$current_cols(vars)
      nrow <- length(self$current_rows())
      new_tibble(cols, nrow = nrow)
    },

    current_cols = function(vars) {
      env_get_list(private$bindings, vars)
    },

    current_rows = function() {
      private$rows[[self$get_current_group()]]
    },

    current_key = function() {
      vec_slice(private$keys, self$get_current_group())
    },

    current_vars = function() {
      names(private$resolved)
    },

    current_non_group_vars = function() {
      current_vars <- self$current_vars()
      setdiff(current_vars, private$group_vars)
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

    unused_vars = function() {
      used <- self$get_used()
      current_vars <- self$current_vars()
      current_vars[!used]
    },

    get_rows = function() {
      private$rows
    },

    across_cols = function() {
      original_data <- self$full_data()
      original_data <- unclass(original_data)

      across_vars <- self$current_non_group_vars()
      unused_vars <- self$unused_vars()

      across_vars_unused <- intersect(across_vars, unused_vars)
      across_vars_used <- setdiff(across_vars, across_vars_unused)

      # Pull unused vars from original data to keep from marking them as used.
      # Column lengths will not match if `original_data` is grouped, but for
      # the usage of tidyselect in `across()` we only need the column names
      # and types to be correct.
      cols_unused <- original_data[across_vars_unused]
      cols_used <- self$current_cols(across_vars_used)

      cols <- vec_c(cols_unused, cols_used)

      # Match original ordering
      cols <- cols[across_vars]

      cols
    },

    across_cache_get = function(key) {
      private$across_cache[[key]]
    },

    across_cache_add = function(key, value) {
      private$across_cache[[key]] <- value
    },

    across_cache_reset = function() {
      private$across_cache <- list()
    }

  ),

  private = list(
    data = NULL,
    mask = NULL,
    old_vars = character(),
    group_vars = character(),
    used = logical(),
    resolved = list(),
    which_used = integer(),
    rows = NULL,
    keys = NULL,
    bindings = NULL,
    current_group = 0L,
    caller = NULL,
    across_cache = list()
  )
)
