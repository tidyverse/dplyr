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

      names_bindings <- chr_unserialise_unicode(names2(data))
      if (anyDuplicated(names_bindings)) {
        abort("Can't transform a data frame with duplicate names.")
      }
      names(data) <- names_bindings
      private$data <- data
      private$caller <- caller

      private$chops <- dplyr_lazy_vec_chop(data, rows)
      private$masks <- dplyr_data_masks(private$chops, data, rows)

      private$keys <- group_keys(data)
      private$group_vars <- group_vars(data)

      private$resolved <- set_names(vector(mode = "list", length = ncol(data)), names_bindings)
    },

    add = function(name, chunks) {
      if (inherits(private$data, "rowwise_df")){
        is_scalar_list <- function(.x) {
          vec_is_list(.x) && length(.x) == 1L
        }
        if (all(map_lgl(chunks, is_scalar_list))) {
          chunks <- map(chunks, `[[`, 1L)
        }
      }

      .Call(`dplyr_mask_add`, private, name, chunks)
    },

    set = function(name, chunks) {
      .Call(`dplyr_mask_set`, private, name, chunks)
    },

    remove = function(name) {
      self$set(name, NULL)
    },

    resolve = function(name) {
      chunks <- self$get_resolved(name)

      if (is.null(chunks)) {
        column <- private$data[[name]]
        chunks <- vec_chop(column, private$rows)
        if (inherits(private$data, "rowwise_df") && vec_is_list(column)) {
          chunks <- map(chunks, `[[`, 1)
        }
        self$set(name, chunks)
      }

      chunks
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
      mask <- parent.env(private$masks[[private$current_group]])
      env_get_list(mask, vars)
    },

    current_rows = function() {
      private$rows[[private$current_group]]
    },

    current_key = function() {
      vec_slice(private$keys, private$current_group)
    },

    current_vars = function() {
      names(private$resolved)
    },

    current_non_group_vars = function() {
      setdiff(self$current_vars(), private$group_vars)
    },

    get_current_group = function() {
      private$current_group[]
    },

    set_current_group = function(group) {
      private$current_group <- group
    },

    full_data = function() {
      private$data
    },

    get_used = function() {
      .Call(env_resolved, private$chops, names(private$resolved))
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

      # workaround until vctrs 0.3.5 is on CRAN
      # (https://github.com/r-lib/vctrs/issues/1263)
      if (length(cols) == 0) {
        names(cols) <- character()
      }

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
    chops = NULL,
    masks = NULL,

    group_vars = character(),
    resolved = list(),
    rows = NULL,
    keys = NULL,
    current_group = 0L,
    caller = NULL,
    across_cache = list()
  )
)
