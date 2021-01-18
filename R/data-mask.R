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
      private$all_vars <- names_bindings
      private$data <- data
      private$caller <- caller

      private$chops <- .Call(dplyr_lazy_vec_chop_impl, data, rows)
      private$mask <- .Call(dplyr_data_masks_setup, private$chops, data, rows)

      private$keys <- group_keys(data)
      private$group_vars <- group_vars(data)

    },

    add_one = function(name, chunks) {
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

    add_many = function(ptype, chunks) {
      chunks_extracted <- .Call(dplyr_extract_chunks, chunks, ptype)
      map2(seq_along(ptype), names(ptype), function(j, nm) {
        self$add_one(nm, chunks_extracted[[j]])
      })
    },

    remove = function(name) {
      .Call(`dplyr_mask_remove`, private, name)
    },

    resolve = function(name) {
      private$chops[[name]]
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
      env_get_list(parent.env(private$mask), vars)
    },

    current_rows = function() {
      private$rows[[self$get_current_group()]]
    },

    current_key = function() {
      vec_slice(private$keys, self$get_current_group())
    },

    current_vars = function() {
      private$all_vars
    },

    current_non_group_vars = function() {
      setdiff(self$current_vars(), private$group_vars)
    },

    get_current_group = function() {
      parent.env(private$chops)$.current_group
    },

    set_current_group = function(group) {
      parent.env(private$chops)$.current_group <- group
    },

    full_data = function() {
      private$data
    },

    get_used = function() {
      .Call(env_resolved, private$chops, private$all_vars)
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
    },

    forget = function(fn) {
      names_bindings <- self$current_vars()

      osbolete_promise_fn <- function(name) {
        abort(c(
          "Obsolete data mask.",
          x = glue("Too late to resolve `{name}` after the end of `dplyr::{fn}()`."),
          i = glue("Did you save an object that uses `{name}` lazily in a column in the `dplyr::{fn}()` expression ?")
        ))
      }

      promises <- map(names_bindings, function(.x) expr(osbolete_promise_fn(!!.x)))
      bindings <- parent.env(private$mask)
      suppressWarnings({
        rm(list = names_bindings, envir = bindings)
        env_bind_lazy(bindings, !!!set_names(promises, names_bindings))
      })
    }

  ),

  private = list(
    # the input data
    data = NULL,

    # environment that contains lazy vec_chop()s for each input column
    # and list of result chunks as they get added.
    #
    # The parent environment of chops has:
    # - .indices: the list of indices
    # - .current_group: scalar integer that identifies the current group
    chops = NULL,

    # dynamic data mask, with active bindings for each column
    # this is an rlang data mask, as such the bindings are actually
    # in the parent environment of `mask`
    mask = NULL,

    # names of all the variables, this initially is names(data)
    # grows (and sometimes shrinks) as new columns are added/removed
    all_vars = character(),

    # names of the grouping variables
    group_vars = character(),

    # list of indices, one integer vector per group
    rows = NULL,

    # data frame of keys, one row per group
    keys = NULL,

    # caller environment of the verb (summarise(), ...)
    caller = NULL,

    # cache for across
    across_cache = list()
  )
)
