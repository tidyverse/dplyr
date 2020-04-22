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

      private$used <- rep(FALSE, ncol(data))

      names_bindings <- chr_unserialise_unicode(names2(data))
      private$resolved <- set_names(vector(mode = "list", length = ncol(data)), names_bindings)

      # promise_fn() is responsible for
      # - materialize all the chunks for a given column as needed
      # - tracking which one has been used

      # it needs to handle 3 cases: grouped, rowwise and simple

     if (inherits(data, "grouped_df")) {
        promise_fn <- function(index) {
          name <- names_bindings[index]
          column <- .subset2(data, index)

          # resolve the chunks and hold the slice for current group
          chunks <- vec_chop(column, rows)
          res <- .subset2(chunks, self$get_current_group())

          self$set(name, chunks)
          res
        }
      } else if (inherits(data, "rowwise_df")) {
        promise_fn <- function(index) {
          # similar to grouped, with some extra work for list columns
          name <- names_bindings[index]
          column <- .subset2(data, index)

          # resolve the chunks and hold the slice for current group
          chunks <- vec_chop(column, rows)
          res <- .subset2(chunks, self$get_current_group())

          # deal with rowwise magic
          if (vec_is_list(column)) {
            # remember to do the rowwise magic
            class(chunks) <- "dplyr_rowwise_simplify"

            # do the rowwise magic for this time
            res <- res[[1L]]
          }

          self$set(name, chunks)
          res
        }
      } else {
        promise_fn <- function(index) {
          # much simpler, this is only used to tracked if the column has been used
          name <- names_bindings[index]
          column <- .subset2(data, index)
          self$set(name, list(column))
          column
        }
      }

      promises <- map(seq_len(ncol(data)), function(.x) expr(promise_fn(!!.x)))
      env_bind_lazy(private$bindings, !!!set_names(promises, names_bindings))

      private$mask <- new_data_mask(private$bindings)
      private$mask$.data <- as_data_pronoun(private$mask)
    },

    set = function(name, chunks) {
      if (inherits(chunks, "dplyr_lazy_vec_chop")) {
        x <- attr(chunks, "x")
        sizes <- attr(chunks, "sizes")

        promise <- function() {
          indices <- NULL
          if (!is.null(sizes)) {
            # would be useful to have something like vec_chop() but that
            # would take a integer vector of sizes instead
            n <- length(sizes)
            indices <- vector(mode = "list", length = n)
            k <- 1
            for (i in seq_len(n)) {
              indices[[i]] <- seq2(k, k + sizes[i] - 1)
              k <- k + sizes[i]
            }
          }
          resolved_chunks <- vec_chop(x, indices)
          self$set(name, resolved_chunks)
          .subset2(resolved_chunks, self$get_current_group())
        }
        # can't use env_bind_lazy() because it evaluates the previous promise if there is one
        delayedAssign(name, promise(), assign.env = private$bindings)

        # so that it appears unresolved
        chunks <- NULL
      }

      private$resolved[name] <- list(chunks)
      private$used <- !map_lgl(private$resolved, is.null)
      private$which_used <- which(private$used)
    },

    remove = function(name) {
      self$set(name, NULL)
      rm(list = name, envir = private$bindings)
    },

    args = function() {
      map(set_names(self$current_vars()), function(.x) expr((!!self)$resolve(!!.x)))
    },

    resolve = function(name) {
      chunks <- self$get_resolved(name)

      if (is.null(chunks)) {
        column <- private$data[[name]]
        chunks <- vec_chop(column, private$rows)
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
