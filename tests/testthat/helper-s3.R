local_methods <- function(..., .frame = caller_env()) {
  local_bindings(..., .env = global_env(), .frame = .frame)
}

local_foo_df <- function(frame = caller_env()) {
  local_methods(.frame = frame,
    group_by.foo_df = function(.data, ...) {
      out <- NextMethod()
      if (missing(...)) {
        class(out) <- c("foo_df", class(out))
      } else {
        class(out) <- c("grouped_foo_df", class(out))
      }
      out
    },
    ungroup.grouped_foo_df = function(x, ...) {
      out <- NextMethod()
      class(out) <- c("foo_df", class(out))
      out
    }
  )
}

new_ctor <- function(base_class) {
  function(x = list(), ..., class = NULL) {
    if (inherits(x, "tbl_df")) {
      tibble::new_tibble(x, class = c(class, base_class), nrow = nrow(x))
    } else if (is.data.frame(x)) {
      structure(x, class = c(class, base_class, "data.frame"), ...)
    } else {
      structure(x, class = c(class, base_class), ...)
    }
  }
}

foobar <- new_ctor("dplyr_foobar")
foobaz <- new_ctor("dplyr_foobaz")
quux <- new_ctor("dplyr_quux")

# For testing reconstructing methods that break invariants by adding
# new columns
new_dispatched_quux <- function(x) {
  out <- quux(x)
  out$dispatched <- rep(TRUE, nrow(out))
  out
}
