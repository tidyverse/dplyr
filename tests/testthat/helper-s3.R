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
