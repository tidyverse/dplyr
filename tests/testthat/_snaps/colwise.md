# colwise utils gives meaningful error messages

    Code
      tbl_at_vars(iris, raw(3))
    Error <rlang_error>
      `.vars` must be a character/numeric vector or a `vars()` object, not a raw vector.

---

    Code
      tbl_if_vars(iris, list(identity, force), environment())
    Error <rlang_error>
      `.predicate` must have length 1, not 2.

---

    Code
      tbl_if_vars(iris, .funs, environment())
    Error <rlang_error>
      `.predicate` must have length 1, not 2.

