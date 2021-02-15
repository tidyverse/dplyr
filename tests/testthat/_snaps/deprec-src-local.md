# src_df() is deprecated / errors

    Code
      src_df("base", new.env())
    Error <rlang_error>
      Exactly one of `pkg` and `env` must be non-NULL, not 2.

---

    Code
      src_df()
    Error <rlang_error>
      Exactly one of `pkg` and `env` must be non-NULL, not 0.

---

    Code
      copy_to(src_env, tibble(x = 1), name = "x")
    Error <rlang_error>
      object with `name` = `x` must not already exist, unless `overwrite` = TRUE.

