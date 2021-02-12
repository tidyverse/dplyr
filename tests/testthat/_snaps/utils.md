# check_pkg() give meaningful error messages

    Code
      dplyr:::check_pkg("`__foobarbaz__`", "foobar a baz")
    Error <rlang_error>
      The `__foobarbaz__` package is required to foobar a baz.
      Please install it with `install.packages("`__foobarbaz__`")`

---

    Code
      dplyr:::check_pkg("`__foobarbaz__`", "foobar a baz", install = FALSE)
    Error <rlang_error>
      The `__foobarbaz__` package is required to foobar a baz.

