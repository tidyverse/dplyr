# check_pkg() give meaningful error messages

    Code
      (expect_error(dplyr:::check_pkg("`__foobarbaz__`", "foobar a baz")))
    Output
      <error/rlang_error>
      Error: The `__foobarbaz__` package is required to foobar a baz.
      i Please install it with `install.packages("`__foobarbaz__`")`.
    Code
      (expect_error(dplyr:::check_pkg("`__foobarbaz__`", "foobar a baz", install = FALSE))
      )
    Output
      <error/rlang_error>
      Error: The `__foobarbaz__` package is required to foobar a baz.

