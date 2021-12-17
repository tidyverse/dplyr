# check_pkg() give meaningful error messages

    Code
      (expect_error(dplyr:::check_pkg("dplyrFooBarBaz", "foobar a baz")))
    Output
      <error/rlang_error>
      Error in `dplyr:::check_pkg()`:
      ! The dplyrFooBarBaz package is required to foobar a baz.
      i Please install it with `install.packages("dplyrFooBarBaz")`.
    Code
      (expect_error(dplyr:::check_pkg("dplyrFooBarBaz", "foobar a baz", install = FALSE))
      )
    Output
      <error/rlang_error>
      Error in `dplyr:::check_pkg()`:
      ! The dplyrFooBarBaz package is required to foobar a baz.

