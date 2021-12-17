# check_pkg() give meaningful error messages

    Code
      (expect_error(dplyr:::check_pkg("`__foobarbaz__`", "foobar a baz")))
    Output
      <error/rlang_error>
      Error in `is_installed()`:
      ! Must supply valid package names.
      x Problematic names:
      * "`__foobarbaz__`"
    Code
      (expect_error(dplyr:::check_pkg("`__foobarbaz__`", "foobar a baz", install = FALSE))
      )
    Output
      <error/rlang_error>
      Error in `is_installed()`:
      ! Must supply valid package names.
      x Problematic names:
      * "`__foobarbaz__`"

