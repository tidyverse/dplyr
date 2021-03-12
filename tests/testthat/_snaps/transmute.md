# transmute() error messages

    Code
      transmute(mtcars, cyl2 = cyl, .keep = "all")
    Error <rlang_error>
      `transmute()` does not support the `.keep` argument
    Code
      transmute(mtcars, cyl2 = cyl, .before = disp)
    Error <rlang_error>
      `transmute()` does not support the `.before` argument
    Code
      transmute(mtcars, cyl2 = cyl, .after = disp)
    Error <rlang_error>
      `transmute()` does not support the `.after` argument

