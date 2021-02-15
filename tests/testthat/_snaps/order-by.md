# order_by() gives useful error messages

    Code
      order_by(mtcars, 10)
    Error <rlang_error>
      `call` must be a function call, not a double vector.

---

    Code
      order_by(mtcars, cyl)
    Error <rlang_error>
      `call` must be a function call, not a symbol.
      * Did you mean `arrange(mtcars, cyl)`?

