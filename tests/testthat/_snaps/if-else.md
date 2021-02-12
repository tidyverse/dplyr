# if_else() give meaningful errors

    Code
      if_else(1:10, 1, 2)
    Error <rlang_error>
      `condition` must be a logical vector, not an integer vector.

---

    Code
      if_else(1:3 < 2, 1:2, 1:3)
    Error <rlang_error>
      `true` must be length 3 (length of `condition`) or one, not 2.

---

    Code
      if_else(1:3 < 2, 1:3, 1:2)
    Error <rlang_error>
      `false` must be length 3 (length of `condition`) or one, not 2.

---

    Code
      if_else(1:3 < 2, 1, 1L)
    Error <rlang_error>
      `false` must be a double vector, not an integer vector.

---

    Code
      if_else(1:3 < 2, x, y)
    Error <rlang_error>
      `false` must have class `factor`, not class `ordered/factor`.

