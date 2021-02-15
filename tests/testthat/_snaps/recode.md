# recode() gives meaningful error messages

    Code
      recode(factor("a"), a = 5, .missing = 10)
    Error <rlang_error>
      `.missing` is not supported for factors.

---

    Code
      recode("a", b = 5, "c")
    Error <rlang_error>
      Argument 3 must be named, not unnamed.

---

    Code
      recode(factor("a"), b = 5, "c")
    Error <rlang_error>
      Argument 3 must be named, not unnamed.

---

    Code
      recode(1:5)
    Error <rlang_error>
      No replacements provided.

---

    Code
      recode("a")
    Error <rlang_error>
      No replacements provided.

---

    Code
      recode(factor("a"))
    Error <rlang_error>
      No replacements provided.

