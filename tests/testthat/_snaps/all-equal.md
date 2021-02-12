# count() give meaningful errors

    Code
      union(tibble(a = 1), tibble(a = "1"))
    Error <rlang_error>
      not compatible: 
      - Incompatible types for column `a`: double vs character

---

    Code
      union(tibble(a = 1, b = 2), tibble(a = "1", b = "2"))
    Error <rlang_error>
      not compatible: 
      - Incompatible types for column `a`: double vs character
      - Incompatible types for column `b`: double vs character

