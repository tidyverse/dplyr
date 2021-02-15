# na_if() gives meaningful errors

    Code
      na_if(1:3, 1:2)
    Error <rlang_error>
      `y` must be length 3 (same as `x`) or one, not 2.

---

    Code
      na_if(1, 1:2)
    Error <rlang_error>
      `y` must be length 1 (same as `x`), not 2.

