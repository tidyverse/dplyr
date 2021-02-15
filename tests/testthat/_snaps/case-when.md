# case_when() give meaningful errors

    Code
      case_when(c(TRUE, FALSE) ~ 1:3, c(FALSE, TRUE) ~ 1:2)
    Error <rlang_error>
      `c(TRUE, FALSE) ~ 1:3` must be length 2 or one, not 3.

---

    Code
      case_when(c(TRUE, FALSE) ~ 1, c(FALSE, TRUE, FALSE) ~ 2, c(FALSE, TRUE, FALSE,
        NA) ~ 3)
    Error <rlang_error>
      `c(FALSE, TRUE, FALSE) ~ 2`, `c(FALSE, TRUE, FALSE, NA) ~ 3` must be length 2 or one, not 3, 4.

---

    Code
      case_when(50 ~ 1:3)
    Error <rlang_error>
      LHS of case 1 (`50`) must be a logical vector, not a double vector.

---

    Code
      case_when(paste(50))
    Error <rlang_error>
      Case 1 (`paste(50)`) must be a two-sided formula, not a character vector.

---

    Code
      case_when()
    Error <rlang_error>
      No cases provided.

