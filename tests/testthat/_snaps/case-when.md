# case_when() give meaningful errors

    `c(TRUE, FALSE) ~ 1:3` must be length 2 or one, not 3.

---

    `c(FALSE, TRUE, FALSE) ~ 2`, `c(FALSE, TRUE, FALSE, NA) ~ 3` must be length 2 or one, not 3, 4.

---

    LHS of case 1 (`50`) must be a logical vector, not a double vector.

---

    Case 1 (`paste(50)`) must be a two-sided formula, not a character vector.

---

    No cases provided.

