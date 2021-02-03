# arrange() gives meaningful errors

    arrange() failed at implicit mutate() step. 
    x Can't transform a data frame with duplicate names.

---

    arrange() failed at implicit mutate() step. 
    * Problem with `mutate()` input `..1`.
    x object 'y' not found
    i Input `..1` is `y`.

---

    arrange() failed at implicit mutate() step. 
    * Problem with `mutate()` input `..1`.
    x Input `..1` can't be recycled to size 1.
    i Input `..1` is `rep(x, 2)`.
    i Input `..1` must be size 1, not 2.

