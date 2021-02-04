# bind_cols repairs names

    Code
      bound <- bind_cols(df, df)
    Message <simpleMessage>
      New names:
      * a -> a...1
      * b -> b...2
      * a -> a...3
      * b -> b...4

# bind_cols() handles unnamed list with name repair (#3402)

    Code
      df <- bind_cols(list(1, 2))
    Message <simpleMessage>
      New names:
      * NA -> ...1
      * NA -> ...2

# *_bind() give meaningful errors

    `.id` must be a scalar string, not a double vector of length 1.

---

    Argument 1 must have names.

---

    Argument 2 must be a data frame or a named atomic vector.

---

    Can't combine `..1$a` <factor<127a2>> and `..2$a` <integer>.

---

    Can't combine `..1$a` <factor<127a2>> and `..2$a` <double>.

---

    Can't combine `..1$b` <double> and `..2$b` <factor<4c40e>>.

---

    Can't combine `..1$b` <double> and `..2$b` <character>.

---

    Can't combine `..1$b` <integer> and `..2$b` <factor<4c40e>>.

---

    Can't combine `..1$b` <integer> and `..2$b` <character>.

---

    Argument 1 must have names.

---

    Can't recycle `a` (size 2) to match `..2` (size 32).

---

    Can't recycle `..1` (size 32) to match `a` (size 3).

