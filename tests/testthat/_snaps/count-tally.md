# can only explicitly chain together multiple tallies

    Code
      df %>% count(g, wt = n)
    Output
        g n
      1 1 3
      2 2 7

---

    Code
      df %>% count(g, wt = n) %>% count(wt = n)
    Output
         n
      1 10

---

    Code
      df %>% count(n)
    Message <message>
      Storing counts in `nn`, as `n` already present in input
      i Use `name = "new_name"` to pick a new name.
    Output
        n nn
      1 1  1
      2 2  1
      3 3  1
      4 4  1

