# set operation give useful error message. #903

    Code
      intersect(alfa, beta)
    Error <rlang_error>
      not compatible: 
      not compatible: 
      - Cols in y but not x: `data2`.
      - Cols in x but not y: `data`.

---

    Code
      union(alfa, beta)
    Error <rlang_error>
      not compatible: 
      not compatible: 
      - Cols in y but not x: `data2`.
      - Cols in x but not y: `data`.

---

    Code
      setdiff(alfa, beta)
    Error <rlang_error>
      not compatible: 
      not compatible: 
      - Cols in y but not x: `data2`.
      - Cols in x but not y: `data`.

