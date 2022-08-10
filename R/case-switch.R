case_switch <- function(.x,
                        ...,
                        .default = NULL,
                        .ptype = NULL) {
  args <- list2(...)

  default_env <- caller_env()
  dots_env <- current_env()
  error_call <- current_env()

  args <- case_formula_evaluate(
    args = args,
    default_env = default_env,
    dots_env = dots_env,
    error_call = error_call
  )

  haystacks <- args$lhs
  values <- args$rhs

  vec_case_switch(
    needles = .x,
    haystacks = haystacks,
    values = values,
    needles_arg = ".x",
    haystacks_arg = "",
    values_arg = "",
    default = .default,
    default_arg = ".default",
    ptype = .ptype,
    call = error_call
  )
}
