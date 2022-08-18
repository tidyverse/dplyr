vec_case_match <- function(needles,
                           haystacks,
                           values,
                           ...,
                           needles_arg = "needles",
                           haystacks_arg = "haystacks",
                           values_arg = "values",
                           default = NULL,
                           default_arg = "default",
                           ptype = NULL,
                           call = current_env()) {
  check_dots_empty0(...)

  vec_assert(needles, arg = needles_arg, call = call)
  vec_check_list(haystacks, arg = haystacks_arg, call = call)
  list_check_all_vectors(haystacks, arg = haystacks_arg, call = call)

  haystacks <- vec_cast_common(
    !!!haystacks,
    .to = needles,
    .arg = haystacks_arg,
    .call = call
  )

  # Could be more efficient in C. Build a dictionary on `needles`
  # once and then reuse it with each haystack
  conditions <- map(haystacks, vec_in, needles = needles)

  size <- vec_size(needles)

  vec_case_when(
    conditions = conditions,
    values = values,
    conditions_arg = "",
    values_arg = values_arg,
    default = default,
    default_arg = default_arg,
    ptype = ptype,
    size = size,
    call = call
  )
}
