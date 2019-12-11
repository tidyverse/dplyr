# workaround so that methods that do not have the .drop argument yet
# don't create the auto mutate .drop column
#
# things like count() and group_by_all()
# can call .group_by_static_drop() instead of group_by()
# so that .drop is only part of the group_by() call if it is FALSE
#
# this is only meant to stay in dplyr until 0.8.0 to give
# implementers of group_by() methods a chance to add .drop in their
# arguments
.group_by_static_drop <- function(..., .drop) {
  if(.drop) {
    group_by(...)
  } else {
    group_by(..., .drop = FALSE)
  }
}
