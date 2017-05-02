library("devtools")

revdep_check(threads = parallel::detectCores(), skip = "broom")
revdep_check_save_summary()
revdep_check_print_problems()

# revdep_email(date = "May 11", draft = FALSE)
