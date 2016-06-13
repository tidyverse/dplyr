library("devtools")

install.packages("httr", getOption("devtools.revdep.libpath"))

revdep_check(threads = 6)
revdep_check_save_summary()
revdep_check_print_problems()

# revdep_email(date = "June 23", only_problems = TRUE, draft = FALSE)
