library(tidyverse)

all <- drake::readd(compare_all)

error <- map_chr(map(all, class), 1) == "try-error"

succeeded <- all[!error]

withr::with_output_sink("revdep/new-problems.md", revdepcheck::revdep_report_problems(results = succeeded))
