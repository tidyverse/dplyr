require(dplyr, quietly = TRUE)
require(methods, quietly = TRUE)
require(microbenchmark, quietly = TRUE)

summarise_ <- dplyr:::summarise_
summarise_( iris, s = mean(Sepal.Length) )

