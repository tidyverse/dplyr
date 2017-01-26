func <- function(x) {
  fund <- function(y) {
    mean(y)
  }

  summarize_(x, result = ~fund(a))
}

func(data.frame(a=1:5))
