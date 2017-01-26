devtools::load_all()

dffun <- function(x) {
  data.frame(a=1, time=3)
}

tmp <- data.frame(d=1:5, e=6:10)

summarize(tmp, d=dffun(b)$time)
summarize(tmp, d=dffun(b)[["time"]])
summarize(tmp, d=dffun(b)$a)
