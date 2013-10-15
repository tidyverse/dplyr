library(plyr)
library(data.table)
library(dplyr)

N <- 10000
indices = rep(NA, N)
indices2 = rep(NA, N)
for (i in 1:N) {
  indices[i] <- paste(sample(letters, 10), collapse="")
  indices2[i] <- paste(sample(letters, 10), collapse="")
}
left <- data.frame(key=rep(indices[1:8000], 10),
                   key2=rep(indices2[1:8000], 10),
                   value=rnorm(80000))
right <- data.frame(key=indices[2001:10000],
                    key2=indices2[2001:10000],
                    value2=rnorm(8000))

right2 <- data.frame(key=rep(right$key, 2),
                     key2=rep(right$key2, 2),
                     value2=rnorm(16000))

rownames(left) <- NULL
rownames(right) <- NULL
rownames(right2) <- NULL

left.dt <- data.table(left, key=c("key", "key2"))
right.dt <- data.table(right, key=c("key", "key2"))

timeit <- function(func, niter=10) {
  timing = rep(NA, niter)
  for (i in 1:niter) {
    gc()
    timing[i] <- system.time(func())[3]
  }
  mean(timing)
}

left.join <- function(sort=FALSE) {
  result <- base::merge(left, right, all.x=TRUE, sort=sort)
}

right.join <- function(sort=FALSE) {
  result <- base::merge(left, right, all.y=TRUE, sort=sort)
}

# outer.join <- function(sort=FALSE) {
#   result <- base::merge(left, right, all=TRUE, sort=sort)
# }

inner.join <- function(sort=FALSE) {
  result <- base::merge(left, right, all=FALSE, sort=sort)
}

left.join.dt <- function(sort=FALSE) {
  result <- right.dt[left.dt]
}

right.join.dt <- function(sort=FALSE) {
  result <- left.dt[right.dt]
}

# outer.join.dt <- function(sort=FALSE) {
#   result <- merge(left.dt, right.dt, all=TRUE, sort=sort)
# }

inner.join.dt <- function(sort=FALSE) {
  result <- merge(left.dt, right.dt, all=FALSE, sort=sort)
}

results <- matrix(nrow=3, ncol=4)
colnames(results) <- c("pandas", "base::merge", "data.table", "dplyr" )
rownames(results) <- c("inner", "left", "right")

inner_join_impl <- dplyr:::inner_join_impl
left_join_impl  <- dplyr:::left_join_impl

dplyr.inner.join <- function(){
   inner_join_impl(left, right)         
}
dplyr.left.join <- function(){
   left_join_impl(left, right)         
}
dplyr.right.join <- function(){
   left_join_impl(right, left)         
}

base.functions  <- c(inner.join, left.join, right.join)
dplyr.functions <- c(dplyr.inner.join, dplyr.left.join, dplyr.right.join)
dt.functions <- c(inner.join.dt, left.join.dt, right.join.dt)

for (i in 1:3) {
  base.func     <- base.functions[[i]]
  dplyr.func    <- dplyr.functions[[i]]
  dt.func       <- dt.functions[[i]]
  results[i, 2] <- timeit(base.func)
  results[i, 3] <- timeit(dt.func)
  results[i, 4] <- timeit(dplyr.func)
}
pandas.txt <- system( "python pandas/bench_merge.py", intern = TRUE )
pandas     <- read.table( textConnection( pandas.txt ) )
results[,1] <- pandas[c(1,3:4),1]
round(results)

