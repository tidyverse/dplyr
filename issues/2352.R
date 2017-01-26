x1 <- data_frame(
  a = 1:5,
  b = 6:10,
  c = 11:15,
  d = 16:20
)
names(x1) <- c("a", "b", "b", "b")

# Same df but as a data.frame
x2 <- data.frame(
  a = 1:5,
  b = 6:10,
  b = 11:15,
  b = 16:20,
  check.names = FALSE
)

y <- data.frame(
  a = 1:4,
  d = letters[1:4]
)

# the join is completed on the tibble but the first b column values displace the others:
left_join(x1, y, by = "a")
