devtools::load_all()
df1 <- data.frame(x = 1:10, y = 1:10)
df2 <- expand.grid(x = 1:10, y = 1:10)

df1g <- df1 %>% group_by(x, y)

df3 <- inner_join(df1g, df2, by = "x")
df3
