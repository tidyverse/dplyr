library(dplyr)

df_1 <- data_frame(a = as.integer(1:3), b = runif(3))
df_2 <- data_frame(a = as.factor(1:3), c = runif(3))
left_join(df_1, df_2)
