data1 <- data.frame(var1 = sample(c(1,2,3),50,replace=T), var2 = sample(c("cond1", "cond2"), 50,replace=T),RT = sample(as.numeric(300:1000),50,replace=T))

data1 <- data1 %>%
  group_by(var1) %>%
  mutate(median_var = median(RT[var2=="cond1"]))
