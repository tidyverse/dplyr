set.seed(10)
df <- data.frame(year = 2000:2005, value = (0:5) ^ 2)
scrambled <- df[sample(nrow(df)), ]
wrong <- mutate(scrambled, running = cummax(cumsum(value)))
arrange(wrong, year)
## year value running
## 1 2000     0      30
## 2 2001     1      10
## 3 2002     4      30
## 4 2003     9       9
## 5 2004    16      26
## 6 2005    25      55
right <- mutate(scrambled, running = order_by(year, cummax(cumsum(value))))
arrange(right, year)
## year value running
## 1 2000     0      30
## 2 2001     1      30
## 3 2002     4      30
## 4 2003     9      30
## 5 2004    16      30
## 6 2005    25      55
right2 <- arrange(scrambled,year) %>%mutate(running = cummax(cumsum(value)))
arrange(right2, year)
## year value running
## 1 2000     0       0
## 2 2001     1       1
## 3 2002     4       5
## 4 2003     9      14
## 5 2004    16      30
## 6 2005    25      55
mutate(scrambled, running1 = order_by(year, cumsum(value)), running2 = order_by(year, cummax(running1))) %>% arrange(year)
