library(dplyr)
library(tidyr)

# works
df <- data.frame(key = c("a","b"), value = c(1,2))
df_spread <- df %>% spread(key, value)
mutate_if(df_spread, is.numeric, function(x) {x+1})

# fails with : Error in eval(expr, envir, enclos) : object 'b' not found
df <- data.frame(key = c("a","b-a"), value = c(1,2))
df_spread <- df %>% spread(key, value)
mutate_if(df_spread, is.numeric, function(x) {x+1})

# fails with: Error in parse(text = x) : <text>:1:3: unexpected symbol
df <- data.frame(key = c("a","c d"), value = c(1,2))
df_spread <- df %>% spread(key, value)
mutate_if(df_spread, is.numeric, function(x) {x+1})
