#dplyr 0.5.0
library(dplyr)

test_data = data.frame(
  grp = rep(c("A", "B"), each = 4),
  y = rnorm(8), stringsAsFactors = F
)

#this works
test_data %>% group_by(grp) %>%
  mutate(
    cdf = ecdf(y)(y)
  )

#this throws an error:  Error: object 'y' not found
test_data %>% group_by(grp) %>%
  mutate(
    surv = 1 - ecdf(y)(y)
  )

#but this works
custom_fun = function(input) 1 - ecdf(input)(input)

test_data %>% group_by(grp) %>%
  mutate(
    surv = custom_fun(y)
  )

# example with wilcox.test

test_data2 = data.frame(
  grp = rep(c("A", "B"), each = 4),
  grp2 = rep(c("C", "D"), 4),
  y = rnorm(8), stringsAsFactors = F
)

test_data2 %>% group_by(grp) %>%
  mutate(
    p_value = 1 - wilcox.test(y)$p.value
  )

# Error: object 'y' not found
test_data2 %>% group_by(grp) %>%
  mutate(
    p_value = wilcox.test(y ~ grp2)$p.value
  )

wilcox_fun = function(outcome, group) wilcox.test(outcome ~ factor(group))$p.value

test_data2 %>% group_by(grp) %>%
  mutate(
    p_value = wilcox_fun(y, grp2)
  )
