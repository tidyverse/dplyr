df <- data_frame(id = rep(1:2, each = 4), id2 = rep(1:2, 4))

df %>% group_by(id, id2) %>% distinct(id)
# Source: local data frame [4 x 3]
# Groups: id, id2 [4]
#
# id    id   id2
# <int> <int> <int>
#   1     1     1     1
# 2     1     1     2
# 3     2     2     1
# 4     2     2     2

df %>% group_by(id, id2) %>% select(-id2) %>% distinct(id)
# Adding missing grouping variables: `id2`
# Source: local data frame [4 x 3]
# Groups: id, id2 [4]
#
# id    id   id2
# <int> <int> <int>
#   1     1     1     1
# 2     1     1     2
# 3     2     2     1
# 4     2     2     2
