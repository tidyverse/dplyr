devtools::load_all()
library(tidyr)
set.seed(1)
(d1 = data_frame(name = letters[1:3], id = list(1:3, 4:6, 7:9), other_data1 = rnorm(3)))
(d2 = data_frame(name = LETTERS[1:3], id = list(1:3, 4:6, 7:9), other_data2 = rnorm(3)))

#long form
(d1_long = unnest(d1))
(d2_long = unnest(d2))

#full join on long form
full_join(d1_long, d2_long, by = "id")

#full join on nested form
full_join(d1, d2, by = "id")
#not supplying the id produces the same result
#
