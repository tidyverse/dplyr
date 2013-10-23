# Changes from original:
# * generate data in R and load in pandas (ensuring data exactly the same)
# * switch to same gc strategy as R (i.e. do a full collection once before 
#   starting)

import random
import gc
import time
from pandas import *

N = 10000
niter = 2

left = read_csv("left.csv")
right = read_csv("right.csv")

join_methods = ['inner', 'outer', 'left', 'right']
results = DataFrame(index=join_methods, columns=[False, True])

for sort in [False, True]:
    for join_method in join_methods:
        f = lambda: merge(left, right, how=join_method, sort = sort)
        gc.collect()

        start = time.time()
        for _ in range(niter):
            f()
        elapsed = (time.time() - start) / niter
        results[sort][join_method] = elapsed

results.columns = ['dont_sort', 'sort']

results.to_csv("out.csv")
