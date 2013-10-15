import random
import gc
import time
from pandas import *
from pandas.compat import range, lrange, StringIO
from pandas.util.testing import rands

N = 10000

indices = np.array([rands(10) for _ in range(N)], dtype='O')
indices2 = np.array([rands(10) for _ in range(N)], dtype='O')
key = np.tile(indices[:8000], 10)
key2 = np.tile(indices2[:8000], 10)

left = DataFrame({'key': key, 'key2': key2,
                  'value': np.random.randn(80000)})
right = DataFrame({'key': indices[2000:], 'key2': indices2[2000:],
                   'value2': np.random.randn(8000)})

join_methods = ['inner', 'outer', 'left', 'right']
results = DataFrame(index=join_methods, columns=[False, True])
niter = 10
for sort in [False, True]:
    for join_method in join_methods:
        f = lambda: merge(left, right, how=join_method, sort=sort)
        gc.disable()
        start = time.time()
        for _ in range(niter):
            f()
        elapsed = (time.time() - start) / niter
        gc.enable()
        results[sort][join_method] = elapsed
results.columns = ['dont_sort', 'sort']

presults = results[['dont_sort']].rename(columns={'dont_sort': 'pandas'})
print presults

