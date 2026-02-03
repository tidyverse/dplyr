This is a large release of dplyr.

We have advanced the deprecation stage of many old functions, which has caused a fairly large amount of breakage. We have been very diligent to reach out to all maintainers over the last month and have provided patches, but unfortunately we still see breakages, since as you are well aware, many developers don't do anything until they get the official warning from CRAN. This work is tracked here https://github.com/tidyverse/dplyr/issues/7763.

We have reduced the amount of non-API calls from:

```
Found non-API calls to R: ‘ATTRIB’, ‘LEVELS’, ‘OBJECT’, ‘PRVALUE’,
  ‘R_shallow_duplicate_attr’, ‘Rf_allocSExp’, ‘SET_ATTRIB’,
  ‘SET_PRCODE’, ‘SET_PRENV’, ‘SET_PRVALUE’
These entry points may be removed soon:
  ‘OBJECT’, ‘R_shallow_duplicate_attr’, ‘LEVELS’
```

down to:

```
Found non-API calls to R: ‘PRVALUE’, ‘Rf_allocSExp’, ‘SET_PRCODE’,
  ‘SET_PRENV’, ‘SET_PRVALUE’
```

To remove these promise creation utilities, we need the binding creation API discussed with Luke, documented here https://gist.github.com/lionel-/1ebcbd5ec69c0775d514c329522408a3, and implemented as a patch here https://github.com/r-devel/r-svn/pull/206.
