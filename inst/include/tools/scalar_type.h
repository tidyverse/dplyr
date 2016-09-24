#ifndef DPLYR_SCALAR_TYPE_H
#define DPLYR_SCALAR_TYPE_H

template <int RTYPE>
struct scalar_type {
  typedef typename traits::storage_type<RTYPE>::type type;
};
template <>
struct scalar_type<STRSXP> {
  typedef String type;
};

#endif //DPLYR_SCALAR_TYPE_H
