#ifndef dplyr_HASH_H
#define dplyr_HASH_H

namespace std {
  template <>
  struct hash<Rcomplex>
  {
    size_t operator()(const Rcomplex & x) const
    {
		return hash<int>()(x.i);
    }
  };
}

// http://stackoverflow.com/questions/2590677/how-do-i-combine-hash-values-in-c0x
template <class T>
inline void hash_combine(std::size_t& seed, const T& v)
{
    std::hash<T> hasher;
    seed ^= hasher(v) + 0x9e3779b9 + (seed<<6) + (seed>>2);
}

inline std::size_t hash_value(const Rcomplex& cx) {
  std::hash<double> hasher;
  size_t seed = hasher(cx.r);
  hash_combine(seed, hasher(cx.i));
  return seed;
}

#endif
