#ifndef dplyr_hybrid_dispatch_h
#define dplyr_hybrid_dispatch_h

namespace dplyr{
namespace hybrid{

struct Summary {
  template <typename T>
  inline SEXP operator()(const T& obj) const{
    return obj.summarise();
  }
};

struct Window {
  template <typename T>
  inline SEXP operator()(const T& obj) const{
    return obj.window();
  }
};

}
}


#endif
