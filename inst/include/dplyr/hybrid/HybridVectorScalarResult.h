#ifndef DPLYR_HYBRID_HybridVectorScalarResult_H
#define DPLYR_HYBRID_HybridVectorScalarResult_H

namespace dplyr {
namespace hybrid {

template <int RTYPE, typename Data, typename Impl>
class HybridVectorScalarResult {
public:
  typedef typename Rcpp::Vector<RTYPE> Vec ;
  typedef typename Data::group_iterator group_iterator ;
  typedef typename Data::slicing_index Index;
  typedef typename Vec::stored_type stored_type;

  HybridVectorScalarResult(const Data& data_) :
    data(data_)
  {}

  inline Vec summarise() const {
    int ng = data.ngroups();

    Vec vec = init(ng);

    group_iterator git = data.group_begin();
    for (int i = 0; i < ng; i++, ++git) {
      vec[i] = self()->process(*git);
    }

    return vec ;
  }

  inline Vec window() const {
    int ng = data.ngroups();
    int nr = data.nrows();

    Vec vec = init(nr);

    group_iterator git = data.group_begin();
    for (int i = 0; i < ng; i++, ++git) {
      const Index& indices = *git;
      stored_type res = self()->process(indices);

      int ni = indices.size();
      for (int j = 0; j < ni; j++) {
        vec[indices[j]] = res;
      }
    }

    return vec ;
  }


private:
  const Data& data;

  inline const Impl* self() const {
    return static_cast<const Impl*>(this);
  }

  inline Vec init(int n) const {
    return Rcpp::no_init(n);
  }

};

}
}



#endif
