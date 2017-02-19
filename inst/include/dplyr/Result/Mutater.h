#ifndef dplyr_Result_Mutater_H
#define dplyr_Result_Mutater_H

#include <dplyr/Result/Result.h>

namespace dplyr {

  template <int RTYPE, typename Derived>
  class Mutater : public Result {
  public:

    virtual SEXP process(const GroupedDataFrame& gdf) {
      return process_slices(gdf);
    }

    virtual SEXP process(const RowwiseDataFrame& gdf) {
      return process_slices(gdf);
    }

    virtual SEXP process(const FullDataFrame& df) {
      return process_slices(df);
    }

    virtual SEXP process(const SlicingIndex& index) {
      int nrows = index.size();
      Vector<RTYPE> out = no_init(nrows);
      NaturalSlicingIndex fake(nrows);
      static_cast<Derived&>(*this).process_slice(out, index, fake);
      return out;
    }

  private:
    template <class Data>
    SEXP process_slices(const Data& gdf) {
      int ng = gdf.ngroups();

      Vector<RTYPE> out = no_init(gdf.nrows());
      typename Data::group_iterator git = gdf.group_begin();
      for (int i = 0; i < ng; ++i, ++git) {
        static_cast<Derived&>(*this).process_slice(out, *git, *git);
      }
      return out;
    }
  };

}

#endif
