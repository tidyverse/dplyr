#ifndef dplyr_tools_FullDataFrame_H
#define dplyr_tools_FullDataFrame_H

namespace dplyr {

  class FullDataFrame;

  class FullDataFrameIndexIterator {
  public:
    FullDataFrameIndexIterator(const NaturalSlicingIndex& i_) : i(i_) {}

    FullDataFrameIndexIterator& operator++() {
      return *this;
    }

    NaturalSlicingIndex operator*() const {
      return i;
    }

    const NaturalSlicingIndex& i;
  };

  class FullDataFrame {
  public:
    typedef FullDataFrameIndexIterator group_iterator;
    typedef NaturalSlicingIndex slicing_index;

    explicit FullDataFrame(const DataFrame& data) : data_(data), index(data.nrows()) {}

    const SlicingIndex& get_index() const {
      return index;
    }

    group_iterator group_begin() const {
      return FullDataFrameIndexIterator(index);
    }

    DataFrame& data() {
      return data_;
    }
    const DataFrame& data() const {
      return data_;
    }

    inline int ngroups() const {
      return 1;
    }

    inline int nvars() const {
      return 0;
    }

    inline SEXP symbol(int i) {
      return R_NilValue;
    }

    inline SEXP label(int i) {
      return R_NilValue;
    }

    inline int nrows() const {
      return index.size();
    }

    inline int max_group_size() const {
      return nrows();
    }

  private:
    DataFrame data_;
    NaturalSlicingIndex index;
  };

}
#endif
