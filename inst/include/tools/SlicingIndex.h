#ifndef dplyr_tools_SlicingIndex_H
#define dplyr_tools_SlicingIndex_H

class SlicingIndex {
public:
  virtual int size() const = 0;
  virtual int operator[](int i) const = 0;
  virtual int group() const = 0;
  virtual bool is_identity(SEXP x) const {
    return FALSE;
  };
};

class GroupedSlicingIndex : public SlicingIndex {
public:
  GroupedSlicingIndex(IntegerVector data_) : data(data_), group_index(-1) {}
  GroupedSlicingIndex(IntegerVector data_, int group_) : data(data_), group_index(group_) {}

  virtual int size() const {
    return data.size();
  }

  virtual int operator[](int i) const {
    return data[i];
  }

  virtual int group() const {
    return group_index;
  }

private:
  IntegerVector data;
  int group_index;
};

class RowwiseSlicingIndex : public SlicingIndex {
public:
  RowwiseSlicingIndex(const int start_) : start(start_) {}

  inline int size() const {
    return 1;
  }

  inline int operator[](int i) const {
    return i + start;
  }

  inline int group() const {
    return start;
  }

private:
  int start;
};

class NaturalSlicingIndex : public SlicingIndex {
public:
  NaturalSlicingIndex(const int n_) : n(n_) {}

  virtual int size() const {
    return n;
  }

  virtual int operator[](int i) const {
    return i;
  }

  virtual int group() const {
    return -1;
  }

  virtual bool is_identity(SEXP x) const {
    const R_len_t length = Rf_length(x);
    return length == n;
  }

private:
  int n;
};

class OffsetSlicingIndex : public SlicingIndex {
public:
  OffsetSlicingIndex(const int start_, const int n_) : start(start_), n(n_) {}

  inline int size() const {
    return n;
  }

  inline int operator[](int i) const {
    return i + start;
  }

  inline int group() const {
    return -1;
  }

private:
  int start, n;
};

#endif
