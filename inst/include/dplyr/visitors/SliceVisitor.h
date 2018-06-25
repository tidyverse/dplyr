#ifndef dplyr_visitors_SliceVisitor_h
#define dplyr_visitors_SliceVisitor_h

namespace dplyr{
namespace visitors{

template <typename Vector, typename Index>
class SliceVisitor {
public:
  typedef typename Vector::stored_type STORAGE;

  SliceVisitor(const Vector& data_, const Index& index_) :
    data(data_),
    index(index_)
  {}

  inline STORAGE operator[](int i) const {
    return data[index[i]];
  }

  inline int size() const {
    return index.size();
  }

private:
  const Vector& data;
  const Index& index;
};


}
}
#endif
