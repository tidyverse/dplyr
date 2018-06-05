#ifndef dplyr_GroupedCallReducer_H
#define dplyr_GroupedCallReducer_H

#include <dplyr/Result/CallbackProcessor.h>
#include <dplyr/DataMask.h>

namespace dplyr {

template <typename Data>
class GroupedCallReducer : public CallbackProcessor< Data, GroupedCallReducer<Data> > {
public:
  typedef typename Data::slicing_index Index ;

  GroupedCallReducer( SEXP expr_, SymbolString name_, DataMask<Data>& data_mask_) :
    expr(expr_),
    name(name_),
    data_mask(data_mask_)
  {}

  virtual ~GroupedCallReducer() {};

  inline SEXP process_chunk(const Index& indices) {
    return data_mask.eval(expr, indices);
  }

  const SymbolString& get_name() const {
    return name;
  }

private:
  SEXP expr;
  const SymbolString name;
  DataMask<Data> data_mask;
};

} // namespace dplyr

#endif
