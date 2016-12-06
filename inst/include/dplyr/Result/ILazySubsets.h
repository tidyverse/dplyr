#ifndef dplyr_ILazySubsets_H
#define dplyr_ILazySubsets_H

namespace dplyr {

  class ILazySubsets {
  protected:
    ILazySubsets() {}

  public:
    virtual ~ILazySubsets() {}

    virtual CharacterVector get_variable_names() const = 0;
    virtual SEXP get_variable(SEXP symbol) const = 0;
    virtual bool is_summary(SEXP symbol) const = 0;
    virtual int count(SEXP symbol) const = 0;
    virtual void input(SEXP symbol, SEXP x) = 0;
    virtual int size() const = 0;
    virtual int nrows() const = 0;
  };

}

#endif
