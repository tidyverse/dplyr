#ifndef dplyr_Collecter_H
#define dplyr_Collecter_H

#include <tools/all_na.h>
#include <tools/hash.h>

#include <dplyr/registration.h>
#include <dplyr/vector_class.h>

namespace dplyr {

  static inline bool has_classes(SEXP x) {
    SEXP classes;
    int i, num_classes;
    if (OBJECT(x)) {
      classes = Rf_getAttrib(x, R_ClassSymbol);
      num_classes = Rf_length(classes);
      if (num_classes > 0) {
        return true;
      } else {
        return false;
      }
    }
    return false;
  }

  static inline bool all_logical_na(SEXP x, SEXPTYPE xtype) {
    return LGLSXP == xtype && all_na(x);
  };

  class Collecter {
  public:
    virtual ~Collecter() {};
    virtual void collect(const SlicingIndex& index, SEXP v) = 0;
    virtual SEXP get() = 0;
    virtual bool compatible(SEXP) = 0;
    virtual bool can_promote(SEXP) const = 0;
    virtual bool is_factor_collecter() const {
      return false;
    }
    virtual bool is_logical_all_na() const {
      return false;
    }
    virtual std::string describe() const = 0;
  };

  template <int RTYPE>
  class Collecter_Impl : public Collecter {
  public:
    typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE;

    Collecter_Impl(int n_): data(n_, Rcpp::traits::get_na<RTYPE>()) {}

    void collect(const SlicingIndex& index, SEXP v) {
      if (all_logical_na(v, TYPEOF(v))) {
        collect_logicalNA(index);
      } else {
        collect_sexp(index, v);
      }
    }

    inline SEXP get() {
      return data;
    }

    inline bool compatible(SEXP x) {
      return RTYPE == TYPEOF(x) || all_logical_na(x, TYPEOF(x));
    }

    bool can_promote(SEXP x) const {
      return false;
    }

    std::string describe() const {
      return vector_class<RTYPE>();
    }

    bool is_logical_all_na() const {
      return all_logical_na(data, RTYPE);
    }

  protected:
    Vector<RTYPE> data;

  private:
    void collect_logicalNA(const SlicingIndex& index) {
      for (int i=0; i<index.size(); i++) {
        data[index[i]] = Rcpp::traits::get_na<RTYPE>();
      }
    }

    void collect_sexp(const SlicingIndex& index, SEXP v) {
      Vector<RTYPE> source(v);
      STORAGE* source_ptr = Rcpp::internal::r_vector_start<RTYPE>(source);
      for (int i=0; i<index.size(); i++) {
        data[index[i]] = source_ptr[i];
      }
    }

  };

  template <>
  class Collecter_Impl<REALSXP> : public Collecter {
  public:
    Collecter_Impl(int n_): data(n_, NA_REAL) {}

    void collect(const SlicingIndex& index, SEXP v) {
      NumericVector source(v);
      double* source_ptr = source.begin();
      for (int i=0; i<index.size(); i++) {
        data[index[i]] = source_ptr[i];
      }
    }

    inline SEXP get() {
      return data;
    }

    inline bool compatible(SEXP x) {
      int RTYPE = TYPEOF(x);
      return (RTYPE == REALSXP && !has_classes(x)) ||
             (RTYPE == INTSXP && !has_classes(x)) ||
             all_logical_na(x, RTYPE);
    }

    bool can_promote(SEXP x) const {
      return false;
    }

    std::string describe() const {
      return "numeric";
    }

  protected:
    NumericVector data;

  };

  template <>
  class Collecter_Impl<STRSXP> : public Collecter {
  public:
    Collecter_Impl(int n_): data(n_, NA_STRING) {}

    void collect(const SlicingIndex& index, SEXP v) {
      if (TYPEOF(v) == STRSXP) {
        collect_strings(index, v);
      } else if (Rf_inherits(v, "factor")) {
        collect_factor(index, v);
      } else if (all_logical_na(v, TYPEOF(v))) {
        collect_logicalNA(index, v);
      } else {
        CharacterVector vec(v);
        collect_strings(index, vec);
      }
    }

    inline SEXP get() {
      return data;
    }

    inline bool compatible(SEXP x) {
      return (STRSXP == TYPEOF(x)) || Rf_inherits(x, "factor") || all_logical_na(x, TYPEOF(x));
    }

    bool can_promote(SEXP x) const {
      return false;
    }

    std::string describe() const {
      return "character";
    }

  protected:
    CharacterVector data;

  private:

    void collect_logicalNA(const SlicingIndex& index, LogicalVector source) {
      SEXP* p_data   = Rcpp::internal::r_vector_start<STRSXP>(data);
      int n = index.size();
      for (int i=0; i<n; i++) {
        p_data[index[i]] = NA_STRING;
      }
    }

    void collect_strings(const SlicingIndex& index, CharacterVector source) {
      SEXP* p_source = Rcpp::internal::r_vector_start<STRSXP>(source);
      SEXP* p_data   = Rcpp::internal::r_vector_start<STRSXP>(data);
      int n = index.size();
      for (int i=0; i<n; i++) {
        p_data[index[i]] = p_source[i];
      }
    }

    void collect_factor(const SlicingIndex& index, IntegerVector source) {
      CharacterVector levels = source.attr("levels");
      for (int i=0; i<index.size(); i++) {
        if (source[i] == NA_INTEGER) {
          data[index[i]] = NA_STRING;
        } else {
          data[index[i]] = levels[source[i]-1];
        }
      }
    }

  };

  template <>
  class Collecter_Impl<INTSXP> : public Collecter {
  public:
    Collecter_Impl(int n_): data(n_, NA_INTEGER) {}

    void collect(const SlicingIndex& index, SEXP v) {
      IntegerVector source(v);
      int* source_ptr = source.begin();
      for (int i=0; i<index.size(); i++) {
        data[index[i]] = source_ptr[i];
      }
    }

    inline SEXP get() {
      return data;
    }

    inline bool compatible(SEXP x) {
      int RTYPE = TYPEOF(x);
      return (INTSXP == RTYPE && !has_classes(x)) || all_logical_na(x, RTYPE);
    }

    bool can_promote(SEXP x) const {
      return TYPEOF(x) == REALSXP && !has_classes(x);
    }

    std::string describe() const {
      return "integer";
    }

  protected:
    IntegerVector data;

  };

  template <int RTYPE>
  class TypedCollecter : public Collecter_Impl<RTYPE> {
  public:
    TypedCollecter(int n, SEXP types_) :
      Collecter_Impl<RTYPE>(n), types(types_) {}

    inline SEXP get() {
      Collecter_Impl<RTYPE>::data.attr("class") = types;
      return Collecter_Impl<RTYPE>::data;
    }

    inline bool compatible(SEXP x) {
      String type = STRING_ELT(types,0);
      return Rf_inherits(x, type.get_cstring()) || all_logical_na(x, TYPEOF(x));
    }

    inline bool can_promote(SEXP x) const {
      return false;
    }

    std::string describe() const {
      return collapse<STRSXP>(types);
    }

  private:
    SEXP types;
  };

  class POSIXctCollecter : public Collecter_Impl<REALSXP> {
  public:
    typedef Collecter_Impl<REALSXP> Parent;

    POSIXctCollecter(int n, SEXP tz_) :
      Parent(n), tz(tz_) {}

    void collect(const SlicingIndex& index, SEXP v) {
      if (Rf_inherits(v, "POSIXct")) {
        Parent::collect(index, v);
        update_tz(v);
      } else if (all_logical_na(v, TYPEOF(v))) {
        Parent::collect(index, v);
      }
    }

    inline SEXP get() {
      Parent::data.attr("class") = get_time_classes();
      if (!tz.isNULL()) {
        Parent::data.attr("tzone") = tz;
      }
      return Parent::data;
    }

    inline bool compatible(SEXP x) {
      return Rf_inherits(x, "POSIXct") || all_logical_na(x, TYPEOF(x));
    }

    inline bool can_promote(SEXP x) const {
      return false;
    }

    std::string describe() const {
      return collapse<STRSXP>(get_time_classes());
    }

  private:
    RObject tz;

    void update_tz(SEXP v) {
      RObject v_tz(Rf_getAttrib(v, Rf_install("tzone")));
      // if the new tz is NULL, keep previous value
      if (v_tz.isNULL()) return;

      if (tz.isNULL()) {
        // if current tz is NULL, grab the new one
        tz = v_tz;
      } else {
        // none are NULL, so compare them
        // if they are equal, fine
        if (STRING_ELT(tz, 0) == STRING_ELT(v_tz,0)) return;

        // otherwise, settle to UTC
        tz = wrap("UTC");
      }
    }

  };

  class FactorCollecter : public Collecter {
  public:
    typedef dplyr_hash_map<SEXP,int> LevelsMap;

    FactorCollecter(int n, SEXP model_):
      data(n, IntegerVector::get_na()),
      model(model_),
      levels(Rf_getAttrib(model, Rf_install("levels"))),
      levels_map()
    {
      int nlevels = levels.size();
      for (int i=0; i<nlevels; i++) levels_map[ levels[i] ] = i + 1;
    }

    bool is_factor_collecter() const {
      return true;
    }

    void collect(const SlicingIndex& index, SEXP v) {
      if (Rf_inherits(v, "factor") && has_same_levels_as(v)) {
        collect_factor(index, v);
      } else if (all_logical_na(v, TYPEOF(v))) {
        collect_logicalNA(index);
      }
    }

    inline SEXP get() {
      data.attr("levels") = levels;
      data.attr("class") = model.attr("class");
      return data;
    }

    inline bool compatible(SEXP x) {
      return ((Rf_inherits(x, "factor") && has_same_levels_as(x)) ||
              all_logical_na(x, TYPEOF(x)));
    }

    inline bool can_promote(SEXP x) const {
      return TYPEOF(x) == STRSXP || Rf_inherits(x, "factor");
    }

    inline bool has_same_levels_as(SEXP x) const {
      CharacterVector levels_other = Rf_getAttrib(x, Rf_install("levels"));

      int nlevels = levels_other.size();
      if (nlevels != (int)levels_map.size()) return false;

      for (int i=0; i<nlevels; i++)
        if (! levels_map.count(levels_other[i]))
          return false;
      return true;
    }

    inline std::string describe() const {
      return "factor";
    }

  private:
    IntegerVector data;
    RObject model;
    CharacterVector levels;
    LevelsMap levels_map;

    void collect_factor(const SlicingIndex& index, SEXP v) {
      // here we can assume that v is a factor with the right levels
      // we however do not assume that they are in the same order
      IntegerVector source(v);
      CharacterVector levels = source.attr("levels");

      SEXP* levels_ptr = Rcpp::internal::r_vector_start<STRSXP>(levels);
      int* source_ptr = Rcpp::internal::r_vector_start<INTSXP>(source);
      for (int i=0; i<index.size(); i++) {
        if (source_ptr[i] == NA_INTEGER) {
          data[ index[i] ] = NA_INTEGER;
        } else {
          SEXP x = levels_ptr[ source_ptr[i] - 1 ];
          data[ index[i] ] = levels_map.find(x)->second;
        }
      }
    }

    void collect_logicalNA(const SlicingIndex& index) {
      for (int i=0; i<index.size();i++) {
        data[ index[i] ] = NA_INTEGER;
      }
    }
  };

  template <>
  inline bool Collecter_Impl<LGLSXP>::can_promote(SEXP x) const {
    return is_logical_all_na();
  }

  inline Collecter* collecter(SEXP model, int n) {
    switch (TYPEOF(model)) {
    case INTSXP:
      if (Rf_inherits(model, "POSIXct"))
        return new POSIXctCollecter(n, Rf_getAttrib(model, Rf_install("tzone")));
      if (Rf_inherits(model, "factor"))
        return new FactorCollecter(n, model);
      if (Rf_inherits(model, "Date"))
        return new TypedCollecter<INTSXP>(n, get_date_classes());
      if (has_classes(model)) {
        SEXP classes = Rf_getAttrib(model, R_ClassSymbol);
        Rf_warning("Vectorizing '%s' elements may not preserve their attributes",
                   CHAR(STRING_ELT(classes, 0)));
        return new TypedCollecter<INTSXP>(n, classes);
      }
      return new Collecter_Impl<INTSXP>(n);
    case REALSXP:
      if (Rf_inherits(model, "POSIXct"))
        return new POSIXctCollecter(n, Rf_getAttrib(model, Rf_install("tzone")));
      if (Rf_inherits(model, "Date"))
        return new TypedCollecter<REALSXP>(n, get_date_classes());
      if (has_classes(model)) {
        SEXP classes = Rf_getAttrib(model, R_ClassSymbol);
        Rf_warning("Vectorizing '%s' elements may not preserve their attributes",
                   CHAR(STRING_ELT(classes, 0)));
        return new TypedCollecter<REALSXP>(n, classes);
      }
      return new Collecter_Impl<REALSXP>(n);
    case CPLXSXP:
      return new Collecter_Impl<CPLXSXP>(n);
    case LGLSXP:
      return new Collecter_Impl<LGLSXP>(n);
    case STRSXP:
      return new Collecter_Impl<STRSXP>(n);
    case VECSXP:
      if (Rf_inherits(model, "POSIXlt")) {
        stop("POSIXlt not supported");
      }
      return new Collecter_Impl<VECSXP>(n);
    default:
      break;
    }

    stop("Unsupported vector type %s", Rf_type2char(TYPEOF(model)));
    return 0;
  }

  inline Collecter* promote_collecter(SEXP model, int n, Collecter* previous) {
    // handle the case where the previous collecter was a
    // Factor collecter and model is a factor. when this occurs, we need to
    // return a Collecter_Impl<STRSXP> because the factors don't have the
    // same levels
    if (Rf_inherits(model, "factor") && previous->is_factor_collecter()) {
      Rf_warning("Unequal factor levels: coercing to character");
      return new Collecter_Impl<STRSXP>(n);
    }

    // logical NA can be promoted to whatever type comes next
    if (previous->is_logical_all_na()) {
      return collecter(model, n);
    }

    switch (TYPEOF(model)) {
    case INTSXP:
      if (Rf_inherits(model, "Date"))
        return new TypedCollecter<INTSXP>(n, get_date_classes());
      if (Rf_inherits(model, "factor"))
        return new Collecter_Impl<STRSXP>(n);
      if (has_classes(model)) {
        SEXP classes = Rf_getAttrib(model, R_ClassSymbol);
        Rf_warning("Promoting class %s into %s may lose attributes",
                   previous->describe().c_str(),
                   get_single_class(model).c_str());
        return new TypedCollecter<INTSXP>(n, classes);
      }
      return new Collecter_Impl<INTSXP>(n);
    case REALSXP:
      if (Rf_inherits(model, "POSIXct"))
        return new POSIXctCollecter(n, Rf_getAttrib(model, Rf_install("tzone")));
      if (Rf_inherits(model, "Date"))
        return new TypedCollecter<REALSXP>(n, get_date_classes());
      if (has_classes(model)) {
        SEXP classes = Rf_getAttrib(model, R_ClassSymbol);
        Rf_warning("Promoting class %s into %s may lose attributes",
                   previous->describe().c_str(),
                   get_single_class(model).c_str());
        return new TypedCollecter<REALSXP>(n, classes);
      }
      return new Collecter_Impl<REALSXP>(n);
    case LGLSXP:
      return new Collecter_Impl<LGLSXP>(n);
    case STRSXP:
      if (previous->is_factor_collecter())
        Rf_warning("binding factor and character vector, coercing into character vector");
      return new Collecter_Impl<STRSXP>(n);
    default:
      break;
    }
    stop("Unsupported vector type %s", Rf_type2char(TYPEOF(model)));
    return 0;
  }

}

#endif
