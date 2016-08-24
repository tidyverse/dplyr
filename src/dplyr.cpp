#include <dplyr.h>

using namespace Rcpp ;
using namespace dplyr ;

typedef dplyr_hash_map<SEXP,HybridHandler> HybridHandlerMap ;

bool has_no_class(const RObject& arg) {
  return RCPP_GET_CLASS(arg) == R_NilValue ;
}

bool hybridable(RObject arg) {
  if (Rf_inherits(arg, "Date") || Rf_inherits(arg, "POSIXct") || Rf_inherits(arg, "difftime")) return true ;

  if (arg.isObject() || arg.isS4()) return false ;
  int type = arg.sexp_type() ;
  switch (type) {
  case INTSXP:
  case REALSXP:
  case LGLSXP:
  case STRSXP:
  case CPLXSXP:
  case RAWSXP:
    return has_no_class(arg) ;
  default:
    break ;
  }
  return false ;
}

template <template <int,bool> class Fun, bool narm>
Result* simple_prototype_impl(SEXP arg, bool is_summary) {
  // if not hybridable, just let R handle it
  if (!hybridable(arg)) return 0 ;

  switch (TYPEOF(arg)) {
  case INTSXP:
    return new Fun<INTSXP,narm>(arg, is_summary) ;
  case REALSXP:
    return new Fun<REALSXP,narm>(arg, is_summary) ;
  default:
    break ;
  }
  return 0 ;
}

template <template <int,bool> class Fun>
Result* simple_prototype(SEXP call, const LazySubsets& subsets, int nargs) {
  if (nargs == 0) return 0 ;
  SEXP arg = CADR(call) ;
  bool is_summary = false ;
  if (TYPEOF(arg) == SYMSXP) {
    if (subsets.count(arg)) {
      // we have a symbol from the data - great
      is_summary = subsets.is_summary(arg) ;
      arg = subsets.get_variable(arg) ;
    } else {
      // we have a symbol but we don't know about it, so we give up and let R evaluation handle it
      return 0 ;
    }
  } else {
    // anything else: expressions, constants ...
    // workaround for now : we just let R deal with it
    // of course this needs some specializations, i.e. sum(1) does not need R to get involved
    return 0 ;
  }

  if (nargs == 1) {
    return simple_prototype_impl<Fun, false>(arg, is_summary) ;
  } else if (nargs == 2) {
    SEXP arg2 = CDDR(call) ;
    // we know how to handle fun( ., na.rm = TRUE/FALSE )
    if (TAG(arg2) == R_NaRmSymbol) {
      SEXP narm = CAR(arg2) ;
      if (TYPEOF(narm) == LGLSXP && LENGTH(narm) == 1) {
        if (LOGICAL(narm)[0] == TRUE) {
          return simple_prototype_impl<Fun, true>(arg, is_summary) ;
        } else {
          return simple_prototype_impl<Fun, false>(arg, is_summary) ;
        }
      }
    }
  }
  return 0 ;
}

template< template <int, bool> class Tmpl, bool narm>
Result* minmax_prototype_impl(SEXP arg, bool is_summary) {
  if (!hybridable(arg)) return 0 ;

  switch (TYPEOF(arg)) {
  case INTSXP:
    return new Tmpl<INTSXP,narm>(arg, is_summary) ;
  case REALSXP:
    return new Tmpl<REALSXP,narm>(arg, is_summary) ;
  default:
    break ;
  }
  return 0 ;
}

template< template <int, bool> class Tmpl>
Result* minmax_prototype(SEXP call, const LazySubsets& subsets, int nargs) {
  using namespace dplyr ;
  // we only can handle 1 or two arguments
  if (nargs == 0 || nargs > 2) return 0 ;

  // the first argument is the data to operate on
  SEXP arg = CADR(call) ;

  bool is_summary = false ;
  if (TYPEOF(arg) == SYMSXP) {
    if (subsets.count(arg)) {
      is_summary = subsets.is_summary(arg) ;
      arg = subsets.get_variable(arg) ;
    }
    else return 0 ;
  } else {
    return 0 ;
  }

  if (nargs == 1) {
    return minmax_prototype_impl<Tmpl,false>(arg, is_summary) ;
  } else if (nargs == 2) {
    SEXP arg2 = CDDR(call) ;
    // we know how to handle fun( ., na.rm = TRUE/FALSE )
    if (TAG(arg2) == R_NaRmSymbol) {
      SEXP narm = CAR(arg2) ;
      if (TYPEOF(narm) == LGLSXP && LENGTH(narm) == 1) {
        if (LOGICAL(narm)[0] == TRUE) {
          return minmax_prototype_impl<Tmpl,true>(arg, is_summary) ;
        } else {
          return minmax_prototype_impl<Tmpl,false>(arg, is_summary) ;
        }
      }
    }
  }
  return 0 ;
}

Result* count_prototype(SEXP args, const LazySubsets&, int) {
  if (Rf_length(args) != 1)
    stop("n does not take arguments") ;
  return new Count ;
}

Result* count_distinct_prototype(SEXP call, const LazySubsets& subsets, int nargs) {
  MultipleVectorVisitors visitors ;
  bool na_rm = false ;

  for (SEXP p = CDR(call) ; !Rf_isNull(p) ; p = CDR(p)) {
    SEXP x = CAR(p) ;
    if (!Rf_isNull(TAG(p)) && TAG(p) == Rf_install("na.rm")) {
      if (TYPEOF(x) == LGLSXP && Rf_length(x) == 1) {
        na_rm = LOGICAL(x)[0] ;
      } else {
        stop("incompatible value for `na.rm` parameter") ;
      }
    } else if (TYPEOF(x) == SYMSXP) {
      visitors.push_back(subsets.get_variable(x))  ;
    } else {
      return 0 ;
    }
  }

  if (visitors.size() == 0) {
    stop("need at least one column for n_distinct()");
  }

  if (na_rm) {
    return new Count_Distinct_Narm<MultipleVectorVisitors>(visitors) ;
  } else {
    return new Count_Distinct<MultipleVectorVisitors>(visitors) ;
  }
}

Result* row_number_prototype(SEXP call, const LazySubsets& subsets, int nargs) {
  if (nargs >  1 || subsets.size() == 0) return 0;

  if (nargs == 0) return new RowNumber_0() ;

  RObject data(CADR(call));
  if (TYPEOF(data) == LANGSXP && CAR(data) == Rf_install("desc")) {
    data = CADR(data) ;

    if (TYPEOF(data) == SYMSXP) {
      if (subsets.count(data)) data = subsets.get_variable(data) ;
      else return 0 ;
    }
    if (Rf_length(data) == subsets.nrows()) {
      switch (TYPEOF(data)) {
      case INTSXP:
        return new RowNumber<INTSXP,  false>(data) ;
      case REALSXP:
        return new RowNumber<REALSXP, false>(data) ;
      case STRSXP:
        return new RowNumber<STRSXP,  false>(data) ;
      default:
        break;
      }
    }
    return 0 ;
  }
  if (TYPEOF(data) == SYMSXP) {
    if (subsets.count(data)) data = subsets.get_variable(data) ;
    else return 0 ;
  }
  if (Rf_length(data) == subsets.nrows()) {
    switch (TYPEOF(data)) {
    case INTSXP:
      return new RowNumber<INTSXP,true>(data) ;
    case REALSXP:
      return new RowNumber<REALSXP,true>(data) ;
    case STRSXP:
      return new RowNumber<STRSXP,true>(data) ;
    default:
      break;
    }
  }
  // we don't know how to handle it.
  return 0 ;
}

Result* ntile_prototype(SEXP call, const LazySubsets& subsets, int nargs) {
  if (nargs != 2) return 0;

  // handle 2nd arg
  SEXP ntiles = CADDR(call) ;
  double number_tiles ;
  try {
    number_tiles = as<int>(ntiles) ;
  } catch (...) {
    stop("could not convert n to scalar integer") ;
  }

  RObject data(CADR(call));
  if (TYPEOF(data) == LANGSXP && CAR(data) == Rf_install("desc")) {
    data = CADR(data) ;

    if (TYPEOF(data) == SYMSXP) {
      if (subsets.count(data)) data = subsets.get_variable(data) ;
      else return 0 ;
    }
    switch (TYPEOF(data)) {
    case INTSXP:
      return new Ntile<INTSXP,  false>(data, number_tiles) ;
    case REALSXP:
      return new Ntile<REALSXP, false>(data, number_tiles) ;
    case STRSXP:
      return new Ntile<STRSXP,  false>(data, number_tiles) ;
    default:
      break;
    }
  }
  if (TYPEOF(data) == SYMSXP) {
    if (subsets.count(data)) data = subsets.get_variable(data) ;
    else return 0 ;
  }
  if (subsets.nrows() != Rf_length(data)) return 0 ;

  switch (TYPEOF(data)) {
  case INTSXP:
    return new Ntile<INTSXP ,true>(data, number_tiles) ;
  case REALSXP:
    return new Ntile<REALSXP,true>(data, number_tiles) ;
  case STRSXP:
    return new Ntile<STRSXP ,true>(data, number_tiles) ;
  default:
    break;
  }
  // we don't know how to handle it.
  return 0 ;
}

template <typename Increment>
Result* rank_impl_prototype(SEXP call, const LazySubsets& subsets, int nargs) {
  if (nargs != 1) return 0;
  RObject data(CADR(call));

  if (TYPEOF(data) == LANGSXP && CAR(data) == Rf_install("desc")) {
    data = CADR(data) ;
    if (TYPEOF(data) == SYMSXP) {
      if (subsets.count(data)) data = subsets.get_variable(data) ;
      else return 0 ;
    }

    switch (TYPEOF(data)) {
    case INTSXP:
      return new Rank_Impl<INTSXP,  Increment, false>(data) ;
    case REALSXP:
      return new Rank_Impl<REALSXP, Increment, false>(data) ;
    case STRSXP:
      return new Rank_Impl<STRSXP,  Increment, false>(data) ;
    default:
      break;
    }
  }

  if (TYPEOF(data) == SYMSXP) {
    if (subsets.count(data)) data = subsets.get_variable(data) ;
    else return 0 ;
  }
  switch (TYPEOF(data)) {
  case INTSXP:
    return new Rank_Impl<INTSXP,  Increment, true>(data) ;
  case REALSXP:
    return new Rank_Impl<REALSXP, Increment, true>(data) ;
  case STRSXP:
    return new Rank_Impl<STRSXP,  Increment, true>(data) ;
  default:
    break;
  }
  // we don't know how to handle it.
  return 0 ;
}

struct LeadLag {

  LeadLag(SEXP call) : data(R_NilValue), n(1), def(R_NilValue), ok(true) {

    SEXP p = CDR(call) ;
    SEXP tag = TAG(p) ;
    if (tag != R_NilValue && tag != Rf_install("x")) {
      ok = false ;
      return ;
    }
    data = CAR(p) ;

    p = CDR(p);
    while (p != R_NilValue) {
      tag = TAG(p) ;
      if (tag != R_NilValue && tag != Rf_install("n") && tag != Rf_install("default")) {
        ok = false ;
        return ;
      }
      if (tag == Rf_install("n") || tag == R_NilValue) {
        try {
          n = as<int>(CAR(p));
        } catch (...) {
          SEXP n_ = CADDR(call);
          std::stringstream s ;
          stop("could not convert second argument to an integer. type=%s, length = %d",
               type2name(n_), Rf_length(n_)) ;
        }
      }
      if (tag == Rf_install("default")) {
        def = CAR(p) ;
        if (TYPEOF(def) == LANGSXP) ok = false ;
      }
      p = CDR(p) ;
    }
  }

  RObject data ;
  int n ;
  RObject def ;

  bool ok ;

} ;

template < template<int> class Templ>
Result* leadlag_prototype(SEXP call, const LazySubsets& subsets, int nargs) {
  LeadLag args(call) ;
  if (!args.ok) return 0 ;
  RObject& data = args.data ;

  if (TYPEOF(data) == SYMSXP && subsets.count(data)) {
    bool is_summary = subsets.is_summary(data) ;
    int n = args.n ;
    data = subsets.get_variable(data) ;

    switch (TYPEOF(data)) {
    case INTSXP:
      return new Templ<INTSXP> (data, n, args.def, is_summary) ;
    case REALSXP:
      return new Templ<REALSXP>(data, n, args.def, is_summary) ;
    case STRSXP:
      return new Templ<STRSXP> (data, n, args.def, is_summary) ;
    case LGLSXP:
      return new Templ<LGLSXP> (data, n, args.def, is_summary) ;
    default:
      break ;
    }

  }
  return 0 ;
}

template < template <int> class Templ>
Result* cumfun_prototype(SEXP call, const LazySubsets& subsets, int nargs) {
  if (nargs != 1) return 0 ;
  RObject data(CADR(call));
  if (TYPEOF(data) == SYMSXP) {
    data = subsets.get_variable(data) ;
  }
  switch (TYPEOF(data)) {
  case INTSXP:
    return new Templ<INTSXP>(data) ;
  case REALSXP:
    return new Templ<REALSXP>(data) ;
  default:
    break ;
  }
  return 0 ;
}

bool argmatch(const std::string& target, const std::string& s) {
  if (s.size() > target.size()) return false ;
  return target.compare(0, s.size(), s) == 0 ;
}

Result* in_prototype(SEXP call, const LazySubsets& subsets, int nargs) {
  SEXP lhs = CADR(call) ;
  SEXP rhs = CADDR(call) ;

  // if lhs is not a symbol, let R handle it
  if (TYPEOF(lhs) != SYMSXP) return 0 ;

  // if the lhs is not in the data, let R handle it
  if (!subsets.count(lhs)) return 0 ;

  SEXP v = subsets.get_variable(lhs) ;

  // if the type of the data is not the same as the type of rhs,
  // including if it needs evaluation, let R handle it
  if (TYPEOF(v) != TYPEOF(rhs)) return 0 ;

  // otherwise use hybrid version
  switch (TYPEOF(v)) {
  case STRSXP:
    return new In<STRSXP>(v, rhs) ;
  default:
    break ;
  }

  // type not handled
  return 0 ;

}

HybridHandlerMap& get_handlers() {
  static HybridHandlerMap handlers ;
  if (!handlers.size()) {
    handlers[ Rf_install("n")         ] = count_prototype ;
    handlers[ Rf_install("n_distinct")   ] = count_distinct_prototype ;
    handlers[ Rf_install("row_number")   ] = row_number_prototype ;
    handlers[ Rf_install("ntile")      ] = ntile_prototype ;

    handlers[ Rf_install("min")      ] = minmax_prototype<dplyr::Min> ;
    handlers[ Rf_install("max")      ] = minmax_prototype<dplyr::Max> ;

    handlers[ Rf_install("mean")       ] = simple_prototype<dplyr::Mean> ;
    handlers[ Rf_install("var")      ] = simple_prototype<dplyr::Var> ;
    handlers[ Rf_install("sd")        ] = simple_prototype<dplyr::Sd> ;
    handlers[ Rf_install("sum")      ] = simple_prototype<dplyr::Sum>;

    handlers[ Rf_install("min_rank")     ] = rank_impl_prototype<dplyr::internal::min_rank_increment> ;
    handlers[ Rf_install("percent_rank")   ] = rank_impl_prototype<dplyr::internal::percent_rank_increment> ;
    handlers[ Rf_install("dense_rank")   ] = rank_impl_prototype<dplyr::internal::dense_rank_increment> ;
    handlers[ Rf_install("cume_dist")    ] = rank_impl_prototype<dplyr::internal::cume_dist_increment> ;

    /*
    handlers[ Rf_install( "cumsum")      ] = cumfun_prototype<CumSum> ;
    handlers[ Rf_install( "cummin")      ] = cumfun_prototype<CumMin> ;
    handlers[ Rf_install( "cummax")      ] = cumfun_prototype<CumMax> ;
    */

    handlers[ Rf_install("lead")       ] = leadlag_prototype<Lead> ;
    handlers[ Rf_install("lag")      ] = leadlag_prototype<Lag> ;

    handlers[ Rf_install("first")      ] = first_prototype ;
    handlers[ Rf_install("last")       ] = last_prototype ;
    handlers[ Rf_install("nth")      ] = nth_prototype ;

    // handlers[ Rf_install( "%in%" ) ] = in_prototype ;

  }
  return handlers ;
}

Result* constant_handler(SEXP constant) {
  switch (TYPEOF(constant)) {
  case INTSXP:
  {
    if (Rf_inherits(constant, "Date")) return new TypedConstantResult<INTSXP>(constant, get_date_classes()) ;
    return new ConstantResult<INTSXP>(constant) ;
  }
  case REALSXP:
  {
    if (Rf_inherits(constant, "difftime")) return new DifftimeConstantResult<REALSXP>(constant) ;
    if (Rf_inherits(constant, "POSIXct")) return new TypedConstantResult<REALSXP>(constant, get_time_classes()) ;
    if (Rf_inherits(constant, "Date")) return new TypedConstantResult<REALSXP>(constant, get_date_classes()) ;
    return new ConstantResult<REALSXP>(constant) ;
  }
  case STRSXP:
    return new ConstantResult<STRSXP>(constant) ;
  case LGLSXP:
    return new ConstantResult<LGLSXP>(constant) ;
  }
  return 0;
}

Result* get_handler(SEXP call, const LazySubsets& subsets, const Environment& env) {
  if (TYPEOF(call) == LANGSXP) {
    int depth = Rf_length(call) ;
    HybridHandlerMap& handlers = get_handlers() ;
    SEXP fun_symbol = CAR(call) ;
    if (TYPEOF(fun_symbol) != SYMSXP) return 0 ;

    HybridHandlerMap::const_iterator it = handlers.find(fun_symbol) ;
    if (it == handlers.end()) return 0 ;

    return it->second(call, subsets, depth - 1);
  } else if (TYPEOF(call) == SYMSXP) {
    if (!subsets.count(call)) {
      SEXP data = env.find(CHAR(PRINTNAME(call))) ;
      if (Rf_length(data) == 1) return constant_handler(data) ;
    }
  } else {
    // TODO: perhaps deal with SYMSXP separately
    if (Rf_length(call) == 1) return constant_handler(call) ;
  }
  return 0 ;
}

void registerHybridHandler(const char* name, HybridHandler proto) {
  get_handlers()[ Rf_install(name) ] = proto ;
}

bool can_simplify(SEXP call) {
  if (TYPEOF(call) == LISTSXP) {
    bool res = can_simplify(CAR(call)) ;
    if (res) return true ;
    return can_simplify(CDR(call)) ;
  }

  if (TYPEOF(call) == LANGSXP) {
    SEXP fun_symbol = CAR(call) ;
    if (TYPEOF(fun_symbol) != SYMSXP) return false ;

    if (get_handlers().count(fun_symbol)) return true ;

    return can_simplify(CDR(call)) ;
  }
  return false ;
}

template <typename Index>
DataFrame subset(DataFrame df, const Index& indices, CharacterVector columns, CharacterVector classes) {
  return DataFrameSubsetVisitors(df, columns).subset(indices, classes) ;
}

template <typename Index>
DataFrame subset(DataFrame df, const Index& indices, CharacterVector classes) {
  return DataFrameSubsetVisitors(df).subset(indices, classes) ;
}

template <typename Index>
DataFrame subset_join(DataFrame x, DataFrame y,
                      const Index& indices_x, const Index& indices_y,
                      CharacterVector by_x, CharacterVector by_y ,
                      const std::string& suffix_x, const std::string& suffix_y,
                      CharacterVector classes) {
  // first the joined columns
  DataFrameJoinVisitors join_visitors(x, y, by_x, by_y, false) ;
  int n_join_visitors = join_visitors.size() ;

  // then columns from x but not y
  CharacterVector all_x_columns = x.names() ;
  std::vector<bool> joiner(all_x_columns.size()) ;
  CharacterVector x_columns(all_x_columns.size() - n_join_visitors) ;
  IntegerVector xm = r_match(all_x_columns, by_x) ;
  for (int i=0, k=0; i<all_x_columns.size(); i++) {
    if (xm[i] == NA_INTEGER) {
      joiner[i] = false ;
      x_columns[k++] = all_x_columns[i] ;
    } else {
      joiner[i] = true ;
    }
  }
  DataFrameSubsetVisitors visitors_x(x, x_columns) ;
  int nv_x = visitors_x.size() ;

  // then columns from y but not x
  CharacterVector all_y_columns = y.names() ;
  CharacterVector y_columns(all_y_columns.size() - n_join_visitors) ;
  IntegerVector ym = r_match(all_y_columns, by_y) ;
  for (int i=0, k=0; i<all_y_columns.size(); i++) {
    if (ym[i] == NA_INTEGER) {
      y_columns[k++] = all_y_columns[i] ;
    }
  }
  DataFrameSubsetVisitors visitors_y(y, y_columns) ;

  int nv_y = visitors_y.size() ;

  // construct out object
  int nrows = indices_x.size() ;
  List out(n_join_visitors+nv_x+nv_y);
  CharacterVector names(n_join_visitors+nv_x+nv_y) ;

  int index_join_visitor = 0 ;
  int index_x_visitor = 0 ;
  // ---- join visitors
  for (int i=0; i<all_x_columns.size(); i++) {
    String col_name = all_x_columns[i] ;
    if (joiner[i]) {
      JoinVisitor* v = join_visitors.get(xm[i]-1) ;
      out[i] = v->subset(indices_x) ;
      index_join_visitor++ ;
    } else {

      while (
        (std::find(y_columns.begin(), y_columns.end(), col_name.get_sexp()) != y_columns.end()) ||
        (std::find(names.begin(), names.begin() + i, col_name.get_sexp()) != names.begin() + i)
      ) {
        col_name += suffix_x ;
      }
      out[i] = visitors_x.get(index_x_visitor)->subset(indices_x) ;
      index_x_visitor++ ;
    }
    names[i] = col_name ;
  }

  int k = index_join_visitor +  index_x_visitor ;
  for (int i=0; i<nv_y; i++, k++) {
    String col_name = y_columns[i] ;

    // we suffix by .y if this column is in x_columns

    while (
      (std::find(all_x_columns.begin(), all_x_columns.end(), col_name.get_sexp()) != all_x_columns.end()) ||
      (std::find(names.begin(), names.begin() + k, col_name.get_sexp()) != names.begin() + k)
    ) {
      col_name += suffix_y ;
    }

    out[k] = visitors_y.get(i)->subset(indices_y) ;
    names[k] = col_name ;
  }
  out.attr("class") = classes ;
  set_rownames(out, nrows) ;
  out.names() = names ;

  SEXP vars = x.attr("vars") ;
  if (!Rf_isNull(vars))
    out.attr("vars") = vars ;

  return (SEXP)out ;
}

template <typename TargetContainer, typename SourceContainer>
void push_back(TargetContainer& x, const SourceContainer& y) {
  x.insert(x.end(), y.begin(), y.end()) ;
}
template <typename TargetContainer, typename SourceContainer>
void push_back_right(TargetContainer& x, const SourceContainer& y) {
  // x.insert( x.end(), y.begin(), y.end() ) ;
  int n = y.size() ;
  for (int i=0; i<n; i++) {
    x.push_back(-y[i]-1) ;
  }
}

template <typename Container>
void push_back(Container& x, typename Container::value_type value, int n) {
  for (int i=0; i<n; i++)
    x.push_back(value) ;
}

// [[Rcpp::export]]
void assert_all_white_list(const DataFrame& data) {
  // checking variables are on the white list
  int nc = data.size() ;
  for (int i=0; i<nc; i++) {
    if (!white_list(data[i])) {
      CharacterVector names = data.names() ;
      String name_i = names[i] ;
      SEXP v = data[i] ;

      SEXP klass = Rf_getAttrib(v, R_ClassSymbol) ;
      if (!Rf_isNull(klass)) {
        stop("column '%s' has unsupported class : %s",
             name_i.get_cstring() , get_single_class(v));
      }
      else {
        stop("column '%s' has unsupported type : %s",
             name_i.get_cstring() , Rf_type2char(TYPEOF(v)));
      }

    }
  }
}

// [[Rcpp::export]]
DataFrame semi_join_impl(DataFrame x, DataFrame y, CharacterVector by_x, CharacterVector by_y) {
  if (by_x.size() == 0) stop("no variable to join by") ;
  typedef VisitorSetIndexMap<DataFrameJoinVisitors, std::vector<int> > Map ;
  DataFrameJoinVisitors visitors(x, y, by_x, by_y, false) ;
  Map map(visitors);

  // train the map in terms of x
  train_push_back(map, x.nrows()) ;

  int n_y = y.nrows() ;
  // this will collect indices from rows in x that match rows in y
  std::vector<int> indices ;
  for (int i=0; i<n_y; i++) {
    // find a row in x that matches row i from y
    Map::iterator it = map.find(-i-1) ;

    if (it != map.end()) {
      // collect the indices and remove them from the
      // map so that they are only found once.
      push_back(indices, it->second) ;

      map.erase(it) ;

    }
  }

  return subset(x, indices, x.names(), x.attr("class")) ;
}

// [[Rcpp::export]]
DataFrame anti_join_impl(DataFrame x, DataFrame y, CharacterVector by_x, CharacterVector by_y) {
  if (by_x.size() == 0) stop("no variable to join by") ;
  typedef VisitorSetIndexMap<DataFrameJoinVisitors, std::vector<int> > Map ;
  DataFrameJoinVisitors visitors(x, y, by_x, by_y, false) ;
  Map map(visitors);

  // train the map in terms of x
  train_push_back(map, x.nrows()) ;

  int n_y = y.nrows() ;
  // remove the rows in x that match
  for (int i=0; i<n_y; i++) {
    Map::iterator it = map.find(-i-1) ;
    if (it != map.end())
      map.erase(it) ;
  }

  // collect what's left
  std::vector<int> indices ;
  for (Map::iterator it = map.begin() ; it != map.end(); ++it)
    push_back(indices, it->second) ;

  return subset(x, indices, x.names(), x.attr("class")) ;
}

// [[Rcpp::export]]
DataFrame inner_join_impl(DataFrame x, DataFrame y,
                          CharacterVector by_x, CharacterVector by_y,
                          std::string& suffix_x, std::string& suffix_y) {
  if (by_x.size() == 0) stop("no variable to join by") ;
  typedef VisitorSetIndexMap<DataFrameJoinVisitors, std::vector<int> > Map ;
  DataFrameJoinVisitors visitors(x, y, by_x, by_y, true) ;
  Map map(visitors);

  int n_x = x.nrows(), n_y = y.nrows() ;

  std::vector<int> indices_x ;
  std::vector<int> indices_y ;

  train_push_back_right(map, n_y) ;

  for (int i=0; i<n_x; i++) {
    Map::iterator it = map.find(i) ;
    if (it != map.end()) {
      push_back_right(indices_y, it->second);
      push_back(indices_x, i, it->second.size()) ;
    }
  }

  return subset_join(
           x, y,
           indices_x, indices_y,
           by_x, by_y,
           suffix_x, suffix_y,
           x.attr("class")
         );
}

// [[Rcpp::export]]
DataFrame left_join_impl(DataFrame x, DataFrame y,
                         CharacterVector by_x, CharacterVector by_y,
                         std::string& suffix_x, std::string& suffix_y) {
  if (by_x.size() == 0) stop("no variable to join by") ;
  typedef VisitorSetIndexMap<DataFrameJoinVisitors, std::vector<int> > Map ;
  DataFrameJoinVisitors visitors(y, x, by_y, by_x, true) ;

  Map map(visitors);

  // train the map in terms of y
  train_push_back(map, y.nrows()) ;

  std::vector<int> indices_x ;
  std::vector<int> indices_y ;

  int n_x = x.nrows() ;
  for (int i=0; i<n_x; i++) {
    // find a row in y that matches row i in x
    Map::iterator it = map.find(-i-1) ;
    if (it != map.end()) {
      push_back(indices_y,  it->second) ;
      push_back(indices_x, i, it->second.size()) ;
    } else {
      indices_y.push_back(-1) ; // mark NA
      indices_x.push_back(i) ;
    }
  }

  return subset_join(
           x, y,
           indices_x, indices_y,
           by_x, by_y,
           suffix_x, suffix_y,
           x.attr("class")
         );
}

// [[Rcpp::export]]
DataFrame right_join_impl(DataFrame x, DataFrame y,
                          CharacterVector by_x, CharacterVector by_y,
                          std::string& suffix_x, std::string& suffix_y) {
  if (by_x.size() == 0) stop("no variable to join by") ;
  typedef VisitorSetIndexMap<DataFrameJoinVisitors, std::vector<int> > Map ;
  DataFrameJoinVisitors visitors(x, y, by_x, by_y, true) ;
  Map map(visitors);

  // train the map in terms of x
  train_push_back(map, x.nrows()) ;

  std::vector<int> indices_x ;
  std::vector<int> indices_y ;

  int n_y = y.nrows() ;
  for (int i=0; i<n_y; i++) {
    // find a row in y that matches row i in x
    Map::iterator it = map.find(-i-1) ;
    if (it != map.end()) {
      push_back(indices_x,  it->second) ;
      push_back(indices_y, i, it->second.size()) ;
    } else {
      indices_x.push_back(-i-1) ; // point to the i-th row in the right table
      indices_y.push_back(i) ;
    }
  }
  return subset_join(
           x, y,
           indices_x, indices_y,
           by_x, by_y,
           suffix_x, suffix_y,
           x.attr("class")
         );
}

// [[Rcpp::export]]
DataFrame full_join_impl(DataFrame x, DataFrame y,
                         CharacterVector by_x, CharacterVector by_y,
                         std::string& suffix_x, std::string& suffix_y) {
  if (by_x.size() == 0) stop("no variable to join by") ;
  typedef VisitorSetIndexMap<DataFrameJoinVisitors, std::vector<int> > Map ;
  DataFrameJoinVisitors visitors(y, x, by_y, by_x, true) ;
  Map map(visitors);

  // train the map in terms of y
  train_push_back(map, y.nrows()) ;

  std::vector<int> indices_x ;
  std::vector<int> indices_y ;

  int n_x = x.nrows(), n_y = y.nrows() ;

  // get both the matches and the rows from left but not right
  for (int i=0; i<n_x; i++) {
    // find a row in y that matches row i in x
    Map::iterator it = map.find(-i-1) ;
    if (it != map.end()) {
      push_back(indices_y,  it->second) ;
      push_back(indices_x, i, it->second.size()) ;
    } else {
      indices_y.push_back(-1) ; // mark NA
      indices_x.push_back(i) ;
    }
  }

  // train a new map in terms of x this time
  DataFrameJoinVisitors visitors2(x,y,by_x,by_y, false) ;
  Map map2(visitors2);
  train_push_back(map2, x.nrows()) ;

  for (int i=0; i<n_y; i++) {
    // try to find row in x that matches this row of y
    Map::iterator it = map2.find(-i-1) ;
    if (it == map2.end()) {
      indices_x.push_back(-i-1) ;
      indices_y.push_back(i) ;
    }
  }

  return subset_join(x, y,
                     indices_x, indices_y,
                     by_x, by_y,
                     suffix_x, suffix_y,
                     x.attr("class")
                    );
}

// [[Rcpp::export]]
SEXP shallow_copy(const List& data) {
  int n = data.size() ;
  List out(n) ;
  for (int i=0; i<n; i++) {
    out[i] = shared_SEXP(data[i]) ;
  }
  copy_attributes(out, data) ;
  return out ;
}

// [[Rcpp::export]]
dplyr::BoolResult compatible_data_frame_nonames(DataFrame x, DataFrame y, bool convert) {
  int n = x.size() ;
  if (n != y.size())
    return no_because(tfm::format("different number of columns : %d x %d", n, y.size())) ;

  if (convert) {
    for (int i=0; i<n; i++) {
      try {
        boost::scoped_ptr<JoinVisitor> v(join_visitor(x[i], y[i], "x", "x", true)) ;
      } catch (...) {
        return no_because("incompatible") ;
      }
    }
  } else {
    for (int i=0; i<n; i++) {
      SEXP xi = x[i], yi=y[i] ;
      if (TYPEOF(xi) != TYPEOF(yi))
        return no_because("incompatible types") ;

      if (TYPEOF(xi) == INTSXP) {
        if (Rf_inherits(xi, "factor") && Rf_inherits(yi, "factor")) {
          if (same_levels(xi, yi)) continue ;
          return no_because("factors with different levels") ;
        }

        if (Rf_inherits(xi, "factor")) return no_because("cannot compare factor and integer") ;
        if (Rf_inherits(yi, "factor")) return no_because("cannot compare factor and integer") ;

      }
    }
  }

  return yes() ;

}

// [[Rcpp::export]]
dplyr::BoolResult compatible_data_frame(DataFrame x, DataFrame y, bool ignore_col_order = true, bool convert = false) {
  int n = x.size() ;

  bool null_x = Rf_isNull(x.names()), null_y = Rf_isNull(y.names()) ;
  if (null_x && !null_y) {
    return no_because("x does not have names, but y does") ;
  } else if (null_y && !null_x) {
    return no_because("y does not have names, but x does") ;
  } else if (null_x && null_y) {
    return compatible_data_frame_nonames(x,y, convert) ;
  }

  CharacterVector names_x = x.names() ;
  CharacterVector names_y = y.names() ;

  CharacterVector names_y_not_in_x = setdiff(names_y, names_x);
  CharacterVector names_x_not_in_y = setdiff(names_x, names_y);

  if (!ignore_col_order) {
    if (names_y_not_in_x.size() == 0 && names_y_not_in_x.size() == 0) {
      // so the names are the same, check if they are in the same order
      for (int i=0; i<n; i++) {
        if (names_x[i] != names_y[i]) {
          return no_because("Same column names, but different order") ;
        }
      }
    }
  }

  std::stringstream ss ;
  bool ok = true ;
  if (names_y_not_in_x.size()) {
    ok = false ;
    ss << "Cols in y but not x: " << collapse(names_y_not_in_x) << ". ";
  }

  if (names_x_not_in_y.size()) {
    ok = false ;
    ss << "Cols in x but not y: " << collapse(names_x_not_in_y) << ". ";
  }

  if (!ok) {
    return no_because(ss.str()) ;
  }

  IntegerVector orders = r_match(names_x, names_y) ;

  String name ;
  for (int i=0; i<n; i++) {
    name = names_x[i] ;
    SEXP xi = x[i], yi = y[orders[i]-1] ;
    boost::scoped_ptr<SubsetVectorVisitor> vx(subset_visitor(xi)) ;
    boost::scoped_ptr<SubsetVectorVisitor> vy(subset_visitor(yi)) ;
    SubsetVectorVisitor* px = vx.get() ;
    SubsetVectorVisitor* py = vy.get() ;

    if (typeid(*px) != typeid(*py)) {
      ss << "Incompatible type for column "
         << name.get_cstring()
         << ": x " << vx->get_r_type()
         << ", y " << vy->get_r_type() ;

      if (!convert) {
        ok = false ;
        continue ;
      }
    }

    if (! vx->is_compatible(py, ss, name)) {
      ok = false ;
    }
  }

  if (!ok) return no_because(ss.str()) ;
  return yes() ;
}

class RowTrack {
public:
  RowTrack(const std::string& msg, int max_count_ = 10) : ss(), count(0), max_count(max_count_) {
    ss << msg ;
  }

  void record(int i) {
    if (count > max_count) return ;
    if (count) ss << ", " ;
    int idx = i >= 0 ? (i+1) : -i ;
    ss << idx ;
    if (count == max_count) ss << "[...]" ;
    count++ ;
  }

  bool empty() const {
    return count == 0 ;
  }

  std::string str() const {
    return ss.str() ;
  }

private:
  std::stringstream ss ;
  int count ;
  int max_count ;
} ;

// [[Rcpp::export]]
dplyr::BoolResult equal_data_frame(DataFrame x, DataFrame y, bool ignore_col_order = true, bool ignore_row_order = true, bool convert = false) {
  BoolResult compat = compatible_data_frame(x, y, ignore_col_order, convert);
  if (!compat) return compat ;

  typedef VisitorSetIndexMap<DataFrameJoinVisitors, std::vector<int> > Map ;
  DataFrameJoinVisitors visitors(x, y, x.names(), x.names(), true) ;
  Map map(visitors);

  // train the map in both x and y
  int nrows_x = x.nrows() ;
  int nrows_y = y.nrows() ;

  if (nrows_x != nrows_y)
    return no_because("Different number of rows") ;
  if (x.size() == 0)
    return yes() ;

  for (int i=0; i<nrows_x; i++) map[i].push_back(i) ;
  for (int i=0; i<nrows_y; i++) map[-i-1].push_back(-i-1) ;

  RowTrack track_x("Rows in x but not y: ") ;
  RowTrack track_y("Rows in y but not x: ") ;
  RowTrack track_mismatch("Rows with difference occurences in x and y: ") ;

  bool ok = true ;
  Map::const_iterator it = map.begin() ;

  for (; it != map.end(); ++it) {
    // retrieve the indices ( -ves for y, +ves for x )
    const std::vector<int>& chunk = it->second ;
    int n = chunk.size() ;

    int count_left = 0, count_right = 0 ;
    for (int i=0; i<n; i++) {
      if (chunk[i] < 0)
        count_right++ ;
      else
        count_left++ ;
    }
    if (count_right == 0) {
      track_x.record(chunk[0]) ;
      ok = false ;
    } else if (count_left == 0) {
      track_y.record(chunk[0]) ;
      ok = false ;
    } else if (count_left != count_right) {
      track_mismatch.record(chunk[0]) ;
      ok = false ;
    }

  }

  if (!ok) {
    std::stringstream ss ;
    if (! track_x.empty()) ss << track_x.str() << ". " ;
    if (! track_y.empty()) ss << track_y.str() << ". " ;
    if (! track_mismatch.empty()) ss << track_mismatch.str() ;

    return no_because(ss.str()) ;
  }

  if (ok && ignore_row_order) return yes();

  if (!ignore_row_order) {
    for (int i=0; i<nrows_x; i++) {
      if (!visitors.equal(i, -i-1)) {
        return no_because("Same row values, but different order") ;
      }
    }
  }

  return yes() ;
}

// [[Rcpp::export]]
dplyr::BoolResult all_equal_data_frame(List args, Environment env) {
  int n = args.size() ;
  DataFrame x0 = Rf_eval(args[0], env) ;
  for (int i=1; i<n; i++) {
    BoolResult test = equal_data_frame(x0, Rf_eval(args[i], env)) ;
    if (!test) return test ;
  }
  return yes() ;
}

// [[Rcpp::export]]
DataFrame union_data_frame(DataFrame x, DataFrame y) {
  BoolResult compat = compatible_data_frame(x,y,true,true) ;
  if (!compat) {
    stop("not compatible: %s", compat.why_not());
  }

  typedef VisitorSetIndexSet<DataFrameJoinVisitors> Set ;
  DataFrameJoinVisitors visitors(x, y, x.names(), x.names(), true) ;
  Set set(visitors);

  train_insert(set, x.nrows()) ;
  train_insert_right(set, y.nrows()) ;

  return visitors.subset(set, x.attr("class")) ;
}

// [[Rcpp::export]]
DataFrame intersect_data_frame(DataFrame x, DataFrame y) {
  BoolResult compat = compatible_data_frame(x,y,true,true) ;
  if (!compat) {
    stop("not compatible: %s", compat.why_not());
  }
  typedef VisitorSetIndexSet<DataFrameJoinVisitors> Set ;

  DataFrameJoinVisitors visitors(x, y, x.names(), x.names(), true) ;
  Set set(visitors);

  train_insert(set, x.nrows()) ;

  std::vector<int> indices ;
  int n_y = y.nrows() ;
  for (int i=0; i<n_y; i++) {
    Set::iterator it = set.find(-i-1) ;
    if (it != set.end()) {
      indices.push_back(*it) ;
      set.erase(it) ;
    }
  }

  return visitors.subset(indices, x.attr("class")) ;
}

// [[Rcpp::export]]
DataFrame setdiff_data_frame(DataFrame x, DataFrame y) {
  BoolResult compat = compatible_data_frame(x,y,true,true) ;
  if (!compat) {
    stop("not compatible: %s", compat.why_not());
  }

  typedef VisitorSetIndexSet<DataFrameJoinVisitors> Set ;
  DataFrameJoinVisitors visitors(y, x, y.names(), y.names(), true) ;
  Set set(visitors);

  train_insert(set, y.nrows()) ;

  std::vector<int> indices ;

  int n_x = x.nrows() ;
  for (int i=0; i<n_x; i++) {
    if (!set.count(-i-1)) {
      set.insert(-i-1) ;
      indices.push_back(-i-1) ;
    }
  }

  return visitors.subset(indices, x.attr("class")) ;
}

// [[Rcpp::export]]
IntegerVector match_data_frame(DataFrame x, DataFrame y) {
  if (!compatible_data_frame(x,y,true,true))
    stop("not compatible");

  typedef VisitorSetIndexSet<DataFrameJoinVisitors> Set ;
  DataFrameJoinVisitors visitors(y, x, x.names(), x.names(), true) ;
  Set set(visitors);

  train_insert(set, y.nrows()) ;

  int n_x = x.nrows() ;
  IntegerVector res = no_init(n_x);
  for (int i=0; i<n_x; i++) {
    Set::iterator it = set.find(-i-1);
    res[i] = (it == set.end()) ? NA_INTEGER : (*it+1) ;
  }

  return res ;
}

// [[Rcpp::export]]
SEXP resolve_vars(List new_groups, CharacterVector names) {
  int n = new_groups.size() ;
  for (int i=0; i<n; i++) {
    List lazy = new_groups[i] ;
    Environment env = lazy[1] ;
    SEXP s = lazy[0] ;

    // expand column
    if (TYPEOF(s) == SYMSXP) {

    } else if (TYPEOF(s) == LANGSXP && CAR(s) == Rf_install("column") && Rf_length(s) == 2) {
      s = extract_column(CADR(s), env) ;
    } else {
      continue ;
    }
    // check that s is indeed in the data

    int pos = as<int>(r_match(CharacterVector::create(PRINTNAME(s)), names));
    if (pos == NA_INTEGER) {
      stop("unknown variable to group by : %s", CHAR(PRINTNAME(s))) ;
    }
    lazy[0] = s ;
  }

  return new_groups ;
}

// [[Rcpp::export]]
DataFrame grouped_df_impl(DataFrame data, ListOf<Symbol> symbols, bool drop) {
  assert_all_white_list(data);
  DataFrame copy(shallow_copy(data));
  copy.attr("vars") = symbols ;
  copy.attr("drop") = drop ;
  if (!symbols.size())
    stop("no variables to group by") ;
  return build_index_cpp(copy) ;
}

DataFrame build_index_cpp(DataFrame data) {
  ListOf<Symbol> symbols(data.attr("vars")) ;

  int nsymbols = symbols.size() ;
  CharacterVector vars(nsymbols) ;
  CharacterVector names = data.names() ;
  for (int i=0; i<nsymbols; i++) {
    vars[i] = PRINTNAME(symbols[i]) ;
  }
  IntegerVector indx = r_match(vars, names) ;

  for (int i=0; i<nsymbols; i++) {
    int pos = indx[i] ;
    if (pos == NA_INTEGER) {
      stop("unknown column '%s' ", CHAR(names[i])) ;
    }

    SEXP v = data[pos-1] ;

    if (!white_list(v) || TYPEOF(v) == VECSXP) {
      const char* name = vars[i] ;
      stop("cannot group column %s, of class '%s'", name, get_single_class(v)) ;
    }
  }

  DataFrameVisitors visitors(data, vars) ;
  ChunkIndexMap map(visitors) ;

  train_push_back(map, data.nrows()) ;

  DataFrame labels = DataFrameSubsetVisitors(data, vars).subset(map, "data.frame") ;
  int ngroups = labels.nrows() ;
  IntegerVector labels_order = OrderVisitors(labels).apply() ;

  labels = DataFrameSubsetVisitors(labels).subset(labels_order, "data.frame") ;

  List indices(ngroups) ;
  IntegerVector group_sizes = no_init(ngroups);
  int biggest_group = 0 ;

  ChunkIndexMap::const_iterator it = map.begin() ;
  std::vector<const std::vector<int>* > chunks(ngroups) ;
  for (int i=0; i<ngroups; i++, ++it) {
    chunks[i] = &it->second ;
  }

  for (int i=0; i<ngroups; i++) {
    int idx = labels_order[i] ;
    const std::vector<int>& chunk = *chunks[idx] ;
    indices[i] = chunk ;
    group_sizes[i] = chunk.size() ;
    biggest_group = std::max(biggest_group, (int)chunk.size());
  }

  data.attr("indices") = indices ;
  data.attr("group_sizes") = group_sizes ;
  data.attr("biggest_group_size") = biggest_group ;
  data.attr("labels") = labels ;
  data.attr("class") = CharacterVector::create("grouped_df", "tbl_df", "tbl", "data.frame") ;
  return data ;
}

DataFrame build_index_adj(DataFrame df, ListOf<Symbol> symbols) {
  int nsymbols = symbols.size() ;
  CharacterVector vars(nsymbols) ;
  for (int i=0; i<nsymbols; i++) {
    vars[i] = PRINTNAME(symbols[i]) ;
  }

  DataFrameVisitors visitors(df, vars) ;
  std::vector<int> sizes ;
  int n = df.nrows() ;

  int i=0 ;
  while (i<n) {
    int start = i++ ;
    for (; i<n && visitors.equal(i, start) ; i++) ;
    sizes.push_back(i-start) ;
  }

  n = sizes.size() ;
  List indices(n);
  IntegerVector first = no_init(n) ;
  int start = 0 ;
  int biggest_group = 0 ;
  for (int i=0; i<n; i++) {
    first[i] = start ;
    int end = start + sizes[i] - 1 ;
    indices[i] = seq(start, end) ;
    start = end + 1 ;
    biggest_group = std::max(biggest_group, sizes[i]) ;
  }

  df.attr("indices") = indices ;
  df.attr("labels")  = DataFrameSubsetVisitors(df, vars).subset(first, "data.frame") ;
  df.attr("group_sizes") = sizes ;
  df.attr("biggest_group_size") = biggest_group ;
  df.attr("class") = CharacterVector::create("adj_grouped_df", "grouped_df", "tbl_df", "tbl", "data.frame") ;
  df.attr("vars") = symbols ;

  return df ;
}

// [[Rcpp::export]]
DataFrame grouped_df_adj_impl(DataFrame data, ListOf<Symbol> symbols, bool drop) {
  DataFrame copy(shallow_copy(data));
  copy.attr("vars") = symbols ;
  copy.attr("drop") = drop ;
  return build_index_adj(data, symbols) ;
}

typedef dplyr_hash_set<SEXP> SymbolSet ;

inline SEXP check_filter_integer_result(SEXP tmp) {
  if (TYPEOF(tmp) != INTSXP &&  TYPEOF(tmp) != REALSXP && TYPEOF(tmp) != LGLSXP) {
    stop("slice condition does not evaluate to an integer or numeric vector. ") ;
  }
  return tmp ;
}

class CountIndices {
public:
  CountIndices(int nr_, IntegerVector test_) : nr(nr_), test(test_), n_pos(0), n_neg(0) {

    for (int j=0; j<test.size(); j++) {
      int i = test[j] ;
      if (i > 0 && i <= nr) {
        n_pos++ ;
      } else if (i < 0 && i >= -nr) {
        n_neg++ ;
      }
    }

    if (n_neg > 0 && n_pos > 0) {
      stop("found %d positive indices and %d negative indices", n_pos, n_neg);
    }

  }

  inline bool is_positive() const {
    return n_pos > 0 ;
  }
  inline int get_n_positive() const {
    return n_pos;
  }
  inline int get_n_negative() const {
    return n_neg;
  }

private:
  int nr ;
  IntegerVector test ;
  int n_pos ;
  int n_neg ;
} ;

SEXP slice_grouped(GroupedDataFrame gdf, const LazyDots& dots) {
  typedef GroupedCallProxy<GroupedDataFrame, LazyGroupedSubsets> Proxy ;

  const DataFrame& data = gdf.data() ;
  const Lazy& lazy = dots[0] ;
  Environment env = lazy.env() ;
  CharacterVector names = data.names() ;
  SymbolSet set ;
  for (int i=0; i<names.size(); i++) {
    set.insert(Rf_installChar(names[i])) ;
  }

  // we already checked that we have only one expression
  Call call(lazy.expr()) ;

  std::vector<int> indx ;
  indx.reserve(1000) ;

  IntegerVector g_test ;
  Proxy call_proxy(call, gdf, env) ;

  int ngroups = gdf.ngroups() ;
  GroupedDataFrame::group_iterator git = gdf.group_begin() ;
  for (int i=0; i<ngroups; i++, ++git) {
    SlicingIndex indices = *git ;
    int nr = indices.size() ;
    g_test = check_filter_integer_result(call_proxy.get(indices)) ;
    CountIndices counter(indices.size(), g_test) ;

    if (counter.is_positive()) {
      // positive indexing
      int ntest = g_test.size() ;
      for (int j=0; j<ntest; j++) {
        if (!(g_test[j] > nr || g_test[j] == NA_INTEGER)) {
          indx.push_back(indices[g_test[j]-1]) ;
        }
      }
    } else if (counter.get_n_negative() != 0) {
      // negative indexing
      std::set<int> drop ;
      int n = g_test.size() ;
      for (int j=0; j<n; j++) {
        if (g_test[j] != NA_INTEGER)
          drop.insert(-g_test[j]) ;
      }
      int n_drop = drop.size() ;
      std::set<int>::const_iterator drop_it = drop.begin() ;

      int k = 0, j = 0 ;
      while (drop_it != drop.end()) {
        int next_drop = *drop_it - 1;
        while (j < next_drop) {
          indx.push_back(indices[j++]) ;
          k++ ;
        }
        j++ ;
        ++drop_it ;
      }
      while (k < nr - n_drop) {
        indx.push_back(indices[j++]) ;
        k++ ;
      }

    }
  }
  DataFrame res = subset(data, indx, names, classes_grouped<GroupedDataFrame>()) ;
  res.attr("vars")   = data.attr("vars") ;
  strip_index(res) ;

  return GroupedDataFrame(res).data() ;

}

SEXP slice_not_grouped(const DataFrame& df, const LazyDots& dots) {
  CharacterVector names = df.names() ;
  SymbolSet set ;
  for (int i=0; i<names.size(); i++) {
    set.insert(Rf_installChar(names[i])) ;
  }
  const Lazy& lazy = dots[0] ;
  Call call(lazy.expr());
  CallProxy proxy(call, df, lazy.env()) ;
  int nr = df.nrows() ;

  IntegerVector test = check_filter_integer_result(proxy.eval()) ;

  int n = test.size() ;

  // count the positive and negatives
  CountIndices counter(nr, test) ;

  // just positives -> one based subset
  if (counter.is_positive()) {
    int n_pos = counter.get_n_positive() ;
    std::vector<int> idx(n_pos) ;
    int j=0 ;
    for (int i=0; i<n_pos; i++) {
      while (test[j] > nr || test[j] == NA_INTEGER) j++ ;
      idx[i] = test[j++] - 1 ;
    }

    return subset(df, idx, df.names(), classes_not_grouped()) ;
  }

  // special case where only NA
  if (counter.get_n_negative() == 0) {
    std::vector<int> indices ;
    DataFrame res = subset(df, indices, df.names(), classes_not_grouped()) ;
    return res ;
  }

  // just negatives (out of range is dealt with early in CountIndices).
  std::set<int> drop ;
  for (int i=0; i<n; i++) {
    if (test[i] != NA_INTEGER)
      drop.insert(-test[i]) ;
  }
  int n_drop = drop.size() ;
  std::vector<int> indices(nr - n_drop) ;
  std::set<int>::const_iterator drop_it = drop.begin() ;

  int i = 0, j = 0 ;
  while (drop_it != drop.end()) {
    int next_drop = *drop_it - 1;
    while (j < next_drop) {
      indices[i++] = j++ ;
    }
    j++ ;
    ++drop_it ;
  }
  while (i < nr - n_drop) {
    indices[i++] = j++ ;
  }

  DataFrame res = subset(df, indices, df.names(), classes_not_grouped()) ;
  return res ;

}

// [[Rcpp::export]]
SEXP slice_impl(DataFrame df, LazyDots dots) {
  if (dots.size() == 0) return df ;
  if (dots.size() != 1)
    stop("slice only accepts one expression");
  if (is<GroupedDataFrame>(df)) {
    return slice_grouped(GroupedDataFrame(df), dots) ;
  } else {
    return slice_not_grouped(df, dots) ;
  }
}

template <typename Data>
SEXP structure_mutate(const NamedListAccumulator<Data>& accumulator, const DataFrame& df, CharacterVector classes) {
  List res = accumulator ;
  res.attr("class") = classes ;
  set_rownames(res, df.nrows()) ;
  res.attr("vars")   = df.attr("vars") ;
  res.attr("labels")  = df.attr("labels");
  res.attr("index")  = df.attr("index") ;
  res.attr("indices") = df.attr("indices") ;
  res.attr("drop") = df.attr("drop") ;
  res.attr("group_sizes") = df.attr("group_sizes") ;
  res.attr("biggest_group_size") = df.attr("biggest_group_size") ;

  return res ;
}

void check_not_groups(const CharacterVector& result_names, const RowwiseDataFrame& gdf) {}
void check_not_groups(const LazyDots& dots, const RowwiseDataFrame& gdf) {}

void check_not_groups(const CharacterVector& result_names, const GroupedDataFrame& gdf) {
  int n = result_names.size() ;
  for (int i=0; i<n; i++) {
    if (gdf.has_group(result_names[i]))
      stop("cannot modify grouping variable") ;
  }
}
void check_not_groups(const LazyDots& dots, const GroupedDataFrame& gdf) {
  int n = dots.size() ;
  for (int i=0; i<n; i++) {
    if (gdf.has_group(dots[i].name()))
      stop("cannot modify grouping variable") ;
  }
}


SEXP mutate_not_grouped(DataFrame df, const LazyDots& dots) {
  int nexpr = dots.size() ;
  int nrows = df.nrows() ;

  NamedListAccumulator<DataFrame> accumulator ;
  int nvars = df.size() ;
  if (nvars) {
    CharacterVector df_names = df.names() ;
    for (int i=0; i<nvars; i++) {
      accumulator.set(df_names[i], df[i]) ;
    }
  }

  CallProxy call_proxy(df) ;
  List results(nexpr) ;

  for (int i=0; i<nexpr; i++) {
    Rcpp::checkUserInterrupt() ;
    const Lazy& lazy = dots[i] ;

    Shield<SEXP> call_(lazy.expr()) ;
    SEXP call = call_ ;
    SEXP name = lazy.name() ;
    Environment env = lazy.env() ;
    call_proxy.set_env(env) ;

    if (TYPEOF(call) == SYMSXP) {
      if (call_proxy.has_variable(call)) {
        results[i] = call_proxy.get_variable(PRINTNAME(call)) ;
      } else {
        results[i] = shared_SEXP(env.find(CHAR(PRINTNAME(call)))) ;
      }
    } else if (TYPEOF(call) == LANGSXP) {
      call_proxy.set_call(call);
      results[i] = call_proxy.eval() ;
    } else if (Rf_length(call) == 1) {
      boost::scoped_ptr<Gatherer> gather(constant_gatherer(call, nrows));
      results[i] = gather->collect() ;
    } else if (Rf_isNull(call)) {
      accumulator.rm(name) ;
      continue ;
    } else {
      stop("cannot handle") ;
    }

    check_supported_type(results[i], name) ;

    if (Rf_inherits(results[i], "POSIXlt")) {
      stop("`mutate` does not support `POSIXlt` results");
    }
    int n_res = Rf_length(results[i]) ;
    if (n_res == nrows) {
      // ok
    } else if (n_res == 1) {
      // recycle
      boost::scoped_ptr<Gatherer> gather(constant_gatherer(results[i] , df.nrows()));
      results[i] = gather->collect() ;
    } else {
      stop("wrong result size (%d), expected %d or 1", n_res, nrows) ;
    }

    call_proxy.input(name, results[i]) ;
    accumulator.set(name, results[i]);
  }
  List res = structure_mutate(accumulator, df, classes_not_grouped()) ;

  return res ;
}

template <typename Data, typename Subsets>
SEXP mutate_grouped(const DataFrame& df, const LazyDots& dots) {
  // special 0 rows case
  if (df.nrows() == 0) {
    DataFrame res = mutate_not_grouped(df, dots) ;
    res.attr("vars") = df.attr("vars") ;
    res.attr("class") = df.attr("class") ;
    return Data(res).data() ;
  }

  typedef GroupedCallProxy<Data, Subsets> Proxy;
  Data gdf(df);
  int nexpr = dots.size() ;
  check_not_groups(dots, gdf);

  Proxy proxy(gdf) ;

  NamedListAccumulator<Data> accumulator ;
  int ncolumns = df.size() ;
  CharacterVector column_names = df.names() ;
  for (int i=0; i<ncolumns; i++) {
    accumulator.set(column_names[i], df[i]) ;
  }

  List variables(nexpr) ;
  for (int i=0; i<nexpr; i++) {
    Rcpp::checkUserInterrupt() ;
    const Lazy& lazy = dots[i] ;

    Environment env = lazy.env() ;
    Shield<SEXP> call_(lazy.expr());
    SEXP call = call_ ;
    SEXP name = lazy.name() ;
    proxy.set_env(env) ;

    if (TYPEOF(call) == SYMSXP) {
      if (proxy.has_variable(call)) {
        SEXP variable = variables[i] = proxy.get_variable(PRINTNAME(call)) ;
        proxy.input(name, variable) ;
        accumulator.set(name, variable) ;
      } else {
        SEXP v = env.find(CHAR(PRINTNAME(call))) ;
        check_supported_type(v, name) ;
        if (Rf_isNull(v)) {
          stop("unknown variable: %s", CHAR(PRINTNAME(call)));
        } else if (Rf_length(v) == 1) {
          boost::scoped_ptr<Gatherer> rep(constant_gatherer(v, gdf.nrows()));
          SEXP variable = variables[i] = rep->collect() ;
          proxy.input(name, variable) ;
          accumulator.set(name, variable) ;
        } else {
          int n = Rf_length(v) ;
          bool test = all(gdf.get_group_sizes() == n).is_true() ;
          if (!test) {
            stop("impossible to replicate vector of size %d", n);
          }

          boost::scoped_ptr<Replicator> rep(replicator<Data>(v, gdf)) ;
          SEXP variable = variables[i] = rep->collect() ;
          proxy.input(name, variable) ;
          accumulator.set(name, variable) ;
        }
      }

    } else if (TYPEOF(call) == LANGSXP) {
      proxy.set_call(call);
      boost::scoped_ptr<Gatherer> gather(gatherer<Data, Subsets>(proxy, gdf, name));
      SEXP variable = variables[i] = gather->collect() ;
      proxy.input(name, variable) ;
      accumulator.set(name, variable) ;
    } else if (Rf_length(call) == 1) {
      boost::scoped_ptr<Gatherer> gather(constant_gatherer(call, gdf.nrows()));
      SEXP variable = variables[i] = gather->collect() ;
      proxy.input(name, variable) ;
      accumulator.set(name, variable) ;
    } else if (Rf_isNull(call)) {
      accumulator.rm(name) ;
      continue ;
    } else {
      stop("cannot handle") ;
    }
  }

  return structure_mutate(accumulator, df, df.attr("class"));
}


// [[Rcpp::export]]
SEXP mutate_impl(DataFrame df, LazyDots dots) {
  if (dots.size() == 0) return df ;
  check_valid_colnames(df) ;
  if (is<RowwiseDataFrame>(df)) {
    return mutate_grouped<RowwiseDataFrame, LazyRowwiseSubsets>(df, dots);
  } else if (is<GroupedDataFrame>(df)) {
    return mutate_grouped<GroupedDataFrame, LazyGroupedSubsets>(df, dots);
  } else {
    return mutate_not_grouped(df, dots) ;
  }
}

// [[Rcpp::export]]
IntegerVector order_impl(List args, Environment env) {
  int nargs = args.size() ;
  SEXP tmp ;
  List variables(nargs) ;
  LogicalVector ascending(nargs) ;
  for (int i=0; i<nargs; i++) {
    tmp = args[i] ;
    if (TYPEOF(tmp) == LANGSXP && CAR(tmp) == Rf_install("desc")) {
      variables[i] = Rf_eval(CAR(CDR(tmp)), env) ;
      ascending[i] = false ;
    } else {
      variables[i] = Rf_eval(tmp, env);
      ascending[i] = true ;
    }
  }
  OrderVisitors o(variables,ascending, nargs) ;
  IntegerVector res = o.apply() ;
  res = res + 1 ;
  return res ;
}

// [[Rcpp::export]]
DataFrame sort_impl(DataFrame data) {
  IntegerVector index = OrderVisitors(data).apply() ;
  return DataFrameSubsetVisitors(data, data.names()).subset(index, "data.frame") ;
}

// [[Rcpp::export]]
IntegerVector group_size_grouped_cpp(GroupedDataFrame gdf) {
  return Count().process(gdf) ;
}

// [[Rcpp::export]]
SEXP n_distinct_multi(List variables, bool na_rm = false) {
  if (variables.length() == 0) {
    stop("need at least one column for n_distinct()");
  }

  MultipleVectorVisitors visitors(variables) ;
  SlicingIndex everything(0, visitors.nrows()) ;
  if (na_rm) {
    Count_Distinct_Narm<MultipleVectorVisitors> counter(visitors) ;
    return counter.process(everything) ;
  } else {
    Count_Distinct<MultipleVectorVisitors> counter(visitors) ;
    return counter.process(everything) ;
  }
}

// [[Rcpp::export]]
DataFrame as_regular_df(DataFrame df) {
  DataFrame copy(shallow_copy(df));
  SET_ATTRIB(copy, strip_group_attributes(df)) ;
  SET_OBJECT(copy, OBJECT(df)) ;
  copy.attr("class") = CharacterVector::create("data.frame") ;
  return copy ;
}

// [[Rcpp::export]]
DataFrame ungroup_grouped_df(DataFrame df) {
  DataFrame copy(shallow_copy(df));
  SET_ATTRIB(copy, strip_group_attributes(df)) ;
  return copy ;
}

// [[Rcpp::export]]
std::vector<std::vector<int> > split_indices(IntegerVector group, int groups) {
  std::vector<std::vector<int> > ids(groups);

  int n = group.size();
  for (int i = 0; i < n; ++i) {
    ids[group[i] - 1].push_back(i + 1);
  }

  return ids;
}


// simple internal debugging function to access the gp part of the SEXP
// only meant for internal use in dplyr debugging

// [[Rcpp::export]]
unsigned short gp(SEXP x) {
  return reinterpret_cast<sxpinfo_struct*>(x)->gp ;
}
