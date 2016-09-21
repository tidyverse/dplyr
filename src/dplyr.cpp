#include <dplyr.h>

using namespace Rcpp;
using namespace dplyr;

// [[Rcpp::export]]
void assert_all_white_list(const DataFrame& data) {
  // checking variables are on the white list
  int nc = data.size();
  for (int i=0; i<nc; i++) {
    if (!white_list(data[i])) {
      CharacterVector names = data.names();
      String name_i = names[i];
      SEXP v = data[i];

      SEXP klass = Rf_getAttrib(v, R_ClassSymbol);
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
SEXP shallow_copy(const List& data) {
  int n = data.size();
  List out(n);
  for (int i=0; i<n; i++) {
    out[i] = shared_SEXP(data[i]);
  }
  copy_attributes(out, data);
  return out;
}

// [[Rcpp::export]]
dplyr::BoolResult compatible_data_frame_nonames(DataFrame x, DataFrame y, bool convert) {
  int n = x.size();
  if (n != y.size())
    return no_because(tfm::format("different number of columns : %d x %d", n, y.size()));

  if (convert) {
    for (int i=0; i<n; i++) {
      try {
        boost::scoped_ptr<JoinVisitor> v(join_visitor(x[i], y[i], "x", "x", true));
      } catch (...) {
        return no_because("incompatible");
      }
    }
  } else {
    for (int i=0; i<n; i++) {
      SEXP xi = x[i], yi=y[i];
      if (TYPEOF(xi) != TYPEOF(yi))
        return no_because("incompatible types");

      if (TYPEOF(xi) == INTSXP) {
        if (Rf_inherits(xi, "factor") && Rf_inherits(yi, "factor")) {
          if (same_levels(xi, yi)) continue;
          return no_because("factors with different levels");
        }

        if (Rf_inherits(xi, "factor")) return no_because("cannot compare factor and integer");
        if (Rf_inherits(yi, "factor")) return no_because("cannot compare factor and integer");

      }
    }
  }

  return yes();

}

// [[Rcpp::export]]
dplyr::BoolResult compatible_data_frame(DataFrame x, DataFrame y, bool ignore_col_order = true, bool convert = false) {
  int n = x.size();

  bool null_x = Rf_isNull(x.names()), null_y = Rf_isNull(y.names());
  if (null_x && !null_y) {
    return no_because("x does not have names, but y does");
  } else if (null_y && !null_x) {
    return no_because("y does not have names, but x does");
  } else if (null_x && null_y) {
    return compatible_data_frame_nonames(x,y, convert);
  }

  CharacterVector names_x = x.names();
  CharacterVector names_y = y.names();

  CharacterVector names_y_not_in_x = setdiff(names_y, names_x);
  CharacterVector names_x_not_in_y = setdiff(names_x, names_y);

  if (!ignore_col_order) {
    if (names_y_not_in_x.size() == 0 && names_y_not_in_x.size() == 0) {
      // so the names are the same, check if they are in the same order
      for (int i=0; i<n; i++) {
        if (names_x[i] != names_y[i]) {
          return no_because("Same column names, but different order");
        }
      }
    }
  }

  std::stringstream ss;
  bool ok = true;
  if (names_y_not_in_x.size()) {
    ok = false;
    ss << "Cols in y but not x: " << collapse(names_y_not_in_x) << ". ";
  }

  if (names_x_not_in_y.size()) {
    ok = false;
    ss << "Cols in x but not y: " << collapse(names_x_not_in_y) << ". ";
  }

  if (!ok) {
    return no_because(ss.str());
  }

  IntegerVector orders = r_match(names_x, names_y);

  String name;
  for (int i=0; i<n; i++) {
    name = names_x[i];
    SEXP xi = x[i], yi = y[orders[i]-1];
    boost::scoped_ptr<SubsetVectorVisitor> vx(subset_visitor(xi));
    boost::scoped_ptr<SubsetVectorVisitor> vy(subset_visitor(yi));
    SubsetVectorVisitor* px = vx.get();
    SubsetVectorVisitor* py = vy.get();

    if (typeid(*px) != typeid(*py)) {
      ss << "Incompatible type for column "
         << name.get_cstring()
         << ": x " << vx->get_r_type()
         << ", y " << vy->get_r_type();

      if (!convert) {
        ok = false;
        continue;
      }
    }

    if (! vx->is_compatible(py, ss, name)) {
      ok = false;
    }
  }

  if (!ok) return no_because(ss.str());
  return yes();
}

class RowTrack {
public:
  RowTrack(const std::string& msg, int max_count_ = 10) : ss(), count(0), max_count(max_count_) {
    ss << msg;
  }

  void record(int i) {
    if (count > max_count) return;
    if (count) ss << ", ";
    int idx = i >= 0 ? (i+1) : -i;
    ss << idx;
    if (count == max_count) ss << "[...]";
    count++;
  }

  bool empty() const {
    return count == 0;
  }

  std::string str() const {
    return ss.str();
  }

private:
  std::stringstream ss;
  int count;
  int max_count;
};

// [[Rcpp::export]]
dplyr::BoolResult equal_data_frame(DataFrame x, DataFrame y, bool ignore_col_order = true, bool ignore_row_order = true, bool convert = false) {
  BoolResult compat = compatible_data_frame(x, y, ignore_col_order, convert);
  if (!compat) return compat;

  typedef VisitorSetIndexMap<DataFrameJoinVisitors, std::vector<int> > Map;
  DataFrameJoinVisitors visitors(x, y, x.names(), x.names(), true);
  Map map(visitors);

  // train the map in both x and y
  int nrows_x = x.nrows();
  int nrows_y = y.nrows();

  if (nrows_x != nrows_y)
    return no_because("Different number of rows");
  if (x.size() == 0)
    return yes();

  for (int i=0; i<nrows_x; i++) map[i].push_back(i);
  for (int i=0; i<nrows_y; i++) map[-i-1].push_back(-i-1);

  RowTrack track_x("Rows in x but not y: ");
  RowTrack track_y("Rows in y but not x: ");
  RowTrack track_mismatch("Rows with difference occurences in x and y: ");

  bool ok = true;
  Map::const_iterator it = map.begin();

  for (; it != map.end(); ++it) {
    // retrieve the indices ( -ves for y, +ves for x )
    const std::vector<int>& chunk = it->second;
    int n = chunk.size();

    int count_left = 0, count_right = 0;
    for (int i=0; i<n; i++) {
      if (chunk[i] < 0)
        count_right++;
      else
        count_left++;
    }
    if (count_right == 0) {
      track_x.record(chunk[0]);
      ok = false;
    } else if (count_left == 0) {
      track_y.record(chunk[0]);
      ok = false;
    } else if (count_left != count_right) {
      track_mismatch.record(chunk[0]);
      ok = false;
    }

  }

  if (!ok) {
    std::stringstream ss;
    if (! track_x.empty()) ss << track_x.str() << ". ";
    if (! track_y.empty()) ss << track_y.str() << ". ";
    if (! track_mismatch.empty()) ss << track_mismatch.str();

    return no_because(ss.str());
  }

  if (ok && ignore_row_order) return yes();

  if (!ignore_row_order) {
    for (int i=0; i<nrows_x; i++) {
      if (!visitors.equal(i, -i-1)) {
        return no_because("Same row values, but different order");
      }
    }
  }

  return yes();
}

// [[Rcpp::export]]
dplyr::BoolResult all_equal_data_frame(List args, Environment env) {
  int n = args.size();
  DataFrame x0 = Rf_eval(args[0], env);
  for (int i=1; i<n; i++) {
    BoolResult test = equal_data_frame(x0, Rf_eval(args[i], env));
    if (!test) return test;
  }
  return yes();
}

// [[Rcpp::export]]
DataFrame union_data_frame(DataFrame x, DataFrame y) {
  BoolResult compat = compatible_data_frame(x,y,true,true);
  if (!compat) {
    stop("not compatible: %s", compat.why_not());
  }

  typedef VisitorSetIndexSet<DataFrameJoinVisitors> Set;
  DataFrameJoinVisitors visitors(x, y, x.names(), x.names(), true);
  Set set(visitors);

  train_insert(set, x.nrows());
  train_insert_right(set, y.nrows());

  return visitors.subset(set, x.attr("class"));
}

// [[Rcpp::export]]
DataFrame intersect_data_frame(DataFrame x, DataFrame y) {
  BoolResult compat = compatible_data_frame(x,y,true,true);
  if (!compat) {
    stop("not compatible: %s", compat.why_not());
  }
  typedef VisitorSetIndexSet<DataFrameJoinVisitors> Set;

  DataFrameJoinVisitors visitors(x, y, x.names(), x.names(), true);
  Set set(visitors);

  train_insert(set, x.nrows());

  std::vector<int> indices;
  int n_y = y.nrows();
  for (int i=0; i<n_y; i++) {
    Set::iterator it = set.find(-i-1);
    if (it != set.end()) {
      indices.push_back(*it);
      set.erase(it);
    }
  }

  return visitors.subset(indices, x.attr("class"));
}

// [[Rcpp::export]]
DataFrame setdiff_data_frame(DataFrame x, DataFrame y) {
  BoolResult compat = compatible_data_frame(x,y,true,true);
  if (!compat) {
    stop("not compatible: %s", compat.why_not());
  }

  typedef VisitorSetIndexSet<DataFrameJoinVisitors> Set;
  DataFrameJoinVisitors visitors(y, x, y.names(), y.names(), true);
  Set set(visitors);

  train_insert(set, y.nrows());

  std::vector<int> indices;

  int n_x = x.nrows();
  for (int i=0; i<n_x; i++) {
    if (!set.count(-i-1)) {
      set.insert(-i-1);
      indices.push_back(-i-1);
    }
  }

  return visitors.subset(indices, x.attr("class"));
}

// [[Rcpp::export]]
IntegerVector match_data_frame(DataFrame x, DataFrame y) {
  if (!compatible_data_frame(x,y,true,true))
    stop("not compatible");

  typedef VisitorSetIndexSet<DataFrameJoinVisitors> Set;
  DataFrameJoinVisitors visitors(y, x, x.names(), x.names(), true);
  Set set(visitors);

  train_insert(set, y.nrows());

  int n_x = x.nrows();
  IntegerVector res = no_init(n_x);
  for (int i=0; i<n_x; i++) {
    Set::iterator it = set.find(-i-1);
    res[i] = (it == set.end()) ? NA_INTEGER : (*it+1);
  }

  return res;
}

// [[Rcpp::export]]
SEXP resolve_vars(List new_groups, CharacterVector names) {
  int n = new_groups.size();
  for (int i=0; i<n; i++) {
    List lazy = new_groups[i];
    Environment env = lazy[1];
    SEXP s = lazy[0];

    // expand column
    if (TYPEOF(s) == SYMSXP) {

    } else if (TYPEOF(s) == LANGSXP && CAR(s) == Rf_install("column") && Rf_length(s) == 2) {
      s = extract_column(CADR(s), env);
    } else {
      continue;
    }
    // check that s is indeed in the data

    int pos = as<int>(r_match(CharacterVector::create(PRINTNAME(s)), names));
    if (pos == NA_INTEGER) {
      stop("unknown variable to group by : %s", CHAR(PRINTNAME(s)));
    }
    lazy[0] = s;
  }

  return new_groups;
}

// [[Rcpp::export]]
DataFrame grouped_df_impl(DataFrame data, ListOf<Symbol> symbols, bool drop) {
  assert_all_white_list(data);
  DataFrame copy(shallow_copy(data));
  copy.attr("vars") = symbols;
  copy.attr("drop") = drop;
  if (!symbols.size())
    stop("no variables to group by");
  return build_index_cpp(copy);
}

DataFrame build_index_cpp(DataFrame data) {
  ListOf<Symbol> symbols(data.attr("vars"));

  int nsymbols = symbols.size();
  CharacterVector vars(nsymbols);
  CharacterVector names = data.names();
  for (int i=0; i<nsymbols; i++) {
    vars[i] = PRINTNAME(symbols[i]);
  }
  IntegerVector indx = r_match(vars, names);

  for (int i=0; i<nsymbols; i++) {
    int pos = indx[i];
    if (pos == NA_INTEGER) {
      stop("unknown column '%s' ", CHAR(names[i]));
    }

    SEXP v = data[pos-1];

    if (!white_list(v) || TYPEOF(v) == VECSXP) {
      const char* name = vars[i];
      stop("cannot group column %s, of class '%s'", name, get_single_class(v));
    }
  }

  DataFrameVisitors visitors(data, vars);
  ChunkIndexMap map(visitors);

  train_push_back(map, data.nrows());

  DataFrame labels = DataFrameSubsetVisitors(data, vars).subset(map, "data.frame");
  int ngroups = labels.nrows();
  IntegerVector labels_order = OrderVisitors(labels).apply();

  labels = DataFrameSubsetVisitors(labels).subset(labels_order, "data.frame");

  List indices(ngroups);
  IntegerVector group_sizes = no_init(ngroups);
  int biggest_group = 0;

  ChunkIndexMap::const_iterator it = map.begin();
  std::vector<const std::vector<int>* > chunks(ngroups);
  for (int i=0; i<ngroups; i++, ++it) {
    chunks[i] = &it->second;
  }

  for (int i=0; i<ngroups; i++) {
    int idx = labels_order[i];
    const std::vector<int>& chunk = *chunks[idx];
    indices[i] = chunk;
    group_sizes[i] = chunk.size();
    biggest_group = std::max(biggest_group, (int)chunk.size());
  }

  data.attr("indices") = indices;
  data.attr("group_sizes") = group_sizes;
  data.attr("biggest_group_size") = biggest_group;
  data.attr("labels") = labels;
  data.attr("class") = CharacterVector::create("grouped_df", "tbl_df", "tbl", "data.frame");
  return data;
}

DataFrame build_index_adj(DataFrame df, ListOf<Symbol> symbols) {
  int nsymbols = symbols.size();
  CharacterVector vars(nsymbols);
  for (int i=0; i<nsymbols; i++) {
    vars[i] = PRINTNAME(symbols[i]);
  }

  DataFrameVisitors visitors(df, vars);
  std::vector<int> sizes;
  int n = df.nrows();

  int i=0;
  while (i<n) {
    int start = i++;
    for (; i<n && visitors.equal(i, start); i++)
      ;
    sizes.push_back(i-start);
  }

  n = sizes.size();
  List indices(n);
  IntegerVector first = no_init(n);
  int start = 0;
  int biggest_group = 0;
  for (int i=0; i<n; i++) {
    first[i] = start;
    int end = start + sizes[i] - 1;
    indices[i] = seq(start, end);
    start = end + 1;
    biggest_group = std::max(biggest_group, sizes[i]);
  }

  df.attr("indices") = indices;
  df.attr("labels")  = DataFrameSubsetVisitors(df, vars).subset(first, "data.frame");
  df.attr("group_sizes") = sizes;
  df.attr("biggest_group_size") = biggest_group;
  df.attr("class") = CharacterVector::create("adj_grouped_df", "grouped_df", "tbl_df", "tbl", "data.frame");
  df.attr("vars") = symbols;

  return df;
}

// [[Rcpp::export]]
DataFrame grouped_df_adj_impl(DataFrame data, ListOf<Symbol> symbols, bool drop) {
  DataFrame copy(shallow_copy(data));
  copy.attr("vars") = symbols;
  copy.attr("drop") = drop;
  return build_index_adj(data, symbols);
}

template <typename Data>
SEXP structure_mutate(const NamedListAccumulator<Data>& accumulator, const DataFrame& df, CharacterVector classes) {
  List res = accumulator;
  res.attr("class") = classes;
  set_rownames(res, df.nrows());
  res.attr("vars")   = df.attr("vars");
  res.attr("labels")  = df.attr("labels");
  res.attr("index")  = df.attr("index");
  res.attr("indices") = df.attr("indices");
  res.attr("drop") = df.attr("drop");
  res.attr("group_sizes") = df.attr("group_sizes");
  res.attr("biggest_group_size") = df.attr("biggest_group_size");

  return res;
}

void check_not_groups(const CharacterVector& result_names, const RowwiseDataFrame& gdf) {}
void check_not_groups(const LazyDots& dots, const RowwiseDataFrame& gdf) {}

void check_not_groups(const CharacterVector& result_names, const GroupedDataFrame& gdf) {
  int n = result_names.size();
  for (int i=0; i<n; i++) {
    if (gdf.has_group(result_names[i]))
      stop("cannot modify grouping variable");
  }
}
void check_not_groups(const LazyDots& dots, const GroupedDataFrame& gdf) {
  int n = dots.size();
  for (int i=0; i<n; i++) {
    if (gdf.has_group(dots[i].name()))
      stop("cannot modify grouping variable");
  }
}


SEXP mutate_not_grouped(DataFrame df, const LazyDots& dots) {
  int nexpr = dots.size();
  int nrows = df.nrows();

  NamedListAccumulator<DataFrame> accumulator;
  int nvars = df.size();
  if (nvars) {
    CharacterVector df_names = df.names();
    for (int i=0; i<nvars; i++) {
      accumulator.set(df_names[i], df[i]);
    }
  }

  CallProxy call_proxy(df);
  List results(nexpr);

  for (int i=0; i<nexpr; i++) {
    Rcpp::checkUserInterrupt();
    const Lazy& lazy = dots[i];

    Shield<SEXP> call_(lazy.expr());
    SEXP call = call_;
    SEXP name = lazy.name();
    Environment env = lazy.env();
    call_proxy.set_env(env);

    if (TYPEOF(call) == SYMSXP) {
      if (call_proxy.has_variable(call)) {
        results[i] = call_proxy.get_variable(PRINTNAME(call));
      } else {
        results[i] = shared_SEXP(env.find(CHAR(PRINTNAME(call))));
      }
    } else if (TYPEOF(call) == LANGSXP) {
      call_proxy.set_call(call);
      results[i] = call_proxy.eval();
    } else if (Rf_length(call) == 1) {
      boost::scoped_ptr<Gatherer> gather(constant_gatherer(call, nrows));
      results[i] = gather->collect();
    } else if (Rf_isNull(call)) {
      accumulator.rm(name);
      continue;
    } else {
      stop("cannot handle");
    }

    check_supported_type(results[i], name);

    if (Rf_inherits(results[i], "POSIXlt")) {
      stop("`mutate` does not support `POSIXlt` results");
    }
    int n_res = Rf_length(results[i]);
    if (n_res == nrows) {
      // ok
    } else if (n_res == 1) {
      // recycle
      boost::scoped_ptr<Gatherer> gather(constant_gatherer(results[i] , df.nrows()));
      results[i] = gather->collect();
    } else {
      stop("wrong result size (%d), expected %d or 1", n_res, nrows);
    }

    call_proxy.input(name, results[i]);
    accumulator.set(name, results[i]);
  }
  List res = structure_mutate(accumulator, df, classes_not_grouped());

  return res;
}

template <typename Data, typename Subsets>
SEXP mutate_grouped(const DataFrame& df, const LazyDots& dots) {
  // special 0 rows case
  if (df.nrows() == 0) {
    DataFrame res = mutate_not_grouped(df, dots);
    res.attr("vars") = df.attr("vars");
    res.attr("class") = df.attr("class");
    return Data(res).data();
  }

  typedef GroupedCallProxy<Data, Subsets> Proxy;
  Data gdf(df);
  int nexpr = dots.size();
  check_not_groups(dots, gdf);

  Proxy proxy(gdf);

  NamedListAccumulator<Data> accumulator;
  int ncolumns = df.size();
  CharacterVector column_names = df.names();
  for (int i=0; i<ncolumns; i++) {
    accumulator.set(column_names[i], df[i]);
  }

  List variables(nexpr);
  for (int i=0; i<nexpr; i++) {
    Rcpp::checkUserInterrupt();
    const Lazy& lazy = dots[i];

    Environment env = lazy.env();
    Shield<SEXP> call_(lazy.expr());
    SEXP call = call_;
    SEXP name = lazy.name();
    proxy.set_env(env);

    if (TYPEOF(call) == SYMSXP) {
      if (proxy.has_variable(call)) {
        SEXP variable = variables[i] = proxy.get_variable(PRINTNAME(call));
        proxy.input(name, variable);
        accumulator.set(name, variable);
      } else {
        SEXP v = env.find(CHAR(PRINTNAME(call)));
        check_supported_type(v, name);
        if (Rf_isNull(v)) {
          stop("unknown variable: %s", CHAR(PRINTNAME(call)));
        } else if (Rf_length(v) == 1) {
          boost::scoped_ptr<Gatherer> rep(constant_gatherer(v, gdf.nrows()));
          SEXP variable = variables[i] = rep->collect();
          proxy.input(name, variable);
          accumulator.set(name, variable);
        } else {
          int n = Rf_length(v);
          bool test = all(gdf.get_group_sizes() == n).is_true();
          if (!test) {
            stop("impossible to replicate vector of size %d", n);
          }

          boost::scoped_ptr<Replicator> rep(replicator<Data>(v, gdf));
          SEXP variable = variables[i] = rep->collect();
          proxy.input(name, variable);
          accumulator.set(name, variable);
        }
      }

    } else if (TYPEOF(call) == LANGSXP) {
      proxy.set_call(call);
      boost::scoped_ptr<Gatherer> gather(gatherer<Data, Subsets>(proxy, gdf, name));
      SEXP variable = variables[i] = gather->collect();
      proxy.input(name, variable);
      accumulator.set(name, variable);
    } else if (Rf_length(call) == 1) {
      boost::scoped_ptr<Gatherer> gather(constant_gatherer(call, gdf.nrows()));
      SEXP variable = variables[i] = gather->collect();
      proxy.input(name, variable);
      accumulator.set(name, variable);
    } else if (Rf_isNull(call)) {
      accumulator.rm(name);
      continue;
    } else {
      stop("cannot handle");
    }
  }

  return structure_mutate(accumulator, df, df.attr("class"));
}


// [[Rcpp::export]]
SEXP mutate_impl(DataFrame df, LazyDots dots) {
  if (dots.size() == 0) return df;
  check_valid_colnames(df);
  if (is<RowwiseDataFrame>(df)) {
    return mutate_grouped<RowwiseDataFrame, LazyRowwiseSubsets>(df, dots);
  } else if (is<GroupedDataFrame>(df)) {
    return mutate_grouped<GroupedDataFrame, LazyGroupedSubsets>(df, dots);
  } else {
    return mutate_not_grouped(df, dots);
  }
}

// [[Rcpp::export]]
IntegerVector order_impl(List args, Environment env) {
  int nargs = args.size();
  SEXP tmp;
  List variables(nargs);
  LogicalVector ascending(nargs);
  for (int i=0; i<nargs; i++) {
    tmp = args[i];
    if (TYPEOF(tmp) == LANGSXP && CAR(tmp) == Rf_install("desc")) {
      variables[i] = Rf_eval(CAR(CDR(tmp)), env);
      ascending[i] = false;
    } else {
      variables[i] = Rf_eval(tmp, env);
      ascending[i] = true;
    }
  }
  OrderVisitors o(variables,ascending, nargs);
  IntegerVector res = o.apply();
  res = res + 1;
  return res;
}

// [[Rcpp::export]]
DataFrame sort_impl(DataFrame data) {
  IntegerVector index = OrderVisitors(data).apply();
  return DataFrameSubsetVisitors(data, data.names()).subset(index, "data.frame");
}

// [[Rcpp::export]]
IntegerVector group_size_grouped_cpp(GroupedDataFrame gdf) {
  return Count().process(gdf);
}

// [[Rcpp::export]]
DataFrame as_regular_df(DataFrame df) {
  DataFrame copy(shallow_copy(df));
  SET_ATTRIB(copy, strip_group_attributes(df));
  SET_OBJECT(copy, OBJECT(df));
  copy.attr("class") = CharacterVector::create("data.frame");
  return copy;
}

// [[Rcpp::export]]
DataFrame ungroup_grouped_df(DataFrame df) {
  DataFrame copy(shallow_copy(df));
  SET_ATTRIB(copy, strip_group_attributes(df));
  return copy;
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
  return reinterpret_cast<sxpinfo_struct*>(x)->gp;
}
