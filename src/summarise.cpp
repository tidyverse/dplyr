#include "pch.h"
#include <dplyr/main.h>

#include <boost/scoped_ptr.hpp>

#include <tools/Quosure.h>

#include <dplyr/GroupedDataFrame.h>
#include <dplyr/NaturalDataFrame.h>

#include <dplyr/Result/LazyGroupedSubsets.h>
#include <dplyr/Result/GroupedCallReducer.h>

#include <dplyr/NamedListAccumulator.h>
#include <dplyr/Groups.h>
#include <dplyr/DataMask.h>

#include <dplyr/tbl_cpp.h>

using namespace Rcpp;
using namespace dplyr;

static
SEXP validate_unquoted_value(SEXP value, int nrows, const SymbolString& name) {
  int n = Rf_length(value);
  check_length(n, nrows, "the number of groups", name);

  // Recycle length 1 vectors
  if (n == 1) {
    value = constant_recycle(value, nrows, name);
  }

  return value;
}

template <int RTYPE>
class ExtractVectorVisitor {
public:
  typedef typename Rcpp::Vector<RTYPE> Vec;

  ExtractVectorVisitor(SEXP origin_) :
    origin(origin_)
  {}

  virtual SEXP extract(const std::vector<IntegerVector>& new_indices) {
    int n = new_indices.size();
    Vec out = no_init(n);
    copy_most_attributes(out, origin);
    for (int i = 0; i < n; i++) {
      int new_index = new_indices[i][0];
      out[i] = origin[new_index - 1];
    }
    return out ;
  }

private:
  Vec origin;
};

inline SEXP extract_visit(SEXP origin, const std::vector<IntegerVector>& new_indices) {
  switch (TYPEOF(origin)) {
  case INTSXP:
    return ExtractVectorVisitor<INTSXP>(origin).extract(new_indices);
  case REALSXP:
    return ExtractVectorVisitor<REALSXP>(origin).extract(new_indices);
  case LGLSXP:
    return ExtractVectorVisitor<LGLSXP>(origin).extract(new_indices);
  case STRSXP:
    return ExtractVectorVisitor<STRSXP>(origin).extract(new_indices);
  case RAWSXP:
    return ExtractVectorVisitor<RAWSXP>(origin).extract(new_indices);
  case CPLXSXP:
    return ExtractVectorVisitor<CPLXSXP>(origin).extract(new_indices);
  }

  return R_NilValue;
}

SEXP reconstruct_groups(const DataFrame& old_groups, const std::vector<IntegerVector>& new_indices) {
  int nv = old_groups.size() - 1 ;
  List out(nv);
  CharacterVector names(nv);
  CharacterVector old_names(old_groups.names());
  for (int i = 0; i < nv - 1; i++) {
    out[i] = extract_visit(old_groups[i], new_indices);
    names[i] = old_names[i];
  }
  out[nv - 1] = new_indices;
  names[nv - 1] = ".rows";

  set_rownames(out, new_indices.size());
  set_class(out, classes_not_grouped());
  out.attr("names") = names;
  return out ;
}

template <typename SlicedTibble>
void structure_summarise(List& out, const SlicedTibble& df) {
  set_class(out, classes_not_grouped());
}

template <>
void structure_summarise<GroupedDataFrame>(List& out, const GroupedDataFrame& gdf) {
  const DataFrame& df = gdf.data();

  if (gdf.nvars() > 1) {
    copy_class(out, df);
    SymbolVector vars = gdf.get_vars();
    vars.remove(gdf.nvars() - 1);

    DataFrame old_groups = gdf.group_data();
    int nv = gdf.nvars() - 1;
    DataFrameVisitors visitors(old_groups, nv) ;

    // collect the new indices
    std::vector<IntegerVector> new_indices;
    int old_nrows = old_groups.nrow();
    for (int i = 0; i < old_nrows;) {
      int start = i;
      while (i < old_nrows && visitors.equal(start, i)) i++ ;
      new_indices.push_back(seq(start + 1, i));
    }

    // groups
    DataFrame groups = reconstruct_groups(old_groups, new_indices);
    GroupedDataFrame::set_groups(out, groups);
  } else {
    // clear groups and reset to non grouped classes
    GroupedDataFrame::strip_groups(out);
    out.attr("class") = classes_not_grouped();
  }
}

template <typename SlicedTibble>
DataFrame summarise_grouped(const DataFrame& df, const QuosureList& dots) {
  typedef LazySplitSubsets<SlicedTibble> LazySubsets ;
  SlicedTibble gdf(df);

  int nexpr = dots.size();
  int nvars = gdf.nvars();
  check_not_groups(dots, gdf);

  LOG_VERBOSE << "copying " << nvars << " variables to accumulator";

  NamedListAccumulator<SlicedTibble> accumulator;
  int i = 0;
  List results(nvars + nexpr);
  for (; i < nvars; i++) {
    LOG_VERBOSE << "copying " << gdf.symbol(i).get_utf8_cstring();
    results[i] = shared_SEXP(gdf.label(i));
    accumulator.set(gdf.symbol(i), results[i]);
  }

  LOG_VERBOSE <<  "processing " << nexpr << " variables";

  LazySubsets subsets(gdf);
  for (int k = 0; k < nexpr; k++, i++) {
    LOG_VERBOSE << "processing variable " << k;
    Rcpp::checkUserInterrupt();
    const NamedQuosure& quosure = dots[k];
    const Environment& env = quosure.env();
    DataMask<SlicedTibble> data_mask(subsets, env);

    LOG_VERBOSE << "processing variable " << quosure.name().get_utf8_cstring();

    SEXP expr = quosure.expr();
    RObject result;

    // Unquoted vectors are directly used as column. Expressions are
    // evaluated in each group.
    if (is_vector(expr)) {
      result = validate_unquoted_value(expr, gdf.ngroups(), quosure.name());
    } else {
      // //boost::scoped_ptr<Result> res(get_handler(expr, subsets, env));
      //
      // // If we could not find a direct Result,
      // // we can use a GroupedCallReducer which will callback to R.
      // // Note that the GroupedCallReducer currently doesn't apply
      // // special treatment to summary variables, for which hybrid
      // // evaluation should be turned off completely (#2312)
      // if (!res) {
      //   res.reset(new GroupedCallReducer<SlicedTibble, LazySubsets>(quosure.expr(), subsets, env, quosure.name()));
      // }
      // result = res->process(gdf);

      result = GroupedCallReducer<SlicedTibble>(quosure.expr(), quosure.name(), data_mask).process(gdf);
    }
    check_not_null(result, quosure.name());
    check_length(Rf_length(result), gdf.ngroups(), "a summary value", quosure.name());

    results[i] = result;
    accumulator.set(quosure.name(), result);
    subsets.input_summarised(quosure.name(), SummarisedVariable(result));
  }

  List out = accumulator;
  // so that the attributes of the original tibble are preserved
  // as requested in issue #1064
  copy_most_attributes(out, df);
  out.names() = accumulator.names();

  int nr = gdf.ngroups();
  set_rownames(out, nr);
  structure_summarise<SlicedTibble>(out, gdf) ;
  return out;
}

// [[Rcpp::export]]
SEXP summarise_impl(DataFrame df, QuosureList dots) {
  check_valid_colnames(df);
  if (is<RowwiseDataFrame>(df)) {
    return summarise_grouped<RowwiseDataFrame>(df, dots);
  } else if (is<GroupedDataFrame>(df)) {
    return summarise_grouped<GroupedDataFrame>(df, dots);
  } else {
    return summarise_grouped<NaturalDataFrame>(df, dots);
  }
}
