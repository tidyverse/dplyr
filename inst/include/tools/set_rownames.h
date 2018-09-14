#ifndef dplyr_tools_set_rownames_H
#define dplyr_tools_set_rownames_H

namespace dplyr {

template <typename Df>
inline void set_rownames(Df& data, int n) {
  data.attr("row.names") =
    Rcpp::IntegerVector::create(Rcpp::IntegerVector::get_na(), -n);
}

}

#endif
