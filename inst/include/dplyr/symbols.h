#ifndef DPLYR_SYMBOLS_H
#define DPLYR_SYMBOLS_H

namespace dplyr {

struct symbols_t {
  SEXP package;
  SEXP n;
  SEXP tzone;
  SEXP units;
  SEXP dot_env;
  SEXP dot_data;

  SEXP sum;
  SEXP mean;
  SEXP var;
  SEXP sd;
  SEXP n_distinct;
  SEXP first;
  SEXP last;
  SEXP nth;
  SEXP group_indices;
  SEXP min;
  SEXP max;
  SEXP row_number;
  SEXP ntile;
  SEXP min_rank;
  SEXP percent_rank;
  SEXP dense_rank;
  SEXP cume_dist;
  SEXP lead;
  SEXP lag;
  SEXP in;

  SEXP narm;
  SEXP default_;

  SEXP dplyr;
  SEXP base;
  SEXP stats;

  SEXP desc;
  SEXP double_colon;
  SEXP na_rm;
  SEXP new_env;
  SEXP comment;
  SEXP groups;
  SEXP vars;

  symbols_t() {
    package = Rf_install("package");
    n = Rf_install("n");
    tzone = Rf_install("tzone");
    units = Rf_install("units");
    dot_env = Rf_install(".env");
    dot_data = Rf_install(".data");

    sum = Rf_install("sum");
    mean = Rf_install("mean");
    var = Rf_install("var");
    sd = Rf_install("sd");
    n_distinct = Rf_install("n_distinct");
    first = Rf_install("first");
    last = Rf_install("last");
    nth = Rf_install("nth");
    group_indices = Rf_install("group_indices");
    min = Rf_install("min");
    max = Rf_install("max");
    row_number = Rf_install("row_number");
    ntile = Rf_install("ntile");
    min_rank = Rf_install("min_rank");
    percent_rank = Rf_install("percent_rank");
    dense_rank = Rf_install("dense_rank");
    cume_dist = Rf_install("cume_dist");
    lead = Rf_install("lead");
    lag = Rf_install("lag");
    in = Rf_install("%in%");

    narm = Rf_install("na.rm");
    default_ = Rf_install("default");

    dplyr = Rf_install("dplyr");
    base = Rf_install("base");
    stats = Rf_install("stats");

    desc = Rf_install("desc");
    double_colon = Rf_install("::");
    na_rm = Rf_install("na.rm");
    new_env = Rf_install("new.env");
    comment = Rf_install("comment");
    groups = Rf_install("groups");
    vars = Rf_install("vars");
  }

};

symbols_t& symbols();

}


#endif
