#include <Rcpp.h>
#include <boost/shared_ptr.hpp>
#include <dplyr/symbols.h>

class Expander {
public:
  virtual ~Expander() {};
  virtual int size() const = 0;
};

boost::shared_ptr<Expander> expander(const std::vector<SEXP>& data, const std::vector<int*>& positions, int depth, int index, int start, int end);

class FactorExpander : public Expander {
public:
  FactorExpander(const std::vector<SEXP>& data_, const std::vector<int*>& positions_, int depth_, int index_, int start_, int end_) :
    data(data_),
    positions(positions_),
    index(index_),
    start(start_),
    end(end_)
  {
    SEXP fac = data[depth_];
    SEXP levels = Rf_getAttrib(fac, dplyr::symbols::levels);
    int n_levels = XLENGTH(levels);
    expanders.resize(n_levels);

    int* fac_pos = positions[depth_];

    // for each level, setup an expander for `depth + 1`
    int j = start;
    for (int i = 0; i < n_levels; i++) {
      int start_i = j;
      while (j < end && fac_pos[j] == i + 1) j++;
      expanders[i] = expander(data, positions, depth_ + 1, i + 1, start_i, j);
      // TODO: implicit NA
    }
  }
  ~FactorExpander(){}

  virtual int size() const {
    int n = 0;
    for (int i=0; i<expanders.size(); i++) n += expanders[i]->size();
    return n;
  }

private:
  const std::vector<SEXP>& data;
  const std::vector<int*>& positions;
  int index;
  int start;
  int end;

  std::vector<boost::shared_ptr<Expander>> expanders;
};

class VectorExpander : public Expander {
public:
  VectorExpander(const std::vector<SEXP>& data_, const std::vector<int*>& positions_, int depth_, int index_, int start_, int end_) {}
  ~VectorExpander(){}

  virtual int size() const {
    return 1;
  }
};

class LeafExpander : public Expander {
public:
  LeafExpander(const std::vector<SEXP>& data_, const std::vector<int*>& positions_, int depth_, int index_, int start_, int end_) :
    index(index_),
    start(start_),
    end(end_)
  {}
  ~LeafExpander(){}

  virtual int size() const {
    return 1;
  }
private:
  int index;
  int start;
  int end;
};


boost::shared_ptr<Expander> expander(const std::vector<SEXP>& data, const std::vector<int*>& positions, int depth, int index, int start, int end) {
  if (depth == positions.size()) {
    return boost::shared_ptr<Expander>(new LeafExpander(data, positions, depth, index, start, end));
  } else if (Rf_isFactor(data[depth])) {
    return boost::shared_ptr<Expander>(new FactorExpander(data, positions, depth, index, start, end));
  } else {
    return boost::shared_ptr<Expander>(new VectorExpander(data, positions, depth, index, start, end));
  }
}

// [[Rcpp::export(rng = false)]]
int expand_groups(Rcpp::DataFrame old_groups, Rcpp::List positions) {
  int nvars = old_groups.size() - 1;
  int nr = XLENGTH(positions[0]);

  SEXP names = Rf_getAttrib(old_groups, R_NamesSymbol);
  Rcpp::List old_rows(old_groups[nvars]);
  std::vector<SEXP> vec_data(nvars);
  std::vector<int*> vec_positions(nvars);
  for (int i = 0; i < nvars; i++) {
    vec_data[i] = old_groups[i];
    vec_positions[i] = INTEGER(VECTOR_ELT(positions, i));
  }

  boost::shared_ptr<Expander> exp = expander(vec_data, vec_positions, 0, NA_INTEGER, 0, nr);

  return exp->size();
}
