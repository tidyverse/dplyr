#ifndef dplyr_tools_FilteredListOf_H
#define dplyr_tools_FilteredListOf_H

namespace Rcpp {

    template <typename T>
    class FilteredListOf {
    public:

        FilteredListOf(SEXP data_) : data(data_){
          int n = data.size() ;
          for( int i=0; i<n; i++){
            indices.push_back(i) ;
          }
        }

        T operator[](int i) const {
            return as<T>( data[indices[i]]) ;
        }

        int size() const {
            return indices.size() ;
        }

    private:
        List data ;
        std::vector<int> indices ;
    } ;
}

#endif
