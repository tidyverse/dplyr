#ifndef dplyr_Result_CumMax_H
#define dplyr_Result_CumMax_H

namespace dplyr {

    // version for REALSXP
    template <int RTYPE>
    class CumMax : public Mutater<RTYPE, CumMax<RTYPE> > {
    public:
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;

        CumMax(SEXP data_) : data(data_){}

        void process_slice( Vector<RTYPE>& out, const SlicingIndex& index, const SlicingIndex& out_index){
            int n = index.size() ;
            STORAGE value = data[index[0]] ;
            out[out_index[0]] = value ;
            if( NumericVector::is_na(value) ){
                for( int i=1; i<n; i++){
                    out[out_index[i]] = value ;
                }
                return ;
            }

            for( int i=1; i<n; i++){
                STORAGE current = data[index[i]] ;
                if( Rcpp::traits::is_na<RTYPE>(current) ){
                    for(int j=i; j<n; j++){
                        out[out_index[j]] = current ;
                    }
                    return ;
                }
                if( current > value ) value = current ;
                out[out_index[i]] = value ;
            }
        }

        Vector<RTYPE> data ;
    } ;

}

#endif
