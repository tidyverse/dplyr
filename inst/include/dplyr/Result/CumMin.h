#ifndef dplyr_Result_CumMin_H
#define dplyr_Result_CumMin_H

namespace dplyr {
    
    template <int RTYPE>
    class CumMin : public Mutater<RTYPE, CumMin<RTYPE> > {
    public:
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;
        
        CumMin(SEXP data_) : data(data_){}
                            
        void process_slice( Vector<RTYPE>& out, const SlicingIndex& index, const SlicingIndex& out_index){
            STORAGE value = data[index[0]] ;
            out[out_index[0]] = value ; 
            
            int n = index.size() ;
            for( int i=1; i<n; i++){
                value = std::min( value, data[index[i]] );
                out[out_index[i]] = value ;
            }
        }
        
        Vector<RTYPE> data ;
    } ;
    
}

#endif
