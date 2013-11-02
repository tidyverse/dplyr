#ifndef dplyr_Collecter_H
#define dplyr_Collecter_H

namespace dplyr {
    
    class Collecter {
    public:
        virtual ~Collecter(){} ;
        virtual void collect( const SlicingIndex& index, SEXP v ) = 0 ;
        virtual SEXP get() const = 0 ;
    } ;
    
    template <int RTYPE>
    class Collecter_Impl : public Collecter {
    public:
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;
        
        Collecter_Impl( int n_ ): data( n_, Rcpp::traits::get_na<RTYPE>() ){}
        
        void collect( const SlicingIndex& index, SEXP v ){
            Vector<RTYPE> source(v) ;
            STORAGE* source_ptr = Rcpp::internal::r_vector_start<RTYPE>(source) ;
            for( int i=0; i<index.size(); i++){
                data[index[i]] = source_ptr[i] ;
            }
        }
        
        inline SEXP get() const{
            return data ;    
        }
        
    private:
        Vector<RTYPE> data ;
    } ;
    
    inline Collecter* collecter(SEXP model, int n){
        switch( TYPEOF(model) ){
        case INTSXP: return new Collecter_Impl<INTSXP>(n) ;
        case REALSXP: return new Collecter_Impl<REALSXP>(n) ;
        case LGLSXP: return new Collecter_Impl<LGLSXP>(n) ;
        case STRSXP: return new Collecter_Impl<STRSXP>(n) ;
        default: break ;
        }
        return 0 ;
    }
    
}

#endif
