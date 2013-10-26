#ifndef dplyr_ShrinkableVector_H
#define dplyr_ShrinkableVector_H

namespace Rcpp {
    
    template <int RTYPE>
    class ShrinkableVector {
    public:
        ShrinkableVector( int n ) : data( no_init(n) ), max_size(n) {}
        
        inline void resize( int n){
            SETLENGTH( data, n ) ;    
        }
        
        inline operator SEXP() const {
            return data ;
        }
        
        ~ShrinkableVector(){
            // restore the initial length so that R can reclaim the memory
            SETLENGTH( data, max_size );   
        }
        
    private:
        Rcpp::Vector<RTYPE> data ;
        int max_size ;
    } ;
    
}

#endif
