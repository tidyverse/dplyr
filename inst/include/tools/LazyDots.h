#ifndef dplyr__Lazy_h
#define dplyr__Lazy_h

namespace Rcpp {
    
    class Lazy {
    public:
        Lazy( List data_, SEXP name_ ) : 
            env(data_[1]), 
            expr(data_[0]), 
            name(name_)
        {}
        
        Environment env ;
        SEXP expr ;
        SEXP name ;
    } ;
    
    template <>
    inline bool is<Lazy>(SEXP x){
        return TYPEOF(x) == VECSXP && 
               Rf_length(x) == 2 && 
               Rf_inherits(x, "lazy") &&   
               TYPEOF(VECTOR_ELT(x,1)) == ENVSXP 
               ;
    }
          
    class LazyDots {
    public:
        LazyDots( List data_ ) : data(){
            CharacterVector names = data_.names() ;
            int n = data_.size() ;
            for(int i=0; i<n; i++){
                SEXP x = data_[i] ;
                if( !is<Lazy>(x) ){
                    stop( "corrupt lazy object" );    
                }
                data.push_back( Lazy(data_[i], names[i]) ) ;
            }
        }
        
        inline const Lazy& operator[](int i) const {
            return data[i] ;    
        }
        
        inline int size() const {
            return data.size() ;    
        }
        
    private:
        std::vector<Lazy> data ; 
    } ;
    
}    
#endif
