#if !defined(dplyr_tools_ListOf_H) && !defined(Rcpp_vector_ListOf_h_)
#define dplyr_tools_ListOf_H

namespace Rcpp {
    
    template <typename T>
    class ListOf {
    public: 
        
        ListOf(){}
        
        template <typename U>
        ListOf& operator=( const U& data_ ){
            data = as<List>(data_) ;
            return *this ;
        }
        
        template <typename U>
        ListOf( const U& data_ ) : data( as<List>(data_) ){}
        
        T operator[](int i) const { 
            return as<T>( data[i]) ;
        }
        
        int size() const { 
            return data.size() ; 
        }
        
        inline bool has(const T& u) const {
            int n=data.size();
            for( int i=0; i<n; i++){
                if( as<T>(data[i]) == u ) return true ;    
            }
            return false ;
        }
        
        inline operator SEXP() const {
            return data ;   
        }
        
    private:
        List data ;
    } ;
}

#endif
