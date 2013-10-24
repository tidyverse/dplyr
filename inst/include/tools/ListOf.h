#ifndef dplyr_tools_ListOf_H
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
        
    private:
        List data ;
    } ;
}

#endif
