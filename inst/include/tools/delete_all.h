#ifndef dplyr_tools_delete_all_H
#define dplyr_tools_delete_all_H

namespace dplyr {
    
    template <typename T>
    void delete_all_second( T& value ){
        for( typename T::iterator it=value.begin(); it!=value.end(); ++it) {
            delete it->second ;
        }
        value.clear() ;
    }
    
    template <typename T>
    class pointer_vector {
    public:
        
        typedef typename std::vector<T*> Vector ;
        typedef typename Vector::reference reference ;
        typedef typename Vector::const_reference const_reference ;
        typedef typename Vector::size_type size_type ;
        typedef typename Vector::value_type value_type ;
        typedef typename Vector::iterator iterator ;
        
        pointer_vector() : data(){}
        pointer_vector(size_type n) : data(n){}
        ~pointer_vector(){
            for( iterator it=data.begin(); it!=data.end(); ++it) delete *it ;
            data.clear() ;    
        }
        
        inline reference operator[](size_type i){ 
            return data[i] ; 
        } 
        inline const_reference operator[](size_type i) const { 
            return data[i]; 
        }
        inline void push_back( const value_type& value ){
            data.push_back(value);    
        }
        inline size_type size() const {
            return data.size() ;
        }
        
    private:
        Vector data ;
        
    } ;
}

#endif
