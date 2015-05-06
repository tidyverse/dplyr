#ifndef dplyr_NamedListAccumulator_H
#define dplyr_NamedListAccumulator_H

namespace dplyr {

    template <typename Data>
    class NamedListAccumulator {
    public:
        
        inline void set(Name name, SEXP x){
            if( ! Rcpp::traits::same_type<Data, RowwiseDataFrame>::value )
                check_supported_type(x, name);
                    
            size_t n = size() ;
            for( size_t i = 0; i<n; i++){
                const Name& s = names[i] ;
                if( s == name ){
                    data[i] = x ;
                    return ;
                }
            }
            
            names.push_back(name) ;
            data.push_back(x) ;
            
        }
        
        inline void rm(Name name){
            std::vector<Name>::iterator it = std::find( names.begin(), names.end(), name ) ;
            if( it != names.end() ){
                names.erase(it) ;
                data.erase( data.begin() + std::distance( names.begin(), it ) );
            }
        }
        
        inline operator List() const {
            int n = data.size() ;
            List out(n) ;
            CharacterVector out_names(n) ;
            for( int i=0; i<n; i++){
                out[i] = data[i] ;
                out_names[i] = names[i].get() ;
            }
            out.attr( "names" ) = out_names ;
            return out ;
        }
        
        inline size_t size() const {
            return data.size() ;    
        }
        
    private:
        std::vector<SEXP> data ;
        std::vector<Name> names ;
    } ;

}
#endif
