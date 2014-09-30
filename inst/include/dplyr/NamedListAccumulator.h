#ifndef dplyr_NamedListAccumulator_H
#define dplyr_NamedListAccumulator_H

namespace dplyr {

    template <typename Data>
    class NamedListAccumulator {
    public:
        
        inline void set(SEXP name, SEXP x){
            if( ! Rcpp::traits::same_type<Data, RowwiseDataFrame>::value )
                check_supported_type(x, name);
            
            size_t n = size() ;
            for( size_t i = 0; i<n; i++){
                SEXP s = names[i] ;
                if( get_encoding(s) != get_encoding(name) ){
                    std::stringstream st ;
                    st << "cannot compare two strings of different encodings: "
                      << human_readable_encoding(get_encoding(s))
                      << "/" 
                      << human_readable_encoding(get_encoding(name))
                    ;
                    stop( st.str() );
                }
                if( s == name ){
                    data[i] = x ;
                    return ;
                }
            }
            
            names.push_back(name) ;
            data.push_back(x) ;
            
        }
        
        inline void rm(SEXP name){
            std::vector<SEXP>::iterator it = std::find( names.begin(), names.end(), name ) ;
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
                out_names[i] = names[i] ;
            }
            out.attr( "names" ) = out_names ;
            return out ;
        }
        
        inline size_t size() const {
            return data.size() ;    
        }
        
        inline void debug() const {
            size_t n = size() ;
            Rprintf( "accumulator.size() = %d\n", size() ) ;
            for( size_t i=0; i<n;i++){
                SEXP s = names[i] ;
                Rprintf( "names[%d] = <%p> = %s, enc = %d\n", i, s, CHAR(s), get_encoding(s) ) ;   
            }
        }
        
    private:
        std::vector<SEXP> data ;
        std::vector<SEXP> names ;
    } ;

}
#endif
