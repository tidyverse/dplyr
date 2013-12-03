#ifndef dplyr_NamedListAccumulator_H
#define dplyr_NamedListAccumulator_H

namespace dplyr {

    template <typename T>
    class NamedListAccumulator {
    public:
        
        inline void set(SEXP name, const T& x){
            std::vector<SEXP>::iterator it = std::find( names.begin(), names.end(), name ) ;
            if(it == names.end() ){
                names.push_back(name) ;
                data.push_back(x) ;
            } else {
                data[ std::distance(names.begin(), it ) ] = x ;
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
        
    private:
        std::vector<T> data ;
        std::vector<SEXP> names ;
    } ;

}
#endif
