#ifndef dplyr_NamedListAccumulator_H
#define dplyr_NamedListAccumulator_H

namespace dplyr {

    template <typename Data>
    class NamedListAccumulator {
    public:
        SymbolMap symbol_map ;
        std::vector<SEXP> data ;

        NamedListAccumulator(){}

        inline void set(SEXP name, SEXP x){
            if( ! Rcpp::traits::same_type<Data, RowwiseDataFrame>::value )
                check_supported_type(x, name);

            SymbolMapIndex index = symbol_map.insert(name) ;
            if( index.origin == NEW ){
                data.push_back(x);
            } else {
                data[ index.pos ] = x ;
            }

        }

        inline void rm(SEXP name){
            SymbolMapIndex index = symbol_map.rm(name) ;
            if( index.origin != NEW ){
                data.erase( data.begin() + index.pos ) ;
            }
        }

        inline operator List() const {
            List out = wrap(data) ;
            out.names() = symbol_map.names ;
            return out ;
        }

        inline size_t size() const {
            return data.size() ;
        }

        inline CharacterVector names() const {
            return symbol_map.names ;
        }

    } ;

}
#endif
