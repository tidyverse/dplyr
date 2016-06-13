#ifndef dplyr_LazySubsets_H
#define dplyr_LazySubsets_H

namespace dplyr {

    class LazySubsets {
    public:
        SymbolMap symbol_map ;
        std::vector<SEXP> data ;
        int nr ;

        LazySubsets( const DataFrame& df) : nr(df.nrows()){
            int nvars = df.size() ;
            if( nvars ){
                CharacterVector names = df.names() ;
                for( int i=0; i<nvars; i++){
                    SEXP column = df[i] ;
                    if( Rf_inherits( column, "matrix" ) ){
                        stop( "matrix as column is not supported" ) ;
                    }
                    symbol_map.insert( names[i] ) ;
                    data.push_back( df[i] ) ;
                }
            }
        }
        virtual ~LazySubsets(){}

        virtual SEXP get_variable(SEXP symbol) const {
            return data[ symbol_map.get(symbol) ] ;
        }
        virtual bool is_summary( SEXP symbol ) const {
            return false ;
        }
        virtual int count(SEXP symbol) const{
            int res = symbol_map.has(symbol);
            return res ;
        }

        virtual void input( SEXP symbol, SEXP x){
            SymbolMapIndex index = symbol_map.insert(symbol) ;
            if( index.origin == NEW ){
                data.push_back(x) ;
            } else {
                data[index.pos] = x ;
            }
        }

        virtual int size() const{
            return data.size() ;
        }

        inline int nrows() const {
            return nr ;
        }

        inline SEXP& operator[](SEXP symbol){
            return data[symbol_map.get(symbol)] ;
        }
    } ;

}

#endif
