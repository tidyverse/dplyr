#ifndef dplyr_tools_GroupedDataFrame_H
#define dplyr_tools_GroupedDataFrame_H

namespace Rcpp {

    inline void check_valid_colnames( const DataFrame& df){
        if( df.size() ){
            CharacterVector names(df.names()) ;
            LogicalVector dup = duplicated(names) ;
            if( any(dup).is_true() ){
                std::stringstream s ;
                s << "found duplicated column name: " ;
                bool first = true ;
                for( int i=0; i<df.size(); i++){
                    if( dup[i] == TRUE ){
                        if( first ){
                          first = false ;
                        } else {
                          s << ", " ;
                        }
                        s << names[i] ;
                    }
                }
                stop(s.str()) ;
            }
        }
    }

    class GroupedDataFrame ;

    class GroupedDataFrameIndexIterator {
    public:
        GroupedDataFrameIndexIterator( const GroupedDataFrame& gdf_ ) ;

        GroupedDataFrameIndexIterator& operator++() ;

        SlicingIndex operator*() const  ;

        int i ;
        const GroupedDataFrame& gdf ;
        List indices ;
    } ;

    class GroupedDataFrame {
    public:
        typedef GroupedDataFrameIndexIterator group_iterator ;
        GroupedDataFrame( SEXP x):
            data_(x),
            group_sizes(),
            biggest_group_size(0),
            symbols( data_.attr("vars") ),
            labels()
        {
            // handle lazyness
            bool is_lazy = Rf_isNull( data_.attr( "group_sizes") ) || Rf_isNull( data_.attr( "labels") ) ;

            if( is_lazy ){
                data_ = build_index_cpp( data_) ;
            }
            group_sizes = data_.attr( "group_sizes" );
            biggest_group_size  = data_.attr( "biggest_group_size" ) ;
            labels = data_.attr( "labels" );

            if( !is_lazy ){
                // check consistency of the groups
                int rows_in_groups = sum(group_sizes) ;
                if( data_.nrows() != rows_in_groups ){
                    stop( "corrupt 'grouped_df', contains %d rows, and %s rows in groups", data_.nrows(), rows_in_groups );
                }
            }
        }

        group_iterator group_begin() const {
            return GroupedDataFrameIndexIterator( *this ) ;
        }

        SEXP symbol( int i) const {
            return symbols[i] ;
        }

        DataFrame& data() {
            return data_ ;
        }
        const DataFrame& data() const {
            return data_ ;
        }

        inline int ngroups() const {
            return group_sizes.size() ;
        }

        inline int nvars() const {
            return labels.size() ;
        }

        inline int nrows() const {
            return data_.nrows() ;
        }

        inline SEXP label(int i) const {
            return labels[i];
        }

        inline int max_group_size() const{
            return biggest_group_size ;
        }

        inline bool has_group(SEXP g) const {
            SEXP symb = Rf_installChar(g) ;
            int n = symbols.size() ;
            for( int i=0; i<n; i++){
              if( symbols[i] == symb ) return true ;
            }
            return false ;
        }

        inline const IntegerVector& get_group_sizes() const {
            return group_sizes ;
        }

    private:

        DataFrame data_ ;
        IntegerVector group_sizes ;
        int biggest_group_size ;
        ListOf<Symbol> symbols ;
        DataFrame labels ;

    } ;

    template <>
    inline bool is<GroupedDataFrame>( SEXP x){
        return Rf_inherits(x, "grouped_df" ) && Rf_getAttrib(x, Rf_install("vars") ) != R_NilValue ;
    }

    inline GroupedDataFrameIndexIterator::GroupedDataFrameIndexIterator( const GroupedDataFrame& gdf_ ) :
        i(0), gdf(gdf_), indices(gdf.data().attr("indices")) {}

    inline GroupedDataFrameIndexIterator& GroupedDataFrameIndexIterator::operator++(){
        i++;
        return *this ;
    }

    inline SlicingIndex GroupedDataFrameIndexIterator::operator*() const {
        return SlicingIndex( IntegerVector(indices[i]), i ) ;
    }


}

#endif
