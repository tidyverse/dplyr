#ifndef dplyr_tools_GroupedDataFrame_H
#define dplyr_tools_GroupedDataFrame_H

namespace Rcpp {
    
    class GroupedDataFrameIndexIterator {
    public:
        GroupedDataFrameIndexIterator( const IntegerVector& group_sizes_ ) : 
            group_sizes(group_sizes_), i(0), start(0) {}
        
        GroupedDataFrameIndexIterator& operator++(){
            start += group_sizes[i++] ; 
            return *this ;
        }
        
        SlicingIndex operator*() const {
            return SlicingIndex( start, group_sizes[i] ) ;     
        }
        
        int i ;
        int start ;
        const IntegerVector& group_sizes ; 
        
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
            bool is_lazy = Rf_isNull( data_.attr( "index") ) || Rf_isNull( data_.attr( "labels") ) ;
            if( is_lazy ){
                data_ = build_index_cpp( data_) ;      
            }
            group_sizes = data_.attr( "group_sizes" );
            biggest_group_size  = data_.attr( "biggest_group_size" ) ;
        }
        
        group_iterator group_begin() const {
            return GroupedDataFrameIndexIterator( group_sizes ) ;
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
        
        inline DataFrame::AttributeProxy attr( const std::string& name ) const {
            return data_.attr(name) ;    
        }
        
        inline int max_group_size() const{
            return biggest_group_size ;
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
        return Rf_inherits( x, "grouped_cpp" ) || Rf_inherits(x, "grouped_df" ) ;
    }
    
}

#endif
