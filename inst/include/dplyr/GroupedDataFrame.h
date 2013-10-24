#ifndef dplyr_tools_GroupedDataFrame_H
#define dplyr_tools_GroupedDataFrame_H

namespace Rcpp {
    
    class GroupedDataFrame {
    public:
        GroupedDataFrame( SEXP x): 
            data_(x),
            indices(), 
            symbols( data_.attr("vars") ),
            labels()
        {
            // handle lazyness
            bool is_lazy = Rf_isNull( data_.attr( "index") ) || Rf_isNull( data_.attr( "labels") ) ;
            if( is_lazy ){
                data_ = build_index_cpp( data_) ;      
            }
            indices = data_.attr( "index" );
            labels  = data_.attr( "labels") ;
            
        }
        
        Index_0_based group( int i ) const {
            return indices[i] ;     
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
            return indices.size() ;    
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
        
    private:
        
        DataFrame data_ ;
        ListOf<Index_0_based> indices ;
        ListOf<Symbol> symbols ;
        DataFrame labels ;
    } ;
    
    template <>
    inline bool is<GroupedDataFrame>( SEXP x){
        return Rf_inherits( x, "grouped_cpp" ) || Rf_inherits(x, "grouped_df" ) ;
    }
    
}

#endif
