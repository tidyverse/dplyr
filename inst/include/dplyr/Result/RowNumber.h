#ifndef dplyr_Result_RowNumber_H
#define dplyr_Result_RowNumber_H

namespace dplyr {
    
    template <int RTYPE>
    class RowNumber : public Result {
    public:
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ; 
        
        typedef VectorSliceVisitor<RTYPE> Slice ;
        typedef OrderVectorVisitorImpl<RTYPE,true,Slice> Visitor ;
        typedef Compare_Single_OrderVisitor<Visitor> Comparer ;
            
        RowNumber(SEXP data_) : data(data_) {}
        
        virtual SEXP process( const GroupedDataFrame& gdf) {
            std::vector<int> tmp( gdf.max_group_size() ) ;
            
            int ng = gdf.ngroups() ; 
            int n  = gdf.nrows() ;
            GroupedDataFrame::group_iterator git = gdf.group_begin(); 
            IntegerVector out(n) ;
            for( int i=0; i<ng; i++, ++git){
                SlicingIndex index = *git ;
                
                // tmp <- 0:(m-1)
                int m = index.size() ;
                for( int i=0; i<m; i++) tmp[i] = i ;
                
                // order( gdf.group(i) )
                std::sort( tmp.begin(), tmp.begin() + m, 
                    Comparer( Visitor( Slice(data, index ) ) )     
                ) ;
                for( int i=0; i<m; i++) out[ index[i] ] = tmp[i] + 1 ;
            }
            return out ;
            
        }
        
        virtual SEXP process( const FullDataFrame& df ) {
            return process( df.get_index() ) ;
            
        }
        
        virtual SEXP process( const SlicingIndex& index ){
            int nrows = index.size() ;
            IntegerVector x = seq(0, nrows -1 ) ;
            std::sort( x.begin(), x.end(), 
                Comparer( Visitor( Slice(data, index ) ) ) 
                ) ;
            x = x + 1 ;
            return x ;
        }
        
    private:
        SEXP data ;
    } ;

}

#endif
