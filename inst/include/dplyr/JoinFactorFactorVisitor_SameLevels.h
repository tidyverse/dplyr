#ifndef dplyr_JoinFactorFactorVisitor_SameLevels_H
#define dplyr_JoinFactorFactorVisitor_SameLevels_H

namespace dplyr{
       
    inline bool same_levels( SEXP left, SEXP right ){
        SEXP s_levels = Rf_install("levels") ;
        CharacterVector levels_left  = Rf_getAttrib(left,s_levels) ;
        CharacterVector levels_right = Rf_getAttrib(right,s_levels) ;
        if( (SEXP)levels_left == (SEXP)levels_right ) return true ; 
        int n = levels_left.size() ;
        if( n != levels_right.size() ) return false ;
        
        for( int i=0; i<n; i++) {
            if( levels_right[i] != levels_left[i] ) return false ;
        }
        
        return true ;
    }
    
    class JoinFactorFactorVisitor_SameLevels : public JoinVisitorImpl<INTSXP, INTSXP> {
    public:
        typedef JoinVisitorImpl<INTSXP,INTSXP> Parent ;
        
        JoinFactorFactorVisitor_SameLevels( const IntegerVector& left, const IntegerVector& right ) : 
            Parent(left, right), 
            levels(left.attr("levels")),
            levels_ptr( Rcpp::internal::r_vector_start<STRSXP>( levels ) )
            {}
        
        inline SEXP subset( const VisitorSetIndexSet<DataFrameJoinVisitors>& set ){
            Vec res = JoinVisitorImpl<INTSXP, INTSXP>::subset(set) ;
            res.attr( "class" )  = Parent::left.attr("class" ) ;
            res.attr( "levels" ) = levels ;
            
            return res ;
        }
        
        inline SEXP subset( const std::vector<int>& indices ){
            Vec res = JoinVisitorImpl<INTSXP, INTSXP>::subset(indices) ;
            res.attr( "class" )  = Parent::left.attr("class" ) ;
            res.attr( "levels" ) = levels ;
            
            return res ;
        }
        
        inline void debug(){
            Rprintf( "visitor= JoinFactorFactorVisitor_SameLevels. levels=" ) ;
            Rf_PrintValue(levels) ;
        }
            
    private:     
        CharacterVector levels ;
        SEXP* levels_ptr ;
        
    } ;
    
}

#endif
