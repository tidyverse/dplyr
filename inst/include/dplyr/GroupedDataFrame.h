// Copyright (C) 2013    Romain Francois
// Copyright (C) 2013    Rice University
//
// This file is part of dplyr.
//
// dplyr is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// dplyr is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with dplyr.  If not, see <http://www.gnu.org/licenses/>.

#ifndef dplyr_tools_GroupedDataFrame_H
#define dplyr_tools_GroupedDataFrame_H

namespace Rcpp {
    
    class GroupedDataFrame {
    public:
        GroupedDataFrame( SEXP x): 
            data_(x), 
            indices( data_.attr("index")),
            symbols(),
            labels( data_.attr("labels"))
        {
            if( !data_.hasAttribute( "index" ) )
                stop( "cannot find the 'index' attribute" ) ;
            List symb = data_.attr("vars") ;
            int n = symb.size() ;
            symbols.resize(n);
            for( int i=0; i<n; i++){
                symbols[i] = symb[i] ;    
            }
        }
        
        Index_1_based group( int i ) const {
            return indices[i] ;     
        }
                         
        SEXP symbol( int i) const {
            return symbols[i] ;    
        }
        
        bool needs_separate_evaluation(SEXP expr){
            switch( TYPEOF( expr ) ){
            case SYMSXP:
                {
                    if( any( symbols.begin(), symbols.end(), expr ) ) return true ;
                    break ;
                }
            case LANGSXP: 
                {
                    SEXP p = CDR(expr) ;
                    while( p != R_NilValue ){
                        if( needs_separate_evaluation( CAR(p) ) ) return true ;
                        p = CDR(p) ;
                    }
                    break ;
                }
            default:
                break ;
            }
            return false ;
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
        
    private:
        
        DataFrame data_ ;
        ListOf<Index_1_based> indices ;
        std::vector<SEXP> symbols ;
        DataFrame labels ;
    } ;
    
    template <>
    inline bool is<GroupedDataFrame>( SEXP x){
        return Rf_inherits( x, "grouped_df" ) ;
    }
    
    
    
}

#endif
