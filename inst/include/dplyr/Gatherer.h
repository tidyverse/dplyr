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

#ifndef dplyr_Gatherer_H
#define dplyr_Gatherer_H

namespace dplyr {
    
class Gatherer {
public:
    virtual ~Gatherer(){}
    virtual SEXP collect() = 0 ;
} ;
    
template <int RTYPE>
class GathererImpl : public Gatherer {
public:
    typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;
    
    GathererImpl( Rcpp::Shield<SEXP>& first, Index_1_based& indices, CallProxy& proxy_, const Rcpp::GroupedDataFrame& gdf_ ) : 
        gdf(gdf_), proxy(proxy_), data(Rcpp::no_init(gdf.nrows()))
    {
        grab( first, indices ) ;
    }
        
    SEXP collect(){
        int ngroups = gdf.ngroups() ;
        Rcpp::Armor<SEXP> subset ;
        for( int i=1; i<ngroups; i++){
            Index_1_based indices = gdf.group(i) ;
            subset = proxy.get( indices ) ;
            grab( subset, indices );
        }
        
        return data ;
    }
private: 
    
    void grab( SEXP subset, const Index_1_based& indices ){
        int n = indices.size();
        STORAGE* ptr = Rcpp::internal::r_vector_start<RTYPE>( subset ) ;
        for( int j=0; j<n; j++){
            data[ indices[j] ] = ptr[j] ;
        }
    }
    
    const Rcpp::GroupedDataFrame& gdf ;
    CallProxy& proxy ;
    Rcpp::Vector<RTYPE> data ;
} ;


Gatherer* gatherer( CallProxy& proxy, const Rcpp::GroupedDataFrame& gdf ) ;

} // namespace dplyr


#endif
