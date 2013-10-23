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

#ifndef dplyr_VisitorSetIndexMap_H
#define dplyr_VisitorSetIndexMap_H

namespace dplyr{
                  
    template <typename VisitorSet, typename VALUE>
    class VisitorSetIndexMap : 
        public boost::unordered_map<int, VALUE, VisitorSetHasher<VisitorSet> , VisitorSetEqualPredicate<VisitorSet> > {
    private:
        typedef VisitorSetHasher<VisitorSet> Hasher ;
        typedef VisitorSetEqualPredicate<VisitorSet> EqualPredicate ;
        typedef typename boost::unordered_map<int, VALUE, Hasher, EqualPredicate> Base ;
        
    public:
        VisitorSetIndexMap() : Base(), visitors(0) {}
                   
        VisitorSetIndexMap( VisitorSet& visitors_ ) : 
            Base( 1024, Hasher(&visitors_), EqualPredicate(&visitors_) ), 
            visitors(&visitors_)
        {}
        
        VisitorSetIndexMap( VisitorSet* visitors_ ) : 
            Base( 1024, Hasher(visitors_), EqualPredicate(visitors_) ), 
            visitors(visitors_)
        {}
        
        VisitorSet* visitors ;
    
    } ;
    
    template <typename Map>
    inline void train_push_back( Map& map, int n){
        for( int i=0; i<n; i++) map[i].push_back(i) ;
    }
    
    template <typename Set>
    inline void train_insert( Set& set, int n){
        for( int i=0; i<n; i++) set.insert(i) ;
    }
    template <typename Set>
    inline void train_insert_right( Set& set, int n){
        for( int i=0; i<n; i++) set.insert(-i-1) ;
    }

}

#endif
