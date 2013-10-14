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

#ifndef dplyr_dplyr_H
#define dplyr_dplyr_H

#include <Rcpp.h>
#include <boost/functional/hash.hpp>
#include <boost/unordered_map.hpp>
#include <boost/unordered_set.hpp>

#include <tools/tools.h>

namespace dplyr {
    class Result ;
    class ResultSet ;   
    // class SplitApplyCombine ;
    class Reducer_Proxy ;
    template <typename VALUE> class DataFrameVisitorsIndexMap ;
    template <typename OUT, int INPUT_RTYPE> class Reducer ; 
}

#include <dplyr/VisitorSetEqual.h>
#include <dplyr/VisitorSetHash.h>
#include <dplyr/VisitorSetLess.h>
#include <dplyr/VisitorSetGreater.h>
#include <dplyr/GroupedDataFrame.h>
#include <dplyr/comparisons.h>
#include <dplyr/VectorVisitor.h>
#include <dplyr/OrderVisitor.h>
#include <dplyr/DataFrameVisitors.h>
#include <dplyr/DataFrameVisitorsOperators.h>
#include <dplyr/DataFrameVisitorsIndexSet.h>
#include <dplyr/DataFrameVisitorsIndexMap.h>
#include <dplyr/VectorVisitorImpl.h>
#include <dplyr/OrderVisitorImpl.h>
#include <dplyr/JoinVisitor.h>
#include <dplyr/JoinVisitorImpl.h>
#include <dplyr/DataFrameJoinVisitors.h>
#include <dplyr/Result/all.h>
#include <dplyr/Gatherer.h>
#include <dplyr/SplitApplyCombine.h>
#include <dplyr/Order.h>

SEXP and_calls(Rcpp::List args) ;
Rcpp::DataFrame subset( Rcpp::DataFrame, Rcpp::LogicalVector, Rcpp::CharacterVector ) ;

// borrowed from Rcpp11
#define RCPP_DEBUG_OBJECT(OBJ) Rf_PrintValue( Rf_eval( Rf_lang2( Rf_install( "str"), OBJ ), R_GlobalEnv ) ) ;    
#define RCPP_INSPECT_OBJECT(OBJ) Rf_PrintValue( Rf_eval( Rf_lang2( Rf_install( ".Internal"), Rf_lang2( Rf_install( "inspect" ), OBJ ) ), R_GlobalEnv ) ) ;

#endif
