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

#ifndef dplyr_Result_Sd_H
#define dplyr_Result_Sd_H

namespace dplyr {

    template <int RTYPE, bool NA_RM>
    class Sd : public Processor<REALSXP, Sd<RTYPE,NA_RM> > {
    public:
        
        Sd(SEXP x) : var(x) {}
        ~Sd(){}
        
        inline double process_chunk( const Index_1_based& indices ){
            return sqrt( var.process_chunk( indices ) );
        }
         
    private:
        Var<RTYPE,NA_RM> var ;
    } ;
 
}

#endif
