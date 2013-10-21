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

#ifndef dplyr_tools_BoolResult_H
#define dplyr_tools_BoolResult_H

namespace dplyr{ 
    
    class BoolResult {
    public:
        BoolResult(bool result_) : result(result_){}
        BoolResult(bool result_, const char* msg) : result(result_), message(msg){}
        
        void set_true(){ result = true ; message.clear() ; }
        void set_false( const char* msg ){ result = false; message = msg ; }
        
        inline operator SEXP() const { 
            Shield<SEXP> res( Rf_ScalarLogical( result ) ) ;
            Rf_setAttrib( res, Rf_install("comment"), Rf_mkString(message.c_str()) );
            return res; 
        }
        
        inline operator bool() const {
            return result ;    
        }
        
    private:
        bool result ;
        std::string message ;
    } ;
    
    inline BoolResult no_because( const char* msg ){
        return BoolResult( false, msg );
    }
    
    inline BoolResult yes(){
        return true ; 
    }

}

#endif
