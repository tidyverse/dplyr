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
            Rf_setAttrib( res, Rf_install("class"), Rf_mkString("BoolResult") );
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
