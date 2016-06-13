#ifndef dplyr_get_single_class_h
#define dplyr_get_single_class_h

namespace dplyr {

    inline std::string get_single_class(SEXP x){
        SEXP klass = Rf_getAttrib(x, R_ClassSymbol) ;
        if( !Rf_isNull(klass) ){
            CharacterVector classes(klass) ;
            return collapse<STRSXP>(classes) ;
        }

        if(Rf_isMatrix(x)){
            return "matrix" ;
        }

        switch( TYPEOF(x) ){
        case INTSXP: return "integer" ;
        case REALSXP : return "numeric" ;
        case LGLSXP: return "logical" ;
        case STRSXP: return "character" ;

        case VECSXP: return "list" ;
        default: break ;
        }

        // just call R to deal with other cases
        // we could call R_data_class directly but we might get a "this is not part of the api"
        klass = Rf_eval( Rf_lang2( Rf_install( "class" ), x), R_GlobalEnv ) ;
        return CHAR(STRING_ELT(klass,0)) ;
    }

}

#endif
