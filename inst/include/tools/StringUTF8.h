#ifndef DPLYR_STRINGUTF8_H
#define DPLYR_STRINGUTF8_H

namespace dplyr {

    class StringUtf8 {
    public:
        StringUtf8( SEXP s_ ) : s(Rf_mkCharCE(Rf_translateCharUTF8(s_), CE_UTF8)) {}
        StringUtf8( CharacterVector::Proxy s_ ) : s(Rf_mkCharCE(Rf_translateCharUTF8(s_), CE_UTF8)) {}

        inline operator SEXP() const {
            return s ;
        }

    private:
        SEXP s ;
    } ;

    class CharacterVectorUtf8 {
    public:
        CharacterVectorUtf8( SEXP v ) : data(v){
            int n = data.size() ;
            // move on to the first non UTF-8 string, if any

            int i = 0 ;
            for( ; i<n; i++){
                cetype_t enc = Rf_getCharCE(data[i]) ;
                if( enc != CE_UTF8 ) break ;
            }
            if( i < n ){
                CharacterVector newdata(n) ;
                for( int j=0; j<i; j++){
                    newdata[j] = data[i] ;
                }
                for( int j=i; j<n; j++){
                    newdata[j] = StringUtf8(data[j]) ;

                }
                data = newdata ;
            }
        }

        inline CharacterVector::Proxy operator[](int i){ return data[i] ; }
        inline CharacterVector::const_Proxy operator[]( int i) const{ return data[i] ; }

        inline CharacterVector::iterator begin(){ return data.begin(); }
        inline CharacterVector::const_iterator begin() const{ return data.begin(); }

        inline CharacterVector::iterator end(){ return data.end(); }
        inline CharacterVector::const_iterator end() const{ return data.end(); }

    private:
        CharacterVector data ;
    } ;

}

#endif
