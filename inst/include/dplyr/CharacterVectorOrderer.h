#ifndef dplyr_CharacterVectorOrderer_H
#define dplyr_CharacterVectorOrderer_H

namespace dplyr {

    class CharacterVectorOrderer {
    public:

        CharacterVectorOrderer( const CharacterVector& data_ ) ;

        inline IntegerVector get() const {
          return orders ;
        }

    private:
        CharacterVector data ;
        dplyr_hash_set<SEXP> set ;
        IntegerVector orders ;
    } ;

}

#endif
