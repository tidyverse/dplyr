#ifndef dplyr_CharacterVectorDifferentiator_H
#define dplyr_CharacterVectorDifferentiator_H

namespace dplyr {

    class CharacterVectorDifferentiator {
    public:

        CharacterVectorDifferentiator( const CharacterVector& data_) ;

        inline IntegerVector get() const {
          return orders ;
        }

    private:
        CharacterVector data ;
        IntegerVector orders ;
    } ;

}

#endif
