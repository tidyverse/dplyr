#ifndef DPLYR_ENCODING_H
#define DPLYR_ENCODING_H

#define TYPE_BITS 5
#define BYTES_MASK (1<<1)
#define LATIN1_MASK (1<<2)
#define UTF8_MASK (1<<3)

// that bit seems unused by R. Just using it to mark
// objects as Shrinkable Vectors
// that is useful for things like summarise(list(x)) where x is a
// variable from the data, because the SEXP that goes into the list
// is the shrinkable vector, we use this information to duplicate
// it if needed. See the maybe_copy method in DelayedProcessor
#define DPLYR_SHRINKABLE_MASK (1<<8)

struct sxpinfo_struct {
    SEXPTYPE type      :  TYPE_BITS;/* ==> (FUNSXP == 99) %% 2^5 == 3 == CLOSXP
			     * -> warning: `type' is narrower than values
			     *              of its type
			     * when SEXPTYPE was an enum */
    unsigned int obj   :  1;
    unsigned int named :  2;
    unsigned int gp    : 16;
    unsigned int mark  :  1;
    unsigned int debug :  1;
    unsigned int trace :  1;  /* functions and memory tracing */
    unsigned int spare :  1;  /* currently unused */
    unsigned int gcgen :  1;  /* old generation number */
    unsigned int gccls :  3;  /* node class */
}; /*		    Tot: 32 */

#ifndef IS_BYTES
# define IS_BYTES(x) (reinterpret_cast<sxpinfo_struct*>(x)->gp & BYTES_MASK)
#endif

#ifndef IS_LATIN1
# define IS_LATIN1(x) (reinterpret_cast<sxpinfo_struct*>(x)->gp & LATIN1_MASK)
#endif

#ifndef IS_ASCII
# define IS_ASCII(x) (reinterpret_cast<sxpinfo_struct*>(x)->gp & ASCII_MASK)
#endif

#ifndef IS_UTF8
# define IS_UTF8(x) (reinterpret_cast<sxpinfo_struct*>(x)->gp & UTF8_MASK)
#endif

namespace dplyr{

    enum encoding {
       BYTES, LATIN1, UTF8, UNKNOWN
    } ;

    inline encoding get_encoding( SEXP s){
        if( IS_BYTES(s) ) return BYTES ;
        if( IS_LATIN1(s) ) return LATIN1 ;
        if( IS_UTF8(s) ) return UTF8 ;
        return UNKNOWN ;
    }

    inline const char* human_readable_encoding( encoding e ){
        switch(e){
        case BYTES: return "bytes" ;
        case LATIN1: return "latin1" ;
        case UTF8: return "UTF-8" ;
        default: break ;
        }
        return "unknown" ;
    }

}

#endif
