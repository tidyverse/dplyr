#ifndef dplyr_tools_encoding_H
#define dplyr_tools_encoding_H

namespace dplyr {

CharacterVector reencode_factor(IntegerVector x);
CharacterVector reencode_char(SEXP x);

}

#endif // #ifndef dplyr_tools_encoding_H
