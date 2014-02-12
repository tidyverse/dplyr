#ifndef dplyr_tools_type_name_H
#define dplyr_tools_type_name_H

inline const char* type_name(SEXP x){
  return type2name(x) ;
}

#endif
