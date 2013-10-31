#ifndef dplyr_tools_tools_H
#define dplyr_tools_tools_H

#include <tools/hash.h>
#include <tools/delete_all.h>
#include <tools/ListOf.h>
#include <tools/collapse.h>
 
// remove these lines and the files when Rcpp 0.10.6 is released
#if !defined(Rcpp_protection_protection_H)
    #include <tools/Shelter.h>
    #include <tools/Shield.h>
    #include <tools/Armor.h>
#endif

#include <tools/ShrinkableVector.h>
#include <tools/wrap_subset.h>
#include <tools/get_all_second.h>

#endif
