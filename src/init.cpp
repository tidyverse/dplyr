#define COMPILING_DPLYR
#include <Rcpp.h>
#include <dplyr.h>

using namespace Rcpp ;
using namespace dplyr ;

#define DPLYR_REGISTER(__FUN__) R_RegisterCCallable( "dplyr", #__FUN__ "__impl", (DL_FUNC)__FUN__ ## __impl );
    
extern "C" void R_init_dplyr( DllInfo* info ){
    // will register functions here through the R_RegisterCCallable mechanism
}

