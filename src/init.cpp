#define COMPILING_DPLYR
#include <Rcpp.h>
#include <dplyr.h>

using namespace Rcpp ;
using namespace dplyr ;

#define DPLYR_REGISTER(__FUN__) R_RegisterCCallable( "dplyr", #__FUN__, (DL_FUNC)__FUN__ );

extern "C" void R_init_dplyr( DllInfo* info ){
    // will register functions here through the R_RegisterCCallable mechanism
    
    DPLYR_REGISTER(build_index_cpp)
    DPLYR_REGISTER(registerHybridHandler)
    
}

