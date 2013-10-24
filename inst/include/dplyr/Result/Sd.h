#ifndef dplyr_Result_Sd_H
#define dplyr_Result_Sd_H

namespace dplyr {

    template <int RTYPE, bool NA_RM>
    class Sd : public Processor<REALSXP, Sd<RTYPE,NA_RM> > {
    public:
        
        Sd(SEXP x) : var(x) {}
        ~Sd(){}
        
        inline double process_chunk( const Index_0_based& indices ){
            return sqrt( var.process_chunk( indices ) );
        }
         
    private:
        Var<RTYPE,NA_RM> var ;
    } ;
 
}

#endif
