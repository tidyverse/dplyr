#ifndef dplyr_Result_Count_H
#define dplyr_Result_Count_H

namespace dplyr {
    
    class Count : public Processor<INTSXP,Count> {
    public:
        Count() {}
        ~Count(){}
        
        inline int process_chunk( const Index_0_based& indices ){ 
            return indices.size() ;    
        }
    } ;

}

#endif
