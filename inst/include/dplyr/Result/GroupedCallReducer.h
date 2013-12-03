#ifndef dplyr_GroupedCalledReducer_H
#define dplyr_GroupedCalledReducer_H

namespace dplyr {
       
    class GroupedCalledReducer : public CallbackProcessor<GroupedCalledReducer> {
    public:
        GroupedCalledReducer(Rcpp::Language call, const LazyGroupedSubsets& subsets, const Environment& env): 
            proxy(call, subsets, env) 
        {
        }
        
        virtual ~GroupedCalledReducer(){} ;
        
        inline SEXP process_chunk( const SlicingIndex& indices ){
            return proxy.get(indices) ;
        }               
        
    private:
        GroupedCallProxy proxy ;
    } ;

} // namespace dplyr

#endif
