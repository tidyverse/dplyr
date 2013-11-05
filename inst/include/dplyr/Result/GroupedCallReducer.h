#ifndef dplyr_GroupedCalledReducer_H
#define dplyr_GroupedCalledReducer_H

namespace dplyr {
       
    class GroupedCalledReducer : public CallbackProcessor<GroupedCalledReducer> {
    public:
        GroupedCalledReducer(Rcpp::Language call, const GroupedDataFrame& data, const Environment& env): 
            proxy(call, data, env) 
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
