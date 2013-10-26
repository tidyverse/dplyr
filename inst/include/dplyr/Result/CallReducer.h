#ifndef dplyr_CallReducer_H
#define dplyr_CallReducer_H

namespace dplyr {
       
    class CallReducer : public CallbackProcessor<CallReducer> {
    public:
        CallReducer(Rcpp::Language call_, const Rcpp::DataFrame& data_, const Environment& env): 
            call(call_), data(data_), call_proxy(call, data, env) {}
        
        virtual ~CallReducer(){} ;
        
        inline SEXP process_chunk( const SlicingIndex& indices ){
            return call_proxy.get(indices) ;
        }               
        
    private:
        
        Rcpp::Language call ;
        const Rcpp::DataFrame& data ;
        
        CallProxy call_proxy ;
    } ;

} // namespace dplyr

#endif
