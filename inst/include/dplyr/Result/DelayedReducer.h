#ifndef dplyr_DelayedReducer_H
#define dplyr_DelayedReducer_H

namespace dplyr {

    template <int INPUT_RTYPE>
    class DelayedReducer : public CallbackProcessor< DelayedReducer<INPUT_RTYPE> > {
        public:
            DelayedReducer(Rcpp::Function fun_, Rcpp::String variable_, SEXP data_ ):
                call(fun_), proxy(call, 1), data(data_) {}

            virtual ~DelayedReducer(){} ;

            inline SEXP process_chunk( const SlicingIndex& indices){
                proxy = wrap_subset<INPUT_RTYPE>( data, indices );
                return call.eval() ;
            }

        private:

            Rcpp::Language call ;
            Rcpp::Language::Proxy proxy ;
            SEXP data ;
    } ;

} // namespace dplyr

#endif
