#ifndef dplyr_CallProxy_H
#define dplyr_CallProxy_H

namespace dplyr {

    class CallProxy {
    public:
        CallProxy( const Rcpp::Call& call_, LazySubsets& subsets_, const Environment& env_) :
            call(call_), subsets(subsets_), proxies(), env(env_)
        {
            // fill proxies
            set_call(call);
        }

        CallProxy( const Rcpp::Call& call_, const Rcpp::DataFrame& data_, const Environment& env_) :
            call(call_), subsets(data_), proxies(), env(env_)
        {
            // fill proxies
            set_call(call);
        }

        CallProxy( const Rcpp::DataFrame& data_, const Environment& env_ ) :
            subsets(data_), proxies(), env(env_){
        }

        CallProxy( const Rcpp::DataFrame& data_) :
            subsets(data_), proxies() {
        }

        ~CallProxy(){}

        SEXP eval() ;

        void set_call( SEXP call_ ) ;

        void input( Rcpp::String name, SEXP x ){
            subsets.input( name.get_sexp(), x ) ;
        }

        inline int nsubsets(){
            return subsets.size() ;
        }

        inline SEXP get_variable( Rcpp::String name ) const {
            return subsets.get_variable( Symbol(name) );
        }

        inline bool has_variable(SEXP symbol){
            return subsets.count(symbol) ;
        }

        inline void set_env(SEXP env_){
            env = env_ ;
        }

    private:

        bool simplified(const SlicingIndex& indices) ;
        bool replace( SEXP p, const SlicingIndex& indices ) ;
        void traverse_call( SEXP obj ) ;

        Rcpp::Call call ;
        LazySubsets subsets ;
        std::vector<CallElementProxy> proxies ;
        Environment env;

    } ;

}

#endif
