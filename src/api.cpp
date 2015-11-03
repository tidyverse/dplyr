#include <dplyr.h>

namespace dplyr{

    Symbol get_column(SEXP arg, const Environment& env, const LazySubsets& subsets ){
      RObject value ;
      if( TYPEOF(arg) == LANGSXP && CAR(arg) == Rf_install("~") ){
        if( Rf_length(arg) != 2 || TYPEOF(CADR(arg)) != SYMSXP )
          stop( "unhandled formula in column" ) ;
        value = CharacterVector::create( PRINTNAME(CADR(arg)) ) ;
      } else {
        value = Rcpp_eval(arg, env) ;
      }
      if( is<Symbol>(value) ){
        value = CharacterVector::create(PRINTNAME(value)) ;
      }
      if( !is<String>(value) ){
        stop("column must return a single string") ;
      }
      Symbol res(STRING_ELT(value,0)) ;
      if( !subsets.count(res) ){
        stop("result of column() expands to a symbol that is not a variable from the data: %s", CHAR(PRINTNAME(res)) ) ;
      }
      return res ;
    }

    void CallProxy::set_call( SEXP call_ ){
        proxies.clear() ;
        call = call_ ;
        if( TYPEOF(call) == LANGSXP ) traverse_call(call) ;
    }

    SEXP CallProxy::eval(){
        if( TYPEOF(call) == LANGSXP ){

            if( can_simplify(call) ){
                SlicingIndex indices(0,subsets.nrows()) ;
                while(simplified(indices)) ;
                set_call(call) ;
            }

            int n = proxies.size() ;
            for( int i=0; i<n; i++){
                proxies[i].set( subsets[proxies[i].symbol] ) ;
            }
            return call.eval(env) ;
        } else if( TYPEOF(call) == SYMSXP) {
            // SYMSXP
            if( subsets.count(call) ) return subsets.get_variable(call) ;
            return call.eval(env) ;
        }
        return call ;
    }

    bool CallProxy::simplified(const SlicingIndex& indices){
        // initial
        if( TYPEOF(call) == LANGSXP ){
            boost::scoped_ptr<Result> res( get_handler(call, subsets, env) );

            if( res ){
                // replace the call by the result of process
                call = res->process(indices) ;

                // no need to go any further, we simplified the top level
                return true ;
            }

            return replace( CDR(call), indices ) ;

        }
        return false ;
    }

    bool CallProxy::replace( SEXP p, const SlicingIndex& indices ){

        SEXP obj = CAR(p) ;

        if( TYPEOF(obj) == LANGSXP ){
            boost::scoped_ptr<Result> res( get_handler(obj, subsets, env) );
            if(res){
                SETCAR(p, res->process(indices) ) ;
                return true ;
            }

            if( replace( CDR(obj), indices ) ) return true ;
        }

        if( TYPEOF(p) == LISTSXP ){
            return replace( CDR(p), indices ) ;
        }

        return false ;
    }

    void CallProxy::traverse_call( SEXP obj ){

        if( TYPEOF(obj) == LANGSXP && CAR(obj) == Rf_install("local") ) return ;

        if( TYPEOF(obj) == LANGSXP && CAR(obj) == Rf_install("global") ){
          SEXP symb = CADR(obj) ;
          if( TYPEOF(symb) != SYMSXP ) stop( "global only handles symbols" ) ;
          SEXP res = env.find(CHAR(PRINTNAME(symb))) ;
          call = res ;
          return ;
        }

        if( TYPEOF(obj) == LANGSXP && CAR(obj) == Rf_install("column") ){
          call = get_column(CADR(obj), env, subsets) ;
          return ;
        }

        if( ! Rf_isNull(obj) ){
            SEXP head = CAR(obj) ;
            switch( TYPEOF( head ) ){
            case LANGSXP:
                if( CAR(head) == Rf_install("global") ){
                    SEXP symb = CADR(head) ;
                    if( TYPEOF(symb) != SYMSXP ) stop( "global only handles symbols" ) ;
                    SEXP res  = env.find( CHAR(PRINTNAME(symb)) ) ;

                    SETCAR(obj, res) ;
                    SET_TYPEOF(obj, LISTSXP) ;

                    break ;
                }
                if( CAR(head) == Rf_install("column")){
                  Symbol column = get_column( CADR(head), env, subsets) ;
                  SETCAR(obj, column ) ;
                  head = CAR(obj) ;
                  proxies.push_back( CallElementProxy( head, obj ) );

                  break ;
                }
                if( CAR(head) == Rf_install("~")) return ;
                if( CAR(head) == Rf_install("order_by") ) break ;
                if( CAR(head) == Rf_install("function") ) break ;
                if( CAR(head) == Rf_install("local") ) return ;
                if( CAR(head) == Rf_install("<-") ){
                    stop( "assignments are forbidden" ) ;
                }
                if( Rf_length(head) == 3 ){
                    SEXP symb = CAR(head) ;
                    if( symb == R_DollarSymbol || symb == Rf_install("@") || symb == Rf_install("::") || symb == Rf_install(":::") ){

                        // Rprintf( "CADR(obj) = " ) ;
                        // Rf_PrintValue( CADR(obj) ) ;

                        // for things like : foo( bar = bling )$bla
                        // so that `foo( bar = bling )` gets processed
                        if( TYPEOF(CADR(head)) == LANGSXP ){
                            traverse_call( CDR(head) ) ;
                        }

                        // deal with foo$bar( bla = boom )
                        if( TYPEOF(CADDR(head)) == LANGSXP ){
                            traverse_call( CDDR(head) ) ;
                        }

                        break ;
                    } else {
                      traverse_call( CDR(head) ) ;
                    }
                } else {
                    traverse_call( CDR(head) ) ;
                }

                break ;
            case LISTSXP:
                traverse_call( head ) ;
                traverse_call( CDR(head) ) ;
                break ;
            case SYMSXP:
                if( TYPEOF(obj) != LANGSXP ){
                    if( ! subsets.count(head) ){
                        if( head == R_MissingArg ) break ;
                        if( head == Rf_install(".") ) break ;

                        // in the Environment -> resolve
                        try{
                            Shield<SEXP> x( env.find( CHAR(PRINTNAME(head)) ) ) ;
                            SETCAR( obj, x );
                        } catch( ...){
                            // what happens when not found in environment
                        }

                    } else {
                        // in the data frame
                        proxies.push_back( CallElementProxy( head, obj ) );
                    }
                    break ;
                }
            }
            traverse_call( CDR(obj) ) ;
        }
    }

    CharacterVectorOrderer::CharacterVectorOrderer( const CharacterVector& data_ ) :
        data(data_),
        set(),
        orders(no_init(data.size()))
    {
        int n = data.size() ;
        if( n == 0 ) return ;

        // 1 - gather unique SEXP pointers from data
        SEXP* p_data = Rcpp::internal::r_vector_start<STRSXP>(data);
        SEXP previous = *p_data++ ;
        set.insert( previous ) ;
        for( int i=1; i<n; i++, p_data++){
            SEXP s = *p_data ;

            // we've just seen this string, keep going
            if( s == previous ) continue ;

            // is this string in the set already
            set.insert(s) ;
            previous = s ;
        }

        // retrieve unique strings from the set
        int n_uniques = set.size() ;
        CharacterVector uniques( set.begin(), set.end() ) ;
        CharacterVector s_uniques = Language( "sort", uniques ).fast_eval() ;

        // order the uniques with a callback to R
        IntegerVector o = Language( "match", uniques, s_uniques ).fast_eval() ;

        // combine uniques and o into a hash map for fast retrieval
        dplyr_hash_map<SEXP,int> map ;
        for( int i=0; i<n_uniques; i++){
            map.insert( std::make_pair(uniques[i], o[i] ) ) ;
        }

        // grab min ranks
        p_data = Rcpp::internal::r_vector_start<STRSXP>(data);
        previous = *p_data++ ;

        int o_pos ;
        orders[0] = o_pos = map.find(previous)->second ;

        for( int i=1; i<n; i++, p_data++){
            SEXP s = *p_data;
            if( s == previous ) {
                orders[i] = o_pos ;
                continue ;
            }
            previous = s ;
            orders[i] = o_pos = map.find(s)->second ;
        }

    }

    CharacterVector get_uniques( const CharacterVector& left, const CharacterVector& right){
        int nleft = left.size(), nright = right.size() ;
        int n = nleft + nright ;

        CharacterVector big = no_init(n) ;
        CharacterVector::iterator it = big.begin() ;
        std::copy( left.begin(), left.end(), it ) ;
        std::copy( right.begin(), right.end(), it + nleft ) ;
        return Language( "unique", big ).fast_eval() ;
    }

    IntegerVector match( const CharacterVector& s, const CharacterVector& levels){
      return Language( "match", s, levels).fast_eval() ;
    }

}



// [[Rcpp::export]]
IntegerVector rank_strings( CharacterVector s ){
  return dplyr::CharacterVectorOrderer(s).get() ;
}
