#ifndef dplyr__Lazy_h
#define dplyr__Lazy_h

namespace Rcpp {

    class Lazy {
    public:
        Lazy( List data_, SEXP name__ ) :
            data(data_),
            name_(name__)
        {}

        Lazy( const Lazy& other ) :
            data(other.data),
            name_(other.name_)
        {}

        inline SEXP expr() const {
            return Rf_duplicate(data[0]) ;
        }
        inline SEXP env() const {
            return data[1];
        }
        inline SEXP name() const {
            return name_ ;
        }

    private:

        List data ;
        SEXP name_ ;
    } ;

    template <>
    inline bool is<Lazy>(SEXP x){
        return TYPEOF(x) == VECSXP &&
               Rf_length(x) == 2 &&
               Rf_inherits(x, "lazy") &&
               TYPEOF(VECTOR_ELT(x,1)) == ENVSXP
               ;
    }

    class LazyDots {
    public:
        LazyDots( List data_ ) : data(){
            int n = data_.size() ;
            if (n == 0) return;

            CharacterVector names = data_.names() ;
            for(int i=0; i<n; i++){
                List x = data_[i] ;
                if( !is<Lazy>(x) ){
                    stop( "corrupt lazy object" );
                }
                data.push_back(Lazy(x, names[i])) ;
            }
        }

        inline const Lazy& operator[](int i) const {
            return data[i] ;
        }

        inline int size() const {
            return data.size() ;
        }

        inline bool single_env() const {
            if( data.size() <= 1 ) return true ;
            SEXP env = data[0].env() ;
            for( size_t i=1; i<data.size(); i++){
                if( data[i].env() != env ) return false ;
            }
            return true ;
        }

    private:
        std::vector<Lazy> data ;
    } ;

}
#endif
