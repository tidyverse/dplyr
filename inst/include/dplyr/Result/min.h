#ifndef dplyr_Result_Min_H
#define dplyr_Result_Min_H

namespace dplyr {

    template <int RTYPE, bool NA_RM>
    class Min : public Processor<RTYPE, Min<RTYPE,NA_RM> > {
    public:
        typedef Processor<RTYPE, Min<RTYPE,NA_RM> > Base ;
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;

        Min(SEXP x, bool is_summary_ = false) :
            Base(x),
            data_ptr( Rcpp::internal::r_vector_start<RTYPE>(x) ),
            is_summary(is_summary_)
        {}
        ~Min(){}

        STORAGE process_chunk( const SlicingIndex& indices ){
            if( indices.size() == 0) return R_PosInf ;
            if( is_summary ) return data_ptr[ indices.group() ] ;

            int n = indices.size() ;
            // find the first non NA value
            STORAGE res = data_ptr[ indices[0] ] ;
            int i=1 ;
            while( i<n && Rcpp::Vector<RTYPE>::is_na(res) ){
                res = data_ptr[ indices[i++] ] ;
            }

            // we enter this loop if we did not scan the full vector
            if( i < n ) for( ; i<n; i++){
                STORAGE current = data_ptr[indices[i]] ;
                if( !Rcpp::Vector<RTYPE>::is_na(current) && internal::is_smaller<RTYPE>( current, res ) ) res = current ;
            }

            return res ;
        }

    private:
        STORAGE* data_ptr ;
        bool is_summary ;
    } ;

    // quit early version for NA_RM = false
    template <int RTYPE>
    class Min<RTYPE,false> : public Processor<RTYPE, Min<RTYPE,false> > {
    public:
        typedef Processor<RTYPE, Min<RTYPE,false> > Base ;
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;

        Min(SEXP x, bool is_summary_ = false) :
            Base(x),
            data_ptr( Rcpp::internal::r_vector_start<RTYPE>(x) ),
            is_summary(is_summary_)
        {}
        ~Min(){}

        STORAGE process_chunk( const SlicingIndex& indices ){
            if( indices.size() == 0) return R_PosInf ;
            if( is_summary ) return data_ptr[ indices.group() ] ;

            int n = indices.size() ;

            // find the first non NA value
            STORAGE res = data_ptr[ indices[0] ] ;
            if( Rcpp::Vector<RTYPE>::is_na(res) ) return res;

            for( int i=1; i<n; i++){
                STORAGE current = data_ptr[indices[i]] ;
                if( Rcpp::Vector<RTYPE>::is_na(current) ) return current ;
                if( internal::is_smaller<RTYPE>( current, res ) ) res = current ;
            }
            return res ;
        }

    private:
        STORAGE* data_ptr ;
        bool is_summary ;
    } ;


}

#endif
