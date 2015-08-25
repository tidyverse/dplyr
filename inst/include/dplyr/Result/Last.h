#ifndef dplyr_Result_Last_H
#define dplyr_Result_Last_H

namespace dplyr {
           
    template <int RTYPE>
    class Last : public Processor< RTYPE, Last<RTYPE> > {
    public:
        typedef Processor< RTYPE, Last<RTYPE> > Base ;
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ; 
        
        Last( Vector<RTYPE> data_, STORAGE def_ = Vector<RTYPE>::get_na() ) : 
            Base(data_), 
            data(data_), 
            def(def_) {}
        
        inline STORAGE process_chunk( const SlicingIndex& indices ){
            int n = indices.size() ;
            return n == 0 ? def : data[ indices[n-1] ] ;
        }
        
    private:
        Vector<RTYPE> data ;
        STORAGE def ;
    } ;

    template <int RTYPE, int ORDER_RTYPE>
    class LastWith : public Processor< RTYPE, LastWith<RTYPE, ORDER_RTYPE> > {
    public:
        typedef Processor< RTYPE, LastWith<RTYPE, ORDER_RTYPE> > Base ;
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ; 
        
        LastWith( Vector<RTYPE> data_, Vector<ORDER_RTYPE> order_, STORAGE def_ = Vector<RTYPE>::get_na() ) : 
            Base(data_), 
            data(data_),
            order(order_),
            def(def_) {}
        
        inline STORAGE process_chunk( const SlicingIndex& indices ){
            int n = indices.size() ;
            
            if( indices.size() == 0 ) return def ;
            
            typedef VectorSliceVisitor<ORDER_RTYPE> Slice ;
            typedef OrderVectorVisitorImpl<ORDER_RTYPE,false,Slice> Visitor ;
            typedef Compare_Single_OrderVisitor<Visitor> Comparer ;
        
            int idx = 0 ;
            
            Comparer comparer( Visitor( Slice(order, indices ) ) ) ;
            for( int i = 1; i<n; i++){
                if( comparer(i, idx) ) idx = i ;
            }
            return data[ indices[idx] ] ;
        }
        
    private:
        Vector<RTYPE> data ;
        Vector<ORDER_RTYPE> order ;
        STORAGE def ;
    } ;

}

#endif
