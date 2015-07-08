#ifndef dplyr_Result_First_H
#define dplyr_Result_First_H

namespace dplyr {
           
    template <int RTYPE>
    class First : public Processor< RTYPE, First<RTYPE> > {
    public:
        typedef Processor< RTYPE, First<RTYPE> > Base ;
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ; 
        
        First( Vector<RTYPE> data_, STORAGE def_ = Vector<RTYPE>::get_na() ) : 
            Base(data_),
            data(data_), 
            def(def_) 
            {}
        
        inline STORAGE process_chunk( const SlicingIndex& indices ){
            return indices.size() == 0 ? def : data[ indices[0] ] ;
        }
        
    private:
        Vector<RTYPE> data ;
        STORAGE def ;
    } ;

    template <int RTYPE, int ORDER_RTYPE>
    class FirstWith : public Processor< RTYPE, FirstWith<RTYPE, ORDER_RTYPE> > {
    public:
        typedef Processor< RTYPE, FirstWith<RTYPE, ORDER_RTYPE> > Base ;
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ; 
        
        FirstWith( Vector<RTYPE> data_, Vector<ORDER_RTYPE> order_, STORAGE def_ = Vector<RTYPE>::get_na() ) : 
            Base(data_), 
            data(data_),
            order(order_),
            def(def_) {}
        
        inline STORAGE process_chunk( const SlicingIndex& indices ){
            if( indices.size() == 0 ) return def ;
            
            int n = indices.size() ;
            typedef VectorSliceVisitor<ORDER_RTYPE> Slice ;
            typedef OrderVectorVisitorImpl<ORDER_RTYPE,true,Slice> Visitor ;
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
