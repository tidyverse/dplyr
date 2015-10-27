#ifndef dplyr_Result_Nth_H
#define dplyr_Result_Nth_H

namespace dplyr {
           
    template <int RTYPE>
    class Nth : public Processor< RTYPE, Nth<RTYPE> > {
    public:
        typedef Processor< RTYPE, Nth<RTYPE> >  Base ;
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ; 
        
        Nth( Vector<RTYPE> data_, int idx_, STORAGE def_ = Vector<RTYPE>::get_na() ) : 
            Base(data_), 
            data(data_), 
            idx(idx_),
            def(def_) {}
        
        inline STORAGE process_chunk( const SlicingIndex& indices ){
            int n = indices.size() ;
            if( n == 0 || idx > n || idx <= 0) return def ;
            return data[ indices[idx-1] ] ;
        }
        
    private:
        Vector<RTYPE> data ;  
        int idx ;
        STORAGE def ;
    } ;
    
    template <int RTYPE, int ORDER_RTYPE>
    class NthWith : public Processor< RTYPE, NthWith<RTYPE, ORDER_RTYPE> > {
    public:
        typedef Processor< RTYPE, NthWith<RTYPE, ORDER_RTYPE> > Base ;
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ; 
        
        NthWith( Vector<RTYPE> data_, int idx_, Vector<ORDER_RTYPE> order_, STORAGE def_ = Vector<RTYPE>::get_na() ) : 
            Base(data_), 
            data(data_),
            idx(idx_),
            order(order_),
            def(def_) {}
        
        inline STORAGE process_chunk( const SlicingIndex& indices ){
            int n = indices.size() ;
            if( n == 0 || idx > n || idx <= 0) return def ;
            
            typedef VectorSliceVisitor<ORDER_RTYPE> Slice ;
            typedef OrderVectorVisitorImpl<ORDER_RTYPE,true,Slice> Visitor ;
            typedef Compare_Single_OrderVisitor<Visitor> Comparer ;
        
            Comparer comparer( Visitor( Slice(order, indices ) ) ) ;
            IntegerVector sequence = seq(0,n-1) ;
            std::nth_element( sequence.begin(), sequence.begin() + idx - 1, sequence.end(), comparer ) ;
            
            return data[ indices[ sequence[idx-1] ] ] ;
        }
        
    private:
        Vector<RTYPE> data ;
        int idx ;
        Vector<ORDER_RTYPE> order ; 
        STORAGE def ;
    } ;

}

#endif
