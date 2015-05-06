#ifndef dplyr_VectorVisitor_H
#define dplyr_VectorVisitor_H

namespace dplyr {
    
    /** 
     * Vector visitor base class, defines the interface
     */
    class VectorVisitor {
    public:
        virtual ~VectorVisitor(){}
        
        /** hash the element of the visited vector at index i */
        virtual size_t hash(int i) const = 0 ;
        
        /** are the elements at indices i and j equal */
        virtual bool equal(int i, int j) const = 0 ;
        
        /** is the i element less than the j element */
        virtual bool less( int i, int j) const = 0 ;
        
        /** is the i element less than the j element */
        virtual bool greater( int i, int j) const = 0 ;
        
        /** creates a new vector, of the same type as the visited vector, by 
         *  copying elements at the given indices
         */
        virtual SEXP subset( const Rcpp::IntegerVector& index ) const = 0 ;
        
        virtual SEXP subset( const std::vector<int>& ) const = 0 ;
        
        /** creates a new vector, of the same type as the visited vector, by 
         *  copying elements at the given indices
         */
        virtual SEXP subset( const ChunkIndexMap& index ) const = 0 ;
        
        virtual SEXP subset( const Rcpp::LogicalVector& index ) const = 0 ;
        
        virtual SEXP subset( EmptySubset ) const = 0 ;
        
        virtual int size() const = 0 ;
        
        virtual std::string get_r_type() const = 0 ;
        
        virtual bool is_compatible( VectorVisitor* other, std::stringstream&, const std::string& ) const {
            return true ;    
        }
    } ;
    
    // defined in visitor.h
    inline VectorVisitor* visitor( SEXP ) ;   
    
} // namespace dplyr


#endif
