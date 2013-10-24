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
    virtual size_t hash(int i) = 0 ;
    
    /** are the elements at indices i and j equal */
    virtual bool equal(int i, int j) = 0 ;
    
    /** is the i element less than the j element */
    virtual bool less( int i, int j) = 0 ;
    
    /** is the i element less than the j element */
    virtual bool greater( int i, int j) = 0 ;
    
    /** creates a new vector, of the same type as the visited vector, by 
     *  copying elements at the given indices
     */
    virtual SEXP subset( const Rcpp::IntegerVector& index ) = 0 ;
    
    virtual SEXP subset( const std::vector<int>& ) = 0 ;
    
    /** creates a new vector, of the same type as the visited vector, by 
     *  copying elements at the given indices
     */
    virtual SEXP subset( const ChunkIndexMap& index ) = 0 ;
    
    virtual SEXP subset( const Rcpp::LogicalVector& index ) = 0 ;
    
} ;

/**
 * dispatch the SEXP, will call the appropriate instantiation of make_visitor
 */
VectorVisitor* visitor( SEXP vec ) ;

} // namespace dplyr


#endif
