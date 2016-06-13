#ifndef dplyr_SubsetVectorVisitor_H
#define dplyr_SubsetVectorVisitor_H

namespace dplyr {

    /**
     * Subset Vector visitor base class, defines the interface
     */
    class SubsetVectorVisitor {
    public:
        virtual ~SubsetVectorVisitor(){}

        /** creates a new vector, of the same type as the visited vector, by
         *  copying elements at the given indices
         */
        virtual SEXP subset( const Rcpp::IntegerVector& index ) const = 0 ;

        virtual SEXP subset( const std::vector<int>& ) const = 0 ;

        virtual SEXP subset( const SlicingIndex& ) const = 0 ;

        /** creates a new vector, of the same type as the visited vector, by
         *  copying elements at the given indices
         */
        virtual SEXP subset( const ChunkIndexMap& index ) const = 0 ;

        virtual SEXP subset( const Rcpp::LogicalVector& index ) const = 0 ;

        virtual SEXP subset( EmptySubset ) const = 0 ;

        virtual int size() const = 0 ;

        virtual std::string get_r_type() const = 0 ;

        virtual bool is_compatible( SubsetVectorVisitor* other, std::stringstream&, const std::string& ) const  = 0 ;

    } ;

    // defined in visitor.h
    inline SubsetVectorVisitor* subset_visitor( SEXP ) ;

} // namespace dplyr


#endif
