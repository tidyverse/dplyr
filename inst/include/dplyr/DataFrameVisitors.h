#ifndef dplyr_DataFrameVisitors_H
#define dplyr_DataFrameVisitors_H

namespace dplyr {

    class DataFrameVisitors :
        public VisitorSetEqual<DataFrameVisitors>,
        public VisitorSetHash<DataFrameVisitors>,
        public VisitorSetLess<DataFrameVisitors>,
        public VisitorSetGreater<DataFrameVisitors> {

        private:

            const Rcpp::DataFrame& data ;
            pointer_vector<VectorVisitor> visitors ;
            Rcpp::CharacterVector visitor_names ;
            int nvisitors ;

        public:
            typedef VectorVisitor visitor_type ;

            DataFrameVisitors( const Rcpp::DataFrame& data_) ;

            DataFrameVisitors( const Rcpp::DataFrame& data_, const Rcpp::CharacterVector& names )  ;

            inline int size() const { return nvisitors ; }
            inline VectorVisitor* get(int k) const { return visitors[k] ; }

            Rcpp::String name(int k) const { return visitor_names[k] ; }

            inline int nrows() const { return data.nrows() ; }

        private:

            void structure( List& x, int nrows, CharacterVector classes ) const  ;

    } ;

} // namespace dplyr


#endif
