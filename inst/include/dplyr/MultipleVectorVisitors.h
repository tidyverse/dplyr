#ifndef dplyr_MultipleVectorVisitors_H
#define dplyr_MultipleVectorVisitors_H

namespace dplyr {

    class MultipleVectorVisitors :
        public VisitorSetEqual<MultipleVectorVisitors>,
        public VisitorSetHash<MultipleVectorVisitors>,
        public VisitorSetLess<MultipleVectorVisitors>,
        public VisitorSetGreater<MultipleVectorVisitors> {

        private:

            List data ;
            pointer_vector<VectorVisitor> visitors ;
            Rcpp::CharacterVector visitor_names ;
            int nvisitors ;

        public:
            typedef VectorVisitor visitor_type ;

            MultipleVectorVisitors( List data_) :
                data(data_),
                visitors(),
                nvisitors(data.size())
            {
                for( int i=0; i<nvisitors; i++){
                    VectorVisitor* v = visitor( data[i] ) ;
                    visitors.push_back(v) ;
                }
            }

            inline int size() const { return nvisitors ; }
            inline VectorVisitor* get(int k) const { return visitors[k] ; }
            inline int nrows() const { return visitors[0]->size() ;}
    } ;

} // namespace dplyr


#endif
