#ifndef dplyr_MultipleVectorVisitors_H
#define dplyr_MultipleVectorVisitors_H

namespace dplyr {

    class MultipleVectorVisitors :
        public VisitorSetEqual<MultipleVectorVisitors>,
        public VisitorSetHash<MultipleVectorVisitors>,
        public VisitorSetLess<MultipleVectorVisitors>,
        public VisitorSetGreater<MultipleVectorVisitors> {

        private:
            std::vector< boost::shared_ptr<VectorVisitor> > visitors ;

        public:
            typedef VectorVisitor visitor_type ;

            MultipleVectorVisitors( List data_) :
                visitors()
            {
                for( int i=0; i<nvisitors; i++){
                    VectorVisitor* v = visitor( data[i] ) ;
                    visitors.push_back( boost::shared_ptr<VectorVisitor>(v) ) ;
                }
            }

            inline int size() const {
              return visitors.size() ;
            }
            inline VectorVisitor* get(int k) const {
              return visitors[k].get() ;
            }
            inline int nrows() const {
              return visitors[0]->size() ;
            }

            inline bool is_na(int index) const {
              int n = size() ;
              for( int i=0; i<n; i++) if( visitors[i]->is_na(i)) return true ;
              return false ;
            }

    } ;

} // namespace dplyr


#endif
