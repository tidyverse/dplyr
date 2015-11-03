#ifndef dplyr_DataFrameSubsetVisitors_H
#define dplyr_DataFrameSubsetVisitors_H

namespace dplyr {

    class DataFrameSubsetVisitors {
        private:

            const Rcpp::DataFrame& data ;
            pointer_vector<SubsetVectorVisitor> visitors ;
            Rcpp::CharacterVector visitor_names ;
            int nvisitors ;

        public:
            typedef SubsetVectorVisitor visitor_type ;

            DataFrameSubsetVisitors( const Rcpp::DataFrame& data_) :
                data(data_),
                visitors(),
                visitor_names(data.names()),
                nvisitors(visitor_names.size())
            {

                for( int i=0; i<nvisitors; i++){
                    SubsetVectorVisitor* v = subset_visitor( data[i] ) ;
                    visitors.push_back(v) ;
                }
            }

            DataFrameSubsetVisitors( const Rcpp::DataFrame& data_, const Rcpp::CharacterVector& names ) :
                data(data_),
                visitors(),
                visitor_names(names),
                nvisitors(visitor_names.size())
            {

                std::string name ;
                int n = names.size() ;
                for( int i=0; i<n; i++){
                    name = (String)names[i] ;
                    SEXP column ;

                    try{
                        column = data[name] ;
                    } catch( ... ){
                        stop( "unknown column '%s' ", name ) ;
                    }
                    SubsetVectorVisitor* v = subset_visitor( column ) ;
                    visitors.push_back(v) ;

                }

            }

            template <typename Container>
            DataFrame subset_impl( const Container& index, const CharacterVector& classes, traits::false_type ) const {
                List out(nvisitors);
                for( int k=0; k<nvisitors; k++){
                    out[k] = get(k)->subset(index) ;
                }
                copy_most_attributes( out, data ) ;
                structure( out, Rf_length(out[0]) , classes) ;
                return out ;
            }

            template <typename Container>
            DataFrame subset_impl( const Container& index, const CharacterVector& classes, traits::true_type ) const {
                int n = index.size() ;
                int n_out = std::count( index.begin(), index.end(), TRUE ) ;
                IntegerVector idx = no_init(n_out) ;
                for(int i=0, k=0; i<n; i++){
                    if( index[i] == TRUE ){
                        idx[k++] = i ;
                    }
                }
                return subset_impl( idx, classes, traits::false_type() ) ;
            }

            template <typename Container>
            inline DataFrame subset( const Container& index, const CharacterVector& classes ) const {
                return subset_impl( index, classes,
                    typename traits::same_type<Container, LogicalVector>::type()
                ) ;
            }

            inline int size() const { return nvisitors ; }
            inline SubsetVectorVisitor* get(int k) const { return visitors[k] ; }

            Rcpp::String name(int k) const { return visitor_names[k] ; }

            inline int nrows() const { return data.nrows() ; }

        private:

            inline void structure( List& x, int nrows, CharacterVector classes ) const {
                x.attr( "class" ) = classes ;
                set_rownames(x, nrows) ;
                x.names() = visitor_names ;
                SEXP vars = data.attr( "vars" ) ;
                if( !Rf_isNull(vars) )
                    x.attr( "vars" ) = vars ;
            }

    } ;

    inline DataFrame subset( DataFrame data, LogicalVector test, CharacterVector select, CharacterVector classes ){
        DataFrameSubsetVisitors visitors( data, select ) ;
        return visitors.subset(test, classes ) ;
    }

    inline DataFrame subset( DataFrame data, LogicalVector test, CharacterVector classes ){
        DataFrameSubsetVisitors visitors( data ) ;
        DataFrame res = visitors.subset(test, classes ) ;
        return res ;
    }


} // namespace dplyr


#endif
