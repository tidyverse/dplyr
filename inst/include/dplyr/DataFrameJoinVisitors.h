#ifndef dplyr_DataFrameJoinVisitors_H
#define dplyr_DataFrameJoinVisitors_H

namespace dplyr{
    
    class DataFrameJoinVisitors : 
        public VisitorSetEqual<DataFrameJoinVisitors>, 
        public VisitorSetHash<DataFrameJoinVisitors>
    {
    public:
        typedef JoinVisitor visitor_type ;
        
        DataFrameJoinVisitors(const Rcpp::DataFrame& left_, const Rcpp::DataFrame& right_, Rcpp::CharacterVector names_) : 
            left(left_), right(right_), visitor_names(names_), nvisitors(names_.size()), visitors(nvisitors)
        {    
            std::string name ;
            for( int i=0; i<nvisitors; i++){
                name = names_[i] ;
                visitors[i] = join_visitor( left[name], right[name]) ;
            }
        }
        
        ~DataFrameJoinVisitors(){
            delete_all(visitors);    
        }
        
        inline JoinVisitor* get(int k) const { return visitors[k] ; }
        inline int size() const{ return nvisitors ; } 
        
        template <typename Container>
        inline DataFrame subset( const Container& index, const CharacterVector& classes ){
            int nrows = index.size() ;
            Rcpp::List out(nvisitors);
            for( int k=0; k<nvisitors; k++){
               out[k] = get(k)->subset(index) ;    
            }
            out.attr( "class" ) = classes ;
            set_rownames(out, nrows) ;
            out.names() = visitor_names ;
            SEXP vars = left.attr( "vars" ) ;
            if( !Rf_isNull(vars) )
                out.attr( "vars" ) = vars ;
            return out.asSexp() ;
        }
        
        inline void print(int i){
            for( int k=0; k<nvisitors; k++)
                visitors[k]->print(i) ;
        }
        
    private:
        const DataFrame& left ;
        const DataFrame& right ;
        CharacterVector visitor_names ;
        int nvisitors ;
        std::vector<JoinVisitor*> visitors ;
        
    } ;
    
}

#endif

