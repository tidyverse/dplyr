#ifndef dplyr_DataFrameJoinVisitors_H
#define dplyr_DataFrameJoinVisitors_H

namespace dplyr{
    
    class DataFrameJoinVisitors : 
        public VisitorSetEqual<DataFrameJoinVisitors>, 
        public VisitorSetHash<DataFrameJoinVisitors>
    {
    public:
        typedef JoinVisitor visitor_type ;
        
        DataFrameJoinVisitors(const Rcpp::DataFrame& left_, const Rcpp::DataFrame& right_, Rcpp::CharacterVector names_left, Rcpp::CharacterVector names_right ) : 
            left(left_), right(right_), 
            visitor_names_left(names_left), 
            visitor_names_right(names_right), 
            nvisitors(names_left.size()), 
            visitors(nvisitors)
        {    
            std::string name_left, name_right ;
            for( int i=0; i<nvisitors; i++){
                name_left  = names_left[i] ;
                name_right = names_right[i] ;
                try{
                    visitors[i] = join_visitor( left[name_left], right[name_right], name_left, name_right ) ;
                } catch( const std::exception& ex ){
                    std::stringstream s ;
                    s << "cannot join on columns '" << name_left << "' x '" << name_right << "' : " << ex.what() ;
                    stop(s.str());    
                } catch( ... ){
                    std::stringstream s ;
                    s << "cannot join on columns '" << name_left << "' x '" << name_right << "'" ;
                    stop(s.str());
                }
            }
        }
        
        ~DataFrameJoinVisitors(){
            delete_all(visitors);    
        }
        
        inline JoinVisitor* get(int k) const { 
            return visitors[k] ; 
        }
        inline int size() const{ 
            return nvisitors ; 
        } 
        
        template <typename Container>
        inline DataFrame subset( const Container& index, const CharacterVector& classes ){
            int nrows = index.size() ;
            Rcpp::List out(nvisitors);
            for( int k=0; k<nvisitors; k++){
               out[k] = get(k)->subset(index) ;    
            }
            out.attr( "class" ) = classes ;
            set_rownames(out, nrows) ;
            out.names() = visitor_names_left ; 
            SEXP vars = left.attr( "vars" ) ;
            if( !Rf_isNull(vars) )
                out.attr( "vars" ) = vars ;
            return (SEXP)out ;
        }
        
        inline void print(int i){
            for( int k=0; k<nvisitors; k++)
                visitors[k]->print(i) ;
        }
        
    private:
        const DataFrame& left ;
        const DataFrame& right ;
        CharacterVector visitor_names_left ;
        CharacterVector visitor_names_right ;
        
        int nvisitors ;
        std::vector<JoinVisitor*> visitors ;
        
    } ;
    
}

#endif

