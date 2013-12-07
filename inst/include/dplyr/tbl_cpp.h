#ifndef dplyr_tools_tbl_cpp_H
#define dplyr_tools_tbl_cpp_H

namespace dplyr {
    
    template <typename Df>
    inline void set_rownames( Df& data, int n ){
        data.attr( "row.names" ) = Rcpp::IntegerVector::create( 
            Rcpp::IntegerVector::get_na(), -n) ;   
    }
    
    inline Rcpp::CharacterVector classes_grouped(){
        return Rcpp::CharacterVector::create( "grouped_df", "tbl_df", "tbl", "data.frame") ;        
    }
    inline Rcpp::CharacterVector classes_not_grouped(){
        return Rcpp::CharacterVector::create( "tbl_df", "tbl", "data.frame") ;
    }

    class tbl_cpp{
    public:
        tbl_cpp( Rcpp::List data_, int nr ) : data(data_){
            set_rownames(data, nr ) ;
            data.attr( "class" ) = classes_not_grouped()  ;
        }
        
        inline operator SEXP(){ 
            return data ; 
        }
        
    private:
        List data ;
    } ;
    
    class summarised_grouped_tbl_cpp{
    public:
        summarised_grouped_tbl_cpp( Rcpp::List data_, const GroupedDataFrame& source ) : data(data_){
            int nr = source.ngroups() ;
            set_rownames(data, nr ) ;
            
            if( source.nvars() > 1){
                data.attr( "class" ) = classes_grouped()  ;
                List vars = source.data().attr("vars") ;
                vars.erase( source.nvars() - 1) ;
                data.attr( "vars") = vars ;
                data.attr( "drop" ) = true ;
            } else {
                data.attr( "class" ) = classes_not_grouped()  ;
                data.attr( "drop" ) = true ;
            }
        }
        
        inline operator SEXP(){ 
            return data ; 
        }
        
    private:
        
        List data ;
    } ;
    
}

#endif
